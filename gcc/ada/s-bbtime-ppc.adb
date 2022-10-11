------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . T I M E                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2014, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Interrupts;
with System.BB.Board_Support;
with System.BB.Protection;
with System.BB.Threads.Queues;
with System.BB.Timing_Events;
with System.BB.CPU_Primitives;
with System.BB.CPU_Specific;
with System.Machine_Code;       use System.Machine_Code;

package body System.BB.Time is

   use Board_Support;
   use System.Multiprocessors;

   --  We use two timers with the same frequency:
   --     A Periodic Timer for the clock
   --     An Alarm Timer for delays

   -----------------------
   -- Local Definitions --
   -----------------------

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   --  Values of this type represent number of times that the clock finishes
   --  its countdown. This type should allow atomic reads and updates.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the alarm interrupt

   procedure Set_DEC (Ticks : Unsigned_32);
   pragma Inline (Set_DEC);
   --  Set the decrementer register

   function Read_TBL return Unsigned_32;
   pragma Inline (Read_TBL);
   --  Read the Time Base Lower word

   function Read_TBU return Unsigned_32;
   pragma Inline (Read_TBU);
   --  Read the Time Base Upper word

   -------------------
   -- Alarm_Handler --
   -------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is
      pragma Unreferenced (Interrupt);
      Now : constant Time := Clock;

   begin
      Board_Support.Clear_Alarm_Interrupt;

      --  A context switch may happen due to an awaken task. Charge the
      --  current task.

      if Scheduling_Event_Hook /= null then
         Scheduling_Event_Hook.all;
      end if;

      --  Note that the code is executed with interruptions disabled, so there
      --  is no need to call Enter_Kernel/Leave_Kernel.

      --  Execute expired events of the current CPU

      Timing_Events.Execute_Expired_Timing_Events (Now);

      --  Wake up our alarms

      Threads.Queues.Wakeup_Expired_Alarms (Now);

      --  Set the timer for the next alarm on this CPU

      Update_Alarm (Get_Next_Timeout (CPU'First));

      --  The interrupt low-level handler will call context_switch if necessary

   end Alarm_Handler;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      Lo : Unsigned_32;
      Hi, Hi1 : Unsigned_32;

   begin
      --  We can't atomically read the 64-bits counter.  So check that the
      --  32 MSB don't change.

      Hi := Read_TBU;
      loop
         Lo := Read_TBL;
         Hi1 := Read_TBU;
         exit when Hi = Hi1;
         Hi := Hi1;
      end loop;

      return (Time (Hi) * 2 ** 32) + Time (Lo);
   end Clock;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Now               : Time;
      Self              : Threads.Thread_Id;
      Inserted_As_First : Boolean;

   begin
      --  First mask interrupts. This is necessary to handle thread queues.

      Protection.Enter_Kernel;

      --  Read the clock once the interrupts are masked to avoid being
      --  interrupted before the alarm is set.

      Now := Clock;

      Self := Threads.Thread_Self;

      --  Test if the alarm time is in the future

      if T > Now then

         --  Extract the thread from the ready queue. When a thread wants to
         --  wait for an alarm it becomes blocked.

         Self.State := Threads.Delayed;

         Threads.Queues.Extract (Self);

         --  Insert Thread_Id in the alarm queue (ordered by time) and if it
         --  was inserted at head then check if Alarm Time is closer than the
         --  next clock interrupt.

         Threads.Queues.Insert_Alarm (T, Self, Inserted_As_First);

         if Inserted_As_First then
            Update_Alarm (Get_Next_Timeout (CPU'First));
         end if;

      else
         --  If alarm time is not in the future, the thread must yield the CPU

         Threads.Queues.Yield (Self);
      end if;

      Protection.Leave_Kernel;
   end Delay_Until;

   ----------------------
   -- Get_Next_Timeout --
   ----------------------

   function Get_Next_Timeout (CPU_Id : CPU) return Time is
      Alarm_Time : constant Time :=
                     Threads.Queues.Get_Next_Alarm_Time (CPU_Id);
      Event_Time : constant Time := Timing_Events.Get_Next_Timeout (CPU_Id);

   begin
      if Alarm_Time <= Event_Time then
         return Alarm_Time;
      else
         return Event_Time;
      end if;
   end Get_Next_Timeout;

   -----------------------
   -- Initialize_Timers --
   -----------------------

   procedure Initialize_Timers is
   begin
      --  Install alarm handler

      CPU_Specific.Install_Exception_Handler
        (Alarm_Handler'Address,
         CPU_Specific.Decrementer_Excp);
   end Initialize_Timers;

   --------------
   -- Read_TBL --
   --------------

   function Read_TBL return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mftbl %0",
        Outputs => Unsigned_32'Asm_Output ("=r", Res),
        Volatile => True);
      return Res;
   end Read_TBL;

   --------------
   -- Read_TBU --
   --------------

   function Read_TBU return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mftbu %0",
        Outputs => Unsigned_32'Asm_Output ("=r", Res),
        Volatile => True);
      return Res;
   end Read_TBU;

   -------------
   -- Set_DEC --
   -------------

   procedure Set_DEC (Ticks : Unsigned_32) is
   begin
      Asm ("mtdec %0",
        Inputs => Unsigned_32'Asm_Input ("r", Ticks),
        Volatile => True);
   end Set_DEC;

   ------------------
   -- Update_Alarm --
   ------------------

   procedure Update_Alarm (Alarm : Time) is
      Max_Timer_Interval : constant Unsigned_32 := 16#7FFF_FFFF#;
      --  The maximum value that can be set in the DEC register. MSB must not
      --  be set to avoid a useless interrupt (PowerPC triggers an interrupt
      --  when the MSB switches from 0 to 1).

      Now : constant Time := Clock;

      Diff : constant Time := (if Alarm > Now then Alarm - Now else 1);
      --  If alarm is in the past (it may happen because we are getting again
      --  the clock value here), set the minimum timer value so the interrupt
      --  will be triggered as soon as possible. Note that we cannot get the
      --  difference first and then check whether the result is negative
      --  because type Time is modular.  On e500, we must set 1 to trigger
      --  an exception.

      Dec  : Unsigned_32;

   begin

      --  Check whether the alarm time is within the DEC period

      if Diff <= Time (Max_Timer_Interval) then
         Dec := Unsigned_32 (Diff);
      else
         Dec := Max_Timer_Interval;
      end if;

      Set_DEC (Dec);
   end Update_Alarm;

end System.BB.Time;
