------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

--  This package implements PowerPC architecture specific support for the GNAT
--  Ravenscar run time.

with System.Machine_Code; use System.Machine_Code;
with System.BB.Interrupts;
with System.BB.Threads.Queues;
with System.BB.Protection;
with System.BB.Board_Support;

package body System.BB.CPU_Primitives is

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   type MSR_Type is mod 2 ** 32;
   for MSR_Type'Size use 32;

   MSR_EE : constant MSR_Type := 2 ** 15;

   function Get_MSR return MSR_Type;
   pragma Inline (Get_MSR);
   --  Read the MSR

   procedure Set_MSR (MSR : MSR_Type);
   pragma Inline (Set_MSR);
   --  Write the MSR

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
      procedure Context_Switch_Asm;
      pragma Import (Asm, Context_Switch_Asm, "context_switch");
   begin
      --  Called with interrupts disabled

      --  Set interrupt priority. Unlike the SPARC implementation, the
      --  interrupt priority is not part of the context (not in a register).

      Board_Support.Set_Current_Priority
        (Threads.Queues.First_Thread.Active_Priority);

      Context_Switch_Asm;
   end Context_Switch;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      Set_MSR (Get_MSR and not MSR_EE);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts (Level : System.Any_Priority) is
   begin
      if Level /= System.Interrupt_Priority'Last then
         Board_Support.Set_Current_Priority (Level);

         --  Really enable interrupts

         Set_MSR (Get_MSR or MSR_EE);
      end if;
   end Enable_Interrupts;

   -------------
   -- Get_MSR --
   -------------

   function Get_MSR return MSR_Type is
      Res : MSR_Type;
   begin
      Asm ("mfmsr %0",
           Outputs => MSR_Type'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_MSR;

   ----------------------
   -- Initialize_Stack --
   ----------------------

   procedure Initialize_Stack
     (Base          : Address;
      Size          : Storage_Elements.Storage_Offset;
      Stack_Pointer : out Address)
   is
      use System.Storage_Elements;

      Minimum_Stack_Size_In_Bytes : constant Integer_Address :=
                                      CPU_Specific.Stack_Alignment;

      Initial_SP : constant System.Address :=
                     To_Address
                       (To_Integer (Base + Size) -
                          Minimum_Stack_Size_In_Bytes);

      LR_Save_Word : System.Address;
      for LR_Save_Word'Address use Initial_SP + Storage_Offset'(4);

      Back_Chain_Word : System.Address;
      for Back_Chain_Word'Address use Initial_SP;

   begin
      --  Put Null to these two values to clear the stack.

      LR_Save_Word    := Null_Address;
      Back_Chain_Word := Null_Address;

      Stack_Pointer := Initial_SP;
   end Initialize_Stack;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
      procedure Start_Thread_Asm;
      pragma Import (Asm, Start_Thread_Asm);

      Initial_SP : Address;

   begin
      --  No need to initialize the contest of the environment task

      if Program_Counter = Null_Address then
         return;
      end if;

      --  We cheat as we don't know the stack size nor the stack base

      Initialize_Stack (Stack_Pointer, 0, Initial_SP);

      --  Overwrite Stack Pointer and Program Counter with values that have
      --  been passed as arguments. The Stack Pointer of the task is 2 words
      --  below Stack_Pointer. These two words correspond to the header of the
      --  new stack. This header contains the LR_Save_Word and Back_Chain_Word.
      --  Program_Counter points to the task_wrapper procedure.

      --  We create a new stack pointer with a size of at least 8 which are
      --  reserved for the header, but we also have to make sure that the stack
      --  is aligned with Standard'Maximum_Alignment

      Buffer.R1 := Initial_SP;
      Buffer.LR := Start_Thread_Asm'Address;

      Buffer.R14 := Program_Counter;
      Buffer.R15 := Argument;
   end Initialize_Context;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      --  To be implemented ???

      null;
   end Install_Error_Handlers;

   -------------------------------
   -- Initialize_Floating_Point --
   -------------------------------

   procedure Initialize_Floating_Point is
   begin
      CPU_Specific.Initialize_CPU;
   end Initialize_Floating_Point;

   -------------
   -- Set_MSR --
   -------------

   --  Note: there is no context synchronization. If required, this must be
   --  done explicitly by the caller.

   procedure Set_MSR (MSR : MSR_Type) is
   begin
      Asm ("mtmsr %0",
           Inputs => MSR_Type'Asm_Input ("r", MSR),
           Volatile => True);
   end Set_MSR;

end System.BB.CPU_Primitives;
