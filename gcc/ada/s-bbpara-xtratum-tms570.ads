------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P A R A M E T E R S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2013, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines basic parameters used by the low level tasking system

--  This is the TMS570 (ARMv7) version of this package

pragma Restrictions (No_Elaboration_Code);

package System.BB.Parameters is
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Frequency : constant := 1_000_000;  --  Hertz
   --  Frequency of the system clock (XtratuM resolution is 1 microsecond)

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   Number_Of_Interrupt_ID : constant := 96 + 32;
   --  Number of interrupts supported by the VIC. For the TMS570 interrupts,
   --  we really only consider the 95 usable interrupt channels. The run time
   --  assumes the interrupt source to interrupt channel map is direct (1:1),
   --  as is the default, but the user can change this as long as the IRQ used
   --  by the system for alarms stays unchanged.

   Trap_Vectors : constant := 7;
   --  ARM in general has these traps:
   --    0   (at 16#0000#) Reset
   --    1   (at 16#0004#) Undefined Instruction (synchronous)
   --    2   (at 16#0008#) Supervisor Call (synchronous)
   --    3   (at 16#000C#) Abort - Prefetch (synchronous)
   --    4   (at 16#0010#) Abort - Data (asynchronous)
   --    5   (at 16#0014#) IRQ Trap (asynchronous)
   --    6   (at 16#0018#) FIQ Trap (asynchronous)

   ------------------------
   -- Context Management --
   ------------------------

   --  The run time stores a minimal amount of state in the thread context.
   --  Most state will be saved on the task's stack when calling a potentially
   --  blocking operation, or on the interrupt stack when the task is pre-
   --  empted. Most of the space is currently required for floating point
   --  state, which is saved lazily.

   --  The TMS570 processor needs to save:

   --   * 8 + 2 registers of 32 bits: r4-r11 (v1-v8), LR, SP
   --     for normal processing
   --   * 16 for s16-s31
   --   * 1 for fpscr

   Context_Buffer_Capacity : constant := 28;

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 512;  --  bytes
   --  Size of each of the interrupt stacks. Each processor has its own
   --  set of interrupt stacks, one per interrupt priority.

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
