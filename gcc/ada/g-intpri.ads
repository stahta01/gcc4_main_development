------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--            G N A T . I N T E R R U P T S _ P R I O R I T I E S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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
------------------------------------------------------------------------------

--  This package is specific to the ravenscar-minimal runtime for
--  PowerPC 8321/8349.

with System;
with Ada.Interrupts;

with System.BB.Board_Support;
pragma Unreferenced (System.BB.Board_Support);
--  Ensure that BB.Board_Support is elaborated before this package

package GNAT.Interrupt_Priorities is

   procedure Define_Interrupt_Priority
     (Interrupt : Ada.Interrupts.Interrupt_ID;
      Priority : System.Interrupt_Priority;
      Mask_Bit : Natural);
   --  Define the priority and the mask for Interrupt.
   --
   --  Mask_Bit is the bit number in the SIMSR_H/SIMSR_L/SEMSR registers.
   --  Bit numbers 0 to 31 means bit 0 (MSB) to 31 (LSB) of SIMSR_H,
   --  bit numbers 32 to 63 means bit 0 (MSB) to 31 (LSB) of SIMSR_L,
   --  and bit numbers 64 to 95 means bit 0 (MSB) to 31 (LSB) of SEMSR.

private
   pragma Import (Ada, Define_Interrupt_Priority,
                  "__gnat_define_interrupt_priority");
end GNAT.Interrupt_Priorities;
