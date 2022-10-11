------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the version for P2020 targets of this package

pragma Restrictions (no_elaboration_code);

with System;
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  Note: do not forget to update procedures Priority_Of_Interrupt and
   --  Initialize_Board of package System.BB.Board_Support (s-bbbosu) when
   --  you modify this package (in particular when you add an interrupt).

   --  The interrupt ID number is purely internal and used only to set the
   --  vector in the PVR (Priority Vector Registers). Do not use 0 as it is
   --  reserved by the runtime for the spurious interrupt.

   --  You can use any priority as long as it belongs to the range defined by
   --  Interrupt_Priority'First .. Interrupt_Priority'Last - 1.

   --  It is not recommended to use Interrupt_Priority'First as such priority
   --  are always disabled by the OpenPIC.

   --  Interrupt_Priority'Last is reserved for non-OpenPIC interrupts such
   --  as the decrementer.

   Interprocessor_Interrupt_0          : constant Interrupt_ID := 1;
   Interprocessor_Interrupt_0_Priority : constant System.Interrupt_Priority :=
                                           System.Interrupt_Priority'First +
                                             1;

   Interprocessor_Interrupt_1          : constant Interrupt_ID := 2;
   Interprocessor_Interrupt_1_Priority : constant System.Interrupt_Priority :=
                                           System.Interrupt_Priority'First +
                                             4;

   Interprocessor_Interrupt_2          : constant Interrupt_ID := 3;
   Interprocessor_Interrupt_2_Priority : constant System.Interrupt_Priority :=
                                           System.Interrupt_Priority'First +
                                             8;

   Interprocessor_Interrupt_3          : constant Interrupt_ID := 4;
   Interprocessor_Interrupt_3_Priority : constant System.Interrupt_Priority :=
                                           System.Interrupt_Priority'First +
                                             12;
end Ada.Interrupts.Names;
