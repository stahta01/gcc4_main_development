------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . P A C K _ 2 9                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with System.Storage_Elements;
with System.Unsigned_Types;
with Ada.Unchecked_Conversion;

package body System.Pack_29 is

   subtype Ofs is System.Storage_Elements.Storage_Offset;
   subtype Uns is System.Unsigned_Types.Unsigned;
   subtype N07 is System.Unsigned_Types.Unsigned range 0 .. 7;

   use type System.Storage_Elements.Storage_Offset;
   use type System.Unsigned_Types.Unsigned;

   type Cluster is record
      E0, E1, E2, E3, E4, E5, E6, E7 : Bits_29;
   end record;

   for Cluster use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
      E3 at 0 range 3 * Bits .. 3 * Bits + Bits - 1;
      E4 at 0 range 4 * Bits .. 4 * Bits + Bits - 1;
      E5 at 0 range 5 * Bits .. 5 * Bits + Bits - 1;
      E6 at 0 range 6 * Bits .. 6 * Bits + Bits - 1;
      E7 at 0 range 7 * Bits .. 7 * Bits + Bits - 1;
   end record;

   for Cluster'Size use Bits * 8;

   for Cluster'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   type Cluster_Ref is access Cluster;

   function To_Ref is new
     Ada.Unchecked_Conversion (System.Address, Cluster_Ref);

   ------------
   -- Get_29 --
   ------------

   function Get_29 (Arr : System.Address; N : Natural) return Bits_29 is
      C : constant Cluster_Ref := To_Ref (Arr + Bits * Ofs (Uns (N) / 8));
   begin
      case N07 (Uns (N) mod 8) is
         when 0 => return C.E0;
         when 1 => return C.E1;
         when 2 => return C.E2;
         when 3 => return C.E3;
         when 4 => return C.E4;
         when 5 => return C.E5;
         when 6 => return C.E6;
         when 7 => return C.E7;
      end case;
   end Get_29;

   ------------
   -- Set_29 --
   ------------

   procedure Set_29 (Arr : System.Address; N : Natural; E : Bits_29) is
      C : constant Cluster_Ref := To_Ref (Arr + Bits * Ofs (Uns (N) / 8));
   begin
      case N07 (Uns (N) mod 8) is
         when 0 => C.E0 := E;
         when 1 => C.E1 := E;
         when 2 => C.E2 := E;
         when 3 => C.E3 := E;
         when 4 => C.E4 := E;
         when 5 => C.E5 := E;
         when 6 => C.E6 := E;
         when 7 => C.E7 := E;
      end case;
   end Set_29;

end System.Pack_29;
