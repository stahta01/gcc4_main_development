------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . G C C . S H I F T S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2013-2014, Free Software Foundation, Inc.          --
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

--  Ada implementation of libgcc: 64-bit shifts

pragma Restrictions (No_Elaboration_Code);

package System.GCC.Shifts is
   pragma Pure;

   function Lshrdi3 (Val : Unsigned_64; Count : Integer) return Unsigned_64;
   pragma Export (C, Lshrdi3, "__lshrdi3");
   --  Logical right rotation of Val by Count position. Return 0 if Count is
   --  greater or equal than 64, and Val if less than 1.

   function Ashrdi3 (Val : Unsigned_64; Count : Integer) return Unsigned_64;
   pragma Export (C, Ashrdi3, "__ashrdi3");
   --  Arithmetic right rotation of Val by Count position. Return 0 or -1 if
   --  Count is greater or equal than 64, and Val if less than 1.

   function Ashldi3 (Val : Unsigned_64; Count : Integer) return Unsigned_64;
   pragma Export (C, Ashldi3, "__ashldi3");
   --  Left rotation of Val by Count position. Return 0 if Count is greater or
   --  equal than 64, and Val if less than 1.

end System.GCC.Shifts;
