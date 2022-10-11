------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          S Y S T E M . G C C                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2013-2014, Free Software Foundation, Inc.         --
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

--  Parent package for Ada implementation of libgcc

pragma Restrictions (No_Elaboration_Code);

with Interfaces;

package System.GCC is
   pragma Pure;

   subtype Unsigned_64 is Interfaces.Unsigned_64;
   subtype Integer_64 is Interfaces.Integer_64;
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   --  Renaming to avoid full names in specifications

   procedure Split
     (Val : Unsigned_64;
      Hi  : out Unsigned_32;
      Lo  : out Unsigned_32);
   pragma Inline (Split);
   --  Extract the most significant word of Val to Hi and the least one to Lo.
   --  The code generated by the compiler should be stand-alone and highly
   --  efficient.

   function Merge (Hi : Unsigned_32; Lo : Unsigned_32) return Unsigned_64;
   pragma Inline (Merge);
   --  Create an unsigned_64 from Hi and Lo. The code generated by the
   --  compiler should be stand-alone and highly efficient.

end System.GCC;
