------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . G C C . D I V I S I O N S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2013, Free Software Foundation, Inc.            --
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

--  Ada implementation of libgcc: 64-bit Divisions

package body System.GCC.Divisions is
   use Interfaces;

   function Udivmoddi3 (Num : Unsigned_64;
                        Den : Unsigned_64;
                        Return_Rem : Boolean)
                       return Unsigned_64;
   --  Divide Num by Den and return the remainder if Return_Rem is true, or
   --  the quotient if false.

   function Divmoddi3 (Num : Integer_64;
                       Den : Integer_64;
                       Return_Rem : Boolean)
                       return Integer_64;
   ----------------
   -- Udivmoddi3 --
   ----------------

   function Udivmoddi3 (Num : Unsigned_64;
                        Den : Unsigned_64;
                        Return_Rem : Boolean)
                       return Unsigned_64
   is
      Q, R : Unsigned_64;
      N : Unsigned_64;
   begin
      --  Check for division by 0
      if Den = 0 then
         raise Constraint_Error;
      end if;

      --  Restoring algorithm
      Q := 0;
      R := 0;
      N := Num;
      for I in 1 .. 64 loop
         --  Insert one bit from the numerator
         R := Shift_Left (R, 1) or Shift_Right (N, 63);
         N := Shift_Left (N, 1);

         --  Compute one bit of the quotient
         Q := Shift_Left (Q, 1);
         if R >= Den then
            R := R - Den;
            Q := Q or 1;
         end if;

         --  Loop invariants (they are also true before the loop):

         --  The remainder is always less than the denominator
         pragma Assert (R < Den);

         --  Division definition
         pragma Assert (Shift_Left (Q, 64 - I) * Den
                          + Shift_Left (R, 64 - I)
                          + Shift_Right (N, I) = Num);
      end loop;

      if Return_Rem then
         return R;
      else
         return Q;
      end if;
   end Udivmoddi3;

   -------------
   -- Udivdi3 --
   -------------

   function Udivdi3 (Num : Unsigned_64; Den : Unsigned_64)
                    return Unsigned_64 is
   begin
      return Udivmoddi3 (Num, Den, False);
   end Udivdi3;

   -------------
   -- Umoddi3 --
   -------------

   function Umoddi3 (Num : Unsigned_64; Den : Unsigned_64)
                    return Unsigned_64 is
   begin
      return Udivmoddi3 (Num, Den, True);
   end Umoddi3;

   ---------------
   -- Divmoddi3 --
   ---------------

   function Divmoddi3 (Num : Integer_64;
                       Den : Integer_64;
                       Return_Rem : Boolean)
                       return Integer_64
   is
      Neg : Boolean := False;
      N : Unsigned_64;
      D : Unsigned_64;
      R : Unsigned_64;
   begin
      if Num < 0 then
         Neg := True;
         N := Unsigned_64 (-Num);
      else
         N := Unsigned_64 (Num);
      end if;

      if Den < 0 then
         Neg := not Neg;
         D := Unsigned_64 (-Den);
      else
         D := Unsigned_64 (Den);
      end if;

      R := Udivmoddi3 (N, D, Return_Rem);

      --  The remainder has the sign of the numerator
      if Return_Rem then
         Neg := Num < 0;
      end if;

      if Neg then
         return -Integer_64 (R);
      else
         return Integer_64 (R);
      end if;
   end Divmoddi3;

   ------------
   -- Divdi3 --
   ------------

   function Divdi3 (Num : Integer_64; Den : Integer_64) return Integer_64 is
   begin
      return Divmoddi3 (Num, Den, False);
   end Divdi3;

   ------------
   -- Moddi3 --
   ------------

   function Moddi3 (Num : Integer_64; Den : Integer_64) return Integer_64 is
   begin
      return Divmoddi3 (Num, Den, True);
   end Moddi3;

end System.GCC.Divisions;
