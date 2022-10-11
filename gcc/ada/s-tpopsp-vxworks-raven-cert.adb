------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                SYSTEM.TASK_PRIMITIVES.OPERATIONS.SPECIFIC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 1998-2012, AdaCore                    --
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
------------------------------------------------------------------------------

--  This is a VxWorks version of this package for ravenscar-cert (VxWorks 6
--  Cert DKM/SKM, VxWorks 653 and VxWorks MILS vThreads). The implementation is
--  based on VxWorks taskVarLib.

separate (System.Task_Primitives.Operations)
package body Specific is

   ATCB_Key : aliased System.Address := System.Null_Address;
   --  Key used to find the Ada Task_Id associated with a thread

   ATCB_Key_Addr : System.Address := ATCB_Key'Address;
   pragma Export (Ada, ATCB_Key_Addr, "__gnat_ATCB_key_addr");
   --  Exported to support the temporary VxWorks 653 task registration
   --  implementation. This mechanism is used to minimize impact on other
   --  targets.

   function Is_Valid_Task return Boolean;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean is
   begin
      return taskVarGet (taskIdSelf, ATCB_Key'Access) /= ERROR;
   end Is_Valid_Task;

   ---------
   -- Set --
   ---------

   procedure Set (Self_Id : Task_Id) is
      Result : STATUS;

   begin
      if not Is_Valid_Task then
         Result := taskVarAdd (0, ATCB_Key'Access);
         pragma Assert (Result = OK);
      end if;

      ATCB_Key := To_Address (Self_Id);
   end Set;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
   begin
      return To_Task_Id (ATCB_Key);
   end Self;

end Specific;
