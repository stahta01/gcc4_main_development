------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                SYSTEM.TASK_PRIMITIVES.OPERATIONS.SPECIFIC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 1998-2011, AdaCore                    --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a VxWorks version of this package using Thread_Local_Storage
--  support for use with ravenscar-cert-rtp. It assumes VxWorks Cert 6.6.3 or
--  more recent. The implementation is based on __threads support.

separate (System.Task_Primitives.Operations)
package body Specific is

   ATCB : aliased Task_Id := null;
   pragma Thread_Local_Storage (ATCB);
   --  Ada Task_Id associated with a thread

   ---------
   -- Set --
   ---------

   procedure Set (Self_Id : Task_Id) is
   begin
      ATCB := Self_Id;
   end Set;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
   begin
      return ATCB;
   end Self;

end Specific;
