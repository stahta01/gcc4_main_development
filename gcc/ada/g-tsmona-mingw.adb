------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--  G N A T . T R A C E B A C K . S Y M B O L I C . M O D U L E _ N A M E   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

--  This is the Windows specific version of this package

with Interfaces.C; use Interfaces.C;

with System.Win32; use System.Win32;

separate (GNAT.Traceback.Symbolic)

package body Module_Name is

   use System;

   ---------
   -- Get --
   ---------

   function Get (Addr : access System.Address) return String is
      Res     : DWORD;
      hModule : aliased HANDLE;
      Path    : String (1 .. 1_024);

   begin
      if GetModuleHandleEx
        (GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
         Addr.all,
         hModule'Access) = Win32.TRUE
      then
         Res := GetModuleFileName (hModule, Path'Address, Path'Length);

         if FreeLibrary (hModule) = Win32.FALSE then
            null;
         end if;

         if Res > 0 then
            return Path (1 .. Positive (Res));
         end if;
      end if;

      return "";

   exception
      when others =>
         return "";
   end Get;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported return Boolean is
   begin
      return True;
   end Is_Supported;

end Module_Name;
