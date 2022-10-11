------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2014, AdaCore                     --
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

--  Run-time symbolic traceback support for targets using DWARF debug data

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;

with Interfaces.C.Strings; use Interfaces.C.Strings;

with System.Dwarf_Lines; use System.Dwarf_Lines;

package body GNAT.Traceback.Symbolic is

   package Module_Name is

      function Get (Addr : access System.Address) return String;
      --  Returns the module name for the given address, Addr may be updated
      --  to be set relative to a shared library. This depends on the platform.
      --  Returns an empty string for the main executable.

      function Is_Supported return Boolean;
      pragma Inline (Is_Supported);
      --  Returns True if Module_Name is supported, so if the traceback is
      --  supported for shared libraries.

   end Module_Name;

   package body Module_Name is separate;

   function Executable_Name return String;
   --  Returns the executable name as reported by argv[0]. If gnat_argv not
   --  initialized or if argv[0] executable not found in path, function returns
   --  an empty string.

   function Get_Executable_Load_Address return System.Address;
   pragma Import (C, Get_Executable_Load_Address,
                    "__gnat_get_executable_load_address");
   --  Get the load address of the executable, or Null_Address if not known

   function Module_Symbolic_Traceback
     (Module_Name  : String;
      Traceback    : Tracebacks_Array;
      Load_Address : System.Address) return String;
   --  Returns the Traceback for a given module or an empty string if not in
   --  a module. Parameter Load_Address is the load address of the module,
   --  or Null_Address is not rebased.

   function Symbolic_Traceback
     (Traceback    : Tracebacks_Array;
      Exec_Name    : String;
      Exec_Address : System.Address) return String;
   --  Build string containing symbolic traceback for the given call chain,
   --  using Exec_Name for the path of the executable and Exec_Address for
   --  its load address.

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name return String is
      type Argv_Array is array (0 .. 0) of chars_ptr;
      gnat_argv : access Argv_Array;
      pragma Import (C, gnat_argv, "gnat_argv");

      function locate_exec_on_path
        (Name : chars_ptr) return chars_ptr;
      pragma Import (C, locate_exec_on_path, "__gnat_locate_exec_on_path");

      procedure cfree (Ptr : chars_ptr);
      pragma Import (C, cfree, "free");

   begin
      if gnat_argv /= null then
         declare
            chars : constant chars_ptr := locate_exec_on_path (gnat_argv (0));

         begin
            if chars /= Null_Ptr then
               declare
                  Result : constant String := Value (chars);

               begin
                  --  The buffer returned by locate_exec_on_path was
                  --  allocated using malloc(), so we should use free() to
                  --  free the memory and not Interfaces.C.Strings.Free
                  --  which calls __gnat_free.

                  cfree (chars);
                  return Result;
               end;
            end if;
         end;
      end if;

      --  If gnat_argv not initialized or if argv[0] executable not found in
      --  path, function returns empty string.

      return "";
   end Executable_Name;

   -------------------------------
   -- Module_Symbolic_Traceback --
   -------------------------------

   function Module_Symbolic_Traceback
     (Module_Name  : String;
      Traceback    : Tracebacks_Array;
      Load_Address : System.Address) return String
   is
      C : Dwarf_Context (In_Exception => True);

   begin
      Open (Module_Name, C);

      --  If a module can't be opened just return an empty string, we
      --  just cannot give more information in this case.

      if not Is_Open (C) then
         return "";
      end if;

      Set_Load_Address (C, Load_Address);

      declare
         Result : constant String :=
                    Symbolic_Traceback
                      (C, Traceback,
                       Suppress_Hex => Symbolic.Module_Name.Is_Supported);

      begin
         Close (C);

         if Symbolic.Module_Name.Is_Supported then
            return '[' & Module_Name & ']' & ASCII.LF & Result;
         else
            return Result;
         end if;
      end;

      --  We must not allow an unhandled exception here, since this function
      --  may be installed as a decorator for all automatic exceptions.

   exception
      when others =>
         return "";
   end Module_Symbolic_Traceback;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback
     (Traceback    : Tracebacks_Array;
      Exec_Name    : String;
      Exec_Address : System.Address) return String
   is
      TB : Tracebacks_Array (Traceback'Range);
      --  A partial copy of the possibly relocated traceback addresses. These
      --  addresses gets relocated for GNU/Linux shared library for example.
      --  This gets done in the Get_Module_Name routine.

   begin
      if Traceback'Length = 0 then
         return "";
      end if;

      declare
         Addr   : aliased System.Address := Traceback (Traceback'First);
         M_Name : constant String := Module_Name.Get (Addr'Access);
         Pos    : Positive;

      begin
         --  Will symbolize the first address...

         TB (TB'First) := Addr;

         Pos := TB'First + 1;

         --  ... and all addresses in the same module

         Same_Module : loop
            exit Same_Module when Pos > Traceback'Last;

            --  Get address to check for corresponding module name

            Addr := Traceback (Pos);

            exit Same_Module when Module_Name.Get (Addr'Access) /= M_Name;

            --  Copy the possibly relocated address into TB

            TB (Pos) := Addr;

            Pos := Pos + 1;
         end loop Same_Module;

         --  Symbolize the addresses in the same module, and do a recursive
         --  call for the remaining addresses.

         if M_Name = "" then
            return Module_Symbolic_Traceback
                     (Exec_Name, TB (TB'First .. Pos - 1), Exec_Address)
                   & Symbolic_Traceback (Traceback (Pos .. Traceback'Last));
         else
            return Module_Symbolic_Traceback
                     (M_Name, TB (TB'First .. Pos - 1), System.Null_Address)
                   & Symbolic_Traceback (Traceback (Pos .. Traceback'Last));
         end if;
      end;
   end Symbolic_Traceback;

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String is
      Exec_Path : constant String         := Executable_Name;
      Exec_Load : constant System.Address := Get_Executable_Load_Address;
   begin
      if Symbolic.Module_Name.Is_Supported then
         return Symbolic_Traceback (Traceback, Exec_Path, Exec_Load);
      else
         return Module_Symbolic_Traceback (Exec_Path, Traceback, Exec_Load);
      end if;
   end Symbolic_Traceback;

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String
   is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
