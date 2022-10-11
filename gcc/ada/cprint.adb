------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C P R I N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Fname;    use Fname;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Ttypes;   use Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Cprint is
   Current_Source_File : Source_File_Index;
   --  Index of source file whose generated code is being dumped

   Dump_Node : Node_Id := Empty;
   --  This is set to the current node, used for printing line numbers

   FLCache_N  : Node_Id := Empty;
   FLCache_FL : Physical_Line_Number;
   FLCache_LL : Physical_Line_Number;
   --  Cache for First_Line and Last_Line (N records last node for which any
   --  of these subprograms were called, FL and LL record the corresponding
   --  First and Last physical line numbers for this node).

   Freeze_Level : Int := 0;
   --  Keep track of freeze level (incremented on entry to freeze actions and
   --  decremented on exit). Used to know if we are within freeze actions.

   Indent : Int := 0;
   --  Number of columns for current line output indentation

   Last_Line_Printed : Physical_Line_Number;
   --  This keeps track of the physical line number of the last source line
   --  for which Write_Source_Lines has processed #line/source output.

   No_Physical_Line_Number : constant Physical_Line_Number :=
                               Physical_Line_Number'Last;
   --  Used internally to indicate no line number available

   --  The following constants are used by Write_Uint_Col_Check. They are
   --  initialized as shown when Source_Dump is called:

   ints  : Nat renames Standard_Integer_Size;
   longs : Nat renames Standard_Long_Integer_Size;
   lls   : Nat renames Standard_Long_Long_Integer_Size;
   --  Length in bits of int, long, long long

   LNegInt  :  Uint; --  -(Uint_2 ** (ints - 1));
   LPosInt  :  Uint; --  abs (LNegInt + 1);
   LNegLong :  Uint; --  -(Uint_2 ** (longs - 1));
   LPosLong :  Uint; --  abs (LNegLong + 1);
   LNegLL   :  Uint; --  -(Uint_2 ** (lls - 1));
   LPosLL   :  Uint; --  abs (LNegLL + 1);
   --  Bounds of int, long, long long

   LPosU    :  Uint; --  (Uint_2 ** ints) - 1;
   LNegU    :  Uint; --  -LPosU;
   LPosUL   :  Uint; --  (Uint_2 ** longs) - 1;
   LNegUL   :  Uint; --  -LPosUL;
   LPosULL  :  Uint; --  (Uint_2 ** lls) - 1;
   LNegULL  :  Uint; --  -LPosULL;
   --  Bounds of unsigned, long unsigned, long long unsigned

   ------------------------------------------
   -- Procedures for printing C constructs --
   ------------------------------------------

   procedure Cprint_And_List (List : List_Id);
   --  Print the given list with items separated by vertical "and"

   procedure Cprint_Bar_List (List : List_Id);
   --  Print the given list with items separated by vertical bars

   procedure Cprint_Call (Node : Node_Id);
   --  Outputs a function or procedure call, with its parameters, dealing
   --  with the case of passing addresses for OUT or IN OUT parameters

   procedure Cprint_Comma_List (List : List_Id);
   --  Prints the nodes in a list, with separating commas. If the list is empty
   --  then no output is generated.

   procedure Cprint_Declare
     (Ent        : Entity_Id;
      Add_Access : Boolean := False;
      Virtual_OK : Boolean := False);
   --  Ent is either a type or object. This procedure prints either a typedef
   --  declaration for a type, or a normal C declaration for an object. The
   --  output does not include the terminating semicolon. If Add_Access is set
   --  to true, then the type has an extra access, i.e. if we have A of type B
   --  then a declaration for A of type *B is output. Note that there is no
   --  indent call, the caller should call Indent if a new line is needed.
   --  Virtual_OK deals with the case of unconstrained array types. When a
   --  normal variable of such a type is declared, the bounds are present in
   --  the type, and are the bounds to be output (case of Virtual_OK = False).
   --  But in e.g. the formal of a call, the bounds come from the caller, and
   --  if the type is unconstrained are to be output simply as []. In this
   --  case Virtual_OK is set True. Bounds are also output as [] if the array
   --  is variable length and Add_Access is True.

   procedure Cprint_Difference (Val1 : Node_Id; Val2 : Uint; B : Boolean);
   procedure Cprint_Difference (Val1 : Uint; Val2 : Node_Id; B : Boolean);
   pragma Unreferenced (Cprint_Difference);
   procedure Cprint_Difference (Val1, Val2 : Node_Id; B : Boolean);
   --  Outputs the value of Val1 - Val2, using a single integer value if the
   --  value is known at compile time and otherwise prints val1 - val2. B
   --  is True if parens should be used in the compound case, false otherwise

   procedure Cprint_Indented_List (List : List_Id);
   --  Like Cprint_Line_List, except that the indentation level is increased
   --  before outputting the list of items, and then decremented (back to its
   --  original level) before returning to the caller.

   procedure Cprint_Left_Opnd (N : Node_Id);
   --  Print left operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.

   procedure Cprint_Node (Node : Node_Id);
   --  Prints a single node. No new lines are output, except as required for
   --  splitting lines that are too long to fit on a single physical line.
   --  No output is generated at all if Node is Empty. No trailing or leading
   --  blank characters are generated.

   procedure Cprint_Node_List (List : List_Id; New_Lines : Boolean := False);
   --  Prints the nodes in a list with no separating characters. This is used
   --  in the case of lists of items which are printed on separate lines using
   --  the current indentation amount. New_Lines controls the generation of
   --  New_Line calls. If False, no New_Line calls are generated. If True,
   --  then New_Line calls are generated as needed to ensure that each list
   --  item starts at the beginning of a line.

   procedure Cprint_Node_Paren (N : Node_Id);
   --  Prints node, adding parentheses if N is an operator, or short circuit
   --  operation or other subexpression which needs parenthesizing as an
   --  operand (we always fully parenthesize expression trees in the C output).

   procedure Cprint_Opt_Node (Node : Node_Id);
   --  Same as normal Cprint_Node procedure, except that one leading blank is
   --  output before the node if it is non-empty.

   procedure Cprint_Opt_Node_List (List : List_Id);
   --  Like Cprint_Node_List, but prints nothing if List = No_List

   procedure Cprint_Opt_Paren_Comma_List (List : List_Id);
   --  Same as normal Cprint_Paren_Comma_List procedure, except that an extra
   --  blank is output if List is non-empty, and nothing at all is printed it
   --  the argument is No_List.

   procedure Cprint_Paren_Comma_List (List : List_Id);
   --  Prints the nodes in a list, surrounded by parentheses, and separated by
   --  commas. If the list is empty, then no output is generated. A blank is
   --  output before the initial left parenthesis.

   procedure Cprint_Right_Opnd (N : Node_Id);
   --  Print right operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.

   pragma Warnings (Off); -- in case unreferenced
   procedure Cprint_Sum (Val1 : Node_Id; Val2 : Uint; B : Boolean);
   procedure Cprint_Sum (Val1 : Uint; Val2 : Node_Id; B : Boolean);
   procedure Cprint_Sum (Val1, Val2 : Node_Id; B : Boolean);
   pragma Warnings (On);
   --  Outputs the value of Val1 + Val2, using a single integer value if the
   --  value is known at compile time and otherwise prints (val1 + val2). B
   --  is True if parens should be used in the compound case, false otherwise

   procedure Cprint_Type_Name (Typ : Entity_Id; No_TD : Boolean := False);
   --  Output C representation of Ada type Typ. No_TD is normally False, in
   --  which case if Typ comes from source, the Typ name is just printed since
   --  it is assumed to be a typedef name. No_TD can be set True to avoid this
   --  behavior. This is used when Cprint_Type_Name is called from typedef
   --  circuitry, to avoid a typedef pointing to itself!

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Col_Check (N : Nat);
   --  Check that at least N characters remain on current line, and if not,
   --  then start an extra line with two characters extra indentation for
   --  continuing text on the next line.

   procedure Extra_Blank_Line;
   --  In some situations we write extra blank lines to separate the generated
   --  code to make it more readable. However, these extra blank lines are
   --  not generated in Dump_Source_Text mode, since there the source text
   --  lines output with preceding blank line and #line directives are
   --  quite sufficient as separators. This procedure writes a blank line if
   --  Dump_Source_Text is False unless we just wrote a { in which case a blank
   --  line would be redundant.

   function First_Line (N : Node_Id) return Physical_Line_Number;
   --  Given a subtree, determines the first physical line number for any node
   --  in the subtree. Returns No_Physical_Line_Number if no value found.

   function Last_Line (N : Node_Id) return Physical_Line_Number;
   --  Given a subtree, determines the last physical line number for any node
   --  in the subtree. Returns No_Physical_Line_Number if no value found.

   procedure Get_First_Last_Line (N : Node_Id);
   --  Determines first and last physical line number for subtree N, placing
   --  the result in FLCache. Result is No_Physical_Line_Number if node N does
   --  not come from current source file.

   function Has_Typedef (Typ : Entity_Id) return Boolean;
   --  Returns True if the given Typ entity is for a type which we generate a
   --  typedef. This is true for all types coming from source, and also for a
   --  base type whose first subtype comes from source.

   procedure Indent_Begin;
   --  Increase indentation level

   procedure Indent_End;
   --  Decrease indentation level

   function Parens_Needed (N : Node_Id) return Boolean;
   --  Returns True if N is in a context where it is not known to be safe to
   --  leave an expression unparenthesized. This is conservative. False means
   --  is is definitely safe to leave out parens, True means that parens may
   --  be needed so they will be put in. Right now, the test is limited to
   --  being the right side of an assignment.

   function Pass_Pointer (Ent : Entity_Id) return Boolean;
   --  Ent is the entity for a formal parameter. This function returns True if
   --  the corresponding object must be passed by using a pointer in C (i.e. by
   --  adding * in the definition of the formal, and & for calls). This is True
   --  for IN and IN OUT parameters and for records (unless the C_Pass_By_Copy
   --  convention applies). Note that it is never True for arrays, since in C,
   --  arrays are always passed in pointer form in any case.

   procedure Process_TFAI_RR_Flags (Nod : Node_Id);
   --  Given a divide, multiplication or division node, check the flags
   --  Treat_Fixed_As_Integer and Rounded_Flags, and if set, output the
   --  appropriate special syntax characters (# and @).

   procedure Write_Discr_Specs (N : Node_Id);
   --  Output discriminant specification for node, which is any of the type
   --  declarations that can have discriminants.

   procedure Write_Id (N : Node_Id);
   --  N is a node with a Chars field. This procedure writes the name that
   --  will be used in the generated code associated with the name. For a
   --  node with no associated entity, this is simply the Chars field. For
   --  the case where there is an entity associated with the node, we print
   --  the name associated with the entity (since it may have been encoded).
   --  One other special case is that an entity has an active external name
   --  (i.e. an external name present with no address clause), then this
   --  external name is output. This procedure also deals with outputting
   --  declarations of referenced itypes, if not output earlier.

   procedure Write_Indent;
   --  Start a new line and write indentation spacing

   procedure Write_Indent_Str (S : String);
   --  Start a new line and write indent spacing followed by given string

   procedure Write_Name_Col_Check (N : Name_Id);
   --  Write name (using Write_Name) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Param_Specs (N : Node_Id);
   --  Output parameter specifications for node (which is either a function or
   --  procedure specification with a Parameter_Specifications field)

   procedure Write_Source_Lines (From, To : Physical_Line_Number);
   --  From, To are the start/end physical line numbers for the construct
   --  whose C translation is about to be printed. This routine takes care of
   --  generating required #line directives, and also in Dump_Source_Text mode,
   --  prints non-comment source Ada lines as C comments.

   procedure Write_Source_Lines (N : Node_Id);
   --  Same, but From, To are First_Line, Last_Line of node N

   procedure Write_Source_Lines (S : Source_Ptr);
   --  Same, but From and To both correspond to the given Source_Ptr value

   procedure Write_Source_Lines (From : Source_Ptr; To : Physical_Line_Number);
   --  Same, but From is line corresponding to given source_Ptr value.

   procedure Write_Str_Col_Check (S : String);
   --  Write string (using Write_Str) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Subprogram_Name (N : Node_Id);
   --  N is the Name field of a function call or procedure statement call. The
   --  effect of the call is to output the name, preceded by a $ if the call is
   --  identified as an implicit call to a run time routine.

   procedure Write_Uint_Col_Check (U : Uint);
   --  Write Uint (using UI_Write) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.
   --  The output is always in decimal. Takes care of special cases of the
   --  largest negative number, and possible long integer output.

   procedure Write_Ureal_Col_Check (U : Ureal);
   --  Write Ureal (using same output format as UR_Write) with column checks
   --  and a possible initial Write_Indent (to get new line) if current line
   --  is too full.

   ---------------
   -- Col_Check --
   ---------------

   procedure Col_Check (N : Nat) is
   begin
      if N + Column > Sprint_Line_Limit then
         Write_Indent_Str ("  ");
      end if;
   end Col_Check;

   ---------------------
   -- Cprint_And_List --
   ---------------------

   procedure Cprint_And_List (List : List_Id) is
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Cprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
            Write_Str (" and ");
         end loop;
      end if;
   end Cprint_And_List;

   ---------------------
   -- Cprint_Bar_List --
   ---------------------

   procedure Cprint_Bar_List (List : List_Id) is
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Cprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
            Write_Str (" | ");
         end loop;
      end if;
   end Cprint_Bar_List;

   -----------------
   -- Cprint_Call --
   -----------------

   procedure Cprint_Call (Node : Node_Id) is
      Actual : Node_Id;
      Formal : Node_Id;

   begin
      Cprint_Node (Name (Node));
      Write_Char ('(');

      Actual := First_Actual (Node);
      Formal := First_Formal (Entity (Name (Node)));
      while Present (Actual) loop
         if Pass_Pointer (Formal) then
            Write_Char ('&');
         end if;

         Cprint_Node (Actual);

         --  Add bounds if formal is unconstrained array. For now, just assume
         --  actual is constrained (deal with unconstrained actuals later).

         declare
            Ftyp : constant Entity_Id := Etype (Formal);
            Atyp : constant Entity_Id := Etype (Actual);

         begin
            if Is_Array_Type (Ftyp) and then not Is_Constrained (Ftyp) then
               Write_Str_Col_Check (", ");

               --  Annoying special case of string literal

               if Ekind (Atyp) = E_String_Literal_Subtype then
                  UI_Write
                    (Intval (String_Literal_Low_Bound (Atyp)),
                     Decimal);

                  Write_Str (", ");

                  UI_Write
                    (String_Literal_Length (Atyp) -
                       Intval (String_Literal_Low_Bound (Atyp)) + 1,
                     Decimal);

               --  All other cases

               else
                  declare
                     LB : Node_Id;
                     UB : Node_Id;

                  begin
                     --  Slice, get slice bounds

                     if Nkind (Actual) = N_Slice then
                        declare
                           Rng : constant Node_Id := Discrete_Range (Actual);
                        begin
                           if Nkind (Rng) = N_Range then
                              LB := Low_Bound (Rng);
                              UB := High_Bound (Rng);
                           else
                              LB := Type_Low_Bound (Etype (Rng));
                              UB := Type_High_Bound (Etype (Rng));
                           end if;
                        end;

                     --  Normal array type, get index subtype bounds

                     else
                        LB := Type_Low_Bound (Etype (First_Index (Atyp)));
                        UB := Type_High_Bound (Etype (First_Index (Atyp)));
                     end if;

                     Cprint_Node (LB);
                     Write_Str (", ");
                     Cprint_Node (UB);
                  end;
               end if;
            end if;
         end;

         Next_Actual (Actual);
         Next_Formal (Formal);
         exit when No (Actual);
         Write_Str (", ");
      end loop;

      Write_Char (')');
   end Cprint_Call;

   --------------------
   -- Cprint_Declare --
   --------------------

   procedure Cprint_Declare
     (Ent : Entity_Id;
      Add_Access : Boolean := False;
      Virtual_OK : Boolean := False)
   is
      Typ : constant Entity_Id := Etype (Ent);

      procedure Add_Star;
      --  Outputs '*' if Add_Access is True, otherwise does nothing

      --------------
      -- Add_Star --
      --------------

      procedure Add_Star is
      begin
         if Add_Access then
            Write_Char ('*');
         end if;
      end Add_Star;

   --  Start of processing for Cprint_Declare

   begin
      if Is_Type (Ent) then
         Write_Str ("typedef ");
      end if;

      --  If we have a typedef and this is not the identity (as occurs in
      --  the enumeration type case), then use the typedef.

      if Has_Typedef (Typ) and then Typ /= Ent then
         Cprint_Node (Typ);
         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent);

      --  Discrete type

      elsif Is_Discrete_Type (Typ) then
         Cprint_Type_Name (Typ, No_TD => Typ = Ent);
         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent);

      --  Access type

      elsif Is_Access_Type (Typ) then
         Cprint_Type_Name (Designated_Type (Typ));
         Write_Str (" *");
         Add_Star;
         Cprint_Node (Ent);

      --  Record type

      elsif Is_Record_Type (Typ) then

         --  For now not tagged

         if Is_Tagged_Type (Typ) then
            raise Program_Error;
         end if;

         Write_Str ("struct {");
         Indent_Begin;

         --  Output record components

         Output_Components : declare
            Decl : constant Node_Id := Declaration_Node (Typ);
            RecD : constant Node_Id := Type_Definition (Decl);

            procedure Output_Component_List (Clist : Node_Id);
            --  Recursive routine to output a component list

            ---------------------------
            -- Output_Component_List --
            ---------------------------

            procedure Output_Component_List (Clist : Node_Id) is
               Comp : Node_Id;
               Var  : Node_Id;

            begin
               --  Output components (ignore types, pragmas etc)

               Comp := First (Component_Items (Clist));
               while Present (Comp) loop
                  if Nkind (Comp) = N_Component_Declaration then
                     Write_Indent;
                     Cprint_Declare (Defining_Identifier (Comp));
                     Write_Char (';');
                  end if;

                  Next (Comp);
               end loop;

               --  Output variant part if present

               if Present (Variant_Part (Clist)) then
                  Write_Indent_Str ("union {");
                  Indent_Begin;

                  Var := First (Variants (Variant_Part (Clist)));
                  while Present (Var) loop
                     declare
                        VCList  : constant Node_Id := Component_List (Var);
                        VCItems : constant List_Id := Component_Items (VCList);

                     begin
                        --  If only one component in this component list, we
                        --  can output it as a single member of the union.

                        if List_Length (VCItems) = 1 then
                           Output_Component_List (VCList);

                        --  Otherwise we have more than one component, so we
                        --  have to introduce a struct.

                        else
                           Write_Indent_Str ("struct {");
                           Indent_Begin;
                           Output_Component_List (VCList);
                           Indent_End;
                           Write_Indent_Str ("};");
                        end if;
                     end;

                     Next (Var);
                  end loop;

                  Indent_End;
                  Write_Indent_Str ("};");
               end if;
            end Output_Component_List;

         --  Start of output for Output_Components

         begin
            --  For now, limit cases we handle

            if Nkind (Decl) /= N_Full_Type_Declaration
              or else Nkind (RecD) /= N_Record_Definition
            then
               raise Program_Error;

            --  Go ahead and output components

            else
               --  Output discriminants

               declare
                  Disc : Node_Id;
               begin
                  if Present (Discriminant_Specifications (Decl)) then
                     Disc := First (Discriminant_Specifications (Decl));
                     while Present (Disc) loop
                        Write_Indent;
                        Cprint_Declare (Defining_Identifier (Disc));
                        Write_Char (';');
                        Next (Disc);
                     end loop;
                  end if;
               end;

               --  Output components

               Output_Component_List (Component_List (RecD));
            end if;
         end Output_Components;

         Indent_End;
         Write_Indent_Str ("} ");
         Add_Star;
         Cprint_Node (Ent);

      --  Array type

      elsif Is_Array_Type (Typ) then
         declare
            Indx : Node_Id;
            LBD  : Node_Id;
            UBD  : Node_Id;

         begin
            Cprint_Type_Name (Component_Type (Typ));
            Write_Char (' ');
            Add_Star;

            Cprint_Node (Ent);

            --  Loop through subscripts

            Indx := First_Index (Typ);
            loop
               Write_Char ('[');

               if Is_Constrained (Typ) or not Virtual_OK then
                  LBD := Type_Low_Bound (Etype (Indx));
                  UBD := Type_High_Bound (Etype (Indx));

                  if Compile_Time_Known_Value (LBD) then
                     if Expr_Value (LBD) = 1 then
                        Cprint_Node (UBD);
                     elsif Expr_Value (LBD) < 1 then
                        Cprint_Sum (UBD, 1 - Expr_Value (LBD), False);
                     else
                        Cprint_Difference (UBD, Expr_Value (LBD) - 1, False);
                     end if;
                  else
                     Cprint_Difference (UBD, LBD, True);
                     Write_Str (" + 1");
                  end if;
               end if;

               Write_Char (']');

               Next_Index (Indx);
               exit when No (Indx);
            end loop;
         end;

      --  For anything else, assume we have typedef reference

      else
         Cprint_Node (Typ);
         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent);
      end if;
   end Cprint_Declare;

   -----------------------
   -- Cprint_Comma_List --
   -----------------------

   procedure Cprint_Comma_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Cprint_Node (Node);
            Next (Node);
            exit when Node = Empty;

            if Last_Char = ';' then
               Delete_Last_Char;
            end if;

            Write_Str (", ");
         end loop;
      end if;
   end Cprint_Comma_List;

   -----------------------
   -- Cprint_Difference --
   -----------------------

   procedure Cprint_Difference (Val1 : Node_Id; Val2 : Uint; B : Boolean) is
   begin
      if Compile_Time_Known_Value (Val1) then
         Write_Uint_Col_Check (Expr_Value (Val1) - Val2);

      elsif Val2 = Uint_0 then
         Cprint_Node (Val1);

      elsif B then
         Write_Str_Col_Check ("(");
         Cprint_Node (Val1);
         Write_Str_Col_Check (" - ");
         Write_Uint_Col_Check (Val2);
         Write_Str_Col_Check (")");

      else
         Cprint_Node (Val1);
         Write_Str_Col_Check (" - ");
         Write_Uint_Col_Check (Val2);
      end if;
   end Cprint_Difference;

   procedure Cprint_Difference (Val1 : Uint; Val2 : Node_Id; B : Boolean) is
   begin
      if Compile_Time_Known_Value (Val2) then
         Write_Uint_Col_Check (Val1 - Expr_Value (Val2));

      elsif B then
         Write_Str_Col_Check ("(");
         Write_Uint_Col_Check (Val1);
         Write_Str_Col_Check (" - ");
         Cprint_Node (Val2);
         Write_Str_Col_Check (")");

      else
         Write_Uint_Col_Check (Val1);
         Write_Str_Col_Check (" - ");
         Cprint_Node (Val2);
      end if;
   end Cprint_Difference;

   procedure Cprint_Difference (Val1, Val2 : Node_Id; B : Boolean) is
   begin
      if Compile_Time_Known_Value (Val2) then
         Cprint_Difference (Val1, Expr_Value (Val2), B);

      elsif Is_Entity_Name (Val1) and then Is_Entity_Name (Val2)
        and then Entity (Val1) = Entity (Val2)
      then
         Write_Str_Col_Check ("0");

      elsif B then
         Write_Str_Col_Check ("(");
         Cprint_Node (Val1);
         Write_Str_Col_Check (" - ");
         Cprint_Node (Val2);
         Write_Str_Col_Check (")");

      else
         Cprint_Node (Val1);
         Write_Str_Col_Check (" - ");
         Cprint_Node (Val2);
      end if;
   end Cprint_Difference;

   --------------------------
   -- Cprint_Indented_List --
   --------------------------

   procedure Cprint_Indented_List (List : List_Id) is
   begin
      Indent_Begin;
      Cprint_Node_List (List);
      Indent_End;
   end Cprint_Indented_List;

   ----------------------
   -- Cprint_Left_Opnd --
   ----------------------

   procedure Cprint_Left_Opnd (N : Node_Id) is
      Opnd : constant Node_Id := Left_Opnd (N);
   begin
      Cprint_Node_Paren (Opnd);
   end Cprint_Left_Opnd;

   -----------------
   -- Cprint_Node --
   -----------------

   procedure Cprint_Node (Node : Node_Id) is
      Save_Dump_Node : constant Node_Id := Dump_Node;

   begin
      if Node = Empty then
         return;
      end if;

      --  Setup current dump node

      Dump_Node := Node;

      --  Select print circuit based on node kind

      case Nkind (Node) is
         when N_Abort_Statement =>
            raise Program_Error;

         when N_Abortable_Part =>
            raise Program_Error;

         when N_Abstract_Subprogram_Declaration =>

            --  Not output in C mode

            null;

         when N_Accept_Alternative =>
            raise Program_Error;

         when N_Accept_Statement =>
            raise Program_Error;

         when N_Access_Definition =>

            --  Ada 2005 (AI-254)

            if Present (Access_To_Subprogram_Definition (Node)) then
               Cprint_Node (Access_To_Subprogram_Definition (Node));
            else
               --  Ada 2005 (AI-231)

               if Null_Exclusion_Present (Node) then
                  Write_Str ("not null ");
               end if;

               Write_Str_Col_Check ("access ");

               if All_Present (Node) then
                  Write_Str ("all ");
               elsif Constant_Present (Node) then
                  Write_Str ("constant ");
               end if;

               Cprint_Node (Subtype_Mark (Node));
            end if;

         when N_Access_Function_Definition =>

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str ("not null ");
            end if;

            Write_Str_Col_Check ("access ");

            if Protected_Present (Node) then
               Write_Str_Col_Check ("protected ");
            end if;

            Write_Str_Col_Check ("function");
            Write_Param_Specs (Node);
            Write_Str_Col_Check (" return ");
            Cprint_Node (Result_Definition (Node));

         when N_Access_Procedure_Definition =>

            --  Ada 2005 (AI-231)

            Write_Str_Col_Check ("void ");
            Write_Param_Specs (Node);

         when N_Access_To_Object_Definition =>
            Write_Str_Col_Check ("access ");

            if All_Present (Node) then
               Write_Str_Col_Check ("all ");
            elsif Constant_Present (Node) then
               Write_Str_Col_Check ("constant ");
            end if;

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str ("not null ");
            end if;

            Cprint_Node (Subtype_Indication (Node));

         when N_Aggregate =>
            if Null_Record_Present (Node) then
               Write_Str_Col_Check ("(null record)");

            else
               Write_Str_Col_Check ("(");

               if Present (Expressions (Node)) then
                  Cprint_Comma_List (Expressions (Node));

                  if Present (Component_Associations (Node))
                    and then not Is_Empty_List (Component_Associations (Node))
                  then
                     Write_Str (", ");
                  end if;
               end if;

               if Present (Component_Associations (Node))
                 and then not Is_Empty_List (Component_Associations (Node))
               then
                  Indent_Begin;

                  declare
                     Nd : Node_Id;

                  begin
                     Nd := First (Component_Associations (Node));

                     loop
                        Write_Indent;
                        Cprint_Node (Nd);
                        Next (Nd);
                        exit when No (Nd);
                        Write_Str (", ");
                     end loop;
                  end;

                  Indent_End;
               end if;

               Write_Char (')');
            end if;

         when N_Allocator =>

            --  For now, just handle case of identifier with no storage pool

            if No (Storage_Pool (Node))
              and then Nkind (Expression (Node)) = N_Identifier
            then
               Write_Str_Col_Check ("malloc(sizeof(");
               Cprint_Type_Name (Etype (Expression (Node)));
               Write_Str ("))");

            --  Not a case we handle

            else
               Write_Str_Col_Check ("new ");
               Cprint_Node (Expression (Node));
            end if;

         when N_And_Then =>
            Cprint_Left_Opnd (Node);
            Write_Str (" && ");
            Cprint_Right_Opnd (Node);

         --  Note: the following code for N_Aspect_Specification is not used,
         --  since we deal with aspects as part of a declaration.

         when N_Aspect_Specification =>
            raise Program_Error;

         when N_Assignment_Statement =>
            declare
               LHS : constant Node_Id := Name (Node);
               RHS : constant Node_Id := Expression (Node);

            begin
               Write_Source_Lines (Node);
               Write_Indent;
               Cprint_Node (LHS);

               --  A special case, if we have X = X +/- 1, convert to the more
               --  natural ++/-- notation in the C output.

               if Is_Entity_Name (LHS)
                 and then Nkind_In (RHS, N_Op_Add, N_Op_Subtract)
                 and then Is_Entity_Name (Left_Opnd (RHS))
                 and then Entity (LHS) = Entity (Left_Opnd (RHS))
                 and then Nkind (Right_Opnd (RHS)) = N_Integer_Literal
                 and then Intval (Right_Opnd (RHS)) = 1
               then
                  if Nkind (RHS) = N_Op_Add then
                     Write_Str ("++");
                  else
                     Write_Str ("--");
                  end if;

               --  Normal case of C assignment

               else
                  Write_Str (" = ");
                  Cprint_Node (RHS);
               end if;

               Write_Char (';');
            end;

         when N_Asynchronous_Select =>
            raise Program_Error;

         when N_At_Clause =>
            Write_Indent_Str ("for ");
            Write_Id (Identifier (Node));
            Write_Str_Col_Check (" use at ");
            Cprint_Node (Expression (Node));
            Write_Char (';');

         when N_Attribute_Definition_Clause =>
            Write_Indent_Str ("for ");
            Cprint_Node (Name (Node));
            Write_Char (''');
            Write_Name_Col_Check (Chars (Node));
            Write_Str_Col_Check (" use ");
            Cprint_Node (Expression (Node));
            Write_Char (';');

         when N_Attribute_Reference =>

            --  Handle only First/Last on unconstrained array formal

            if Nam_In (Attribute_Name (Node), Name_First, Name_Last)
              and then Is_Array_Type (Etype (Prefix (Node)))
              and then not Is_Constrained (Etype (Prefix (Node)))
              and then Is_Formal (Entity (Prefix (Node)))
            then
               Cprint_Node (Entity (Prefix (Node)));

               --  Reference paramF or paramL extra parameters

               if Attribute_Name (Node) = Name_First then
                  Write_Char ('F');
               else
                  Write_Char ('L');
               end if;

            --  Handle Succ and Pred

            elsif Attribute_Name (Node) = Name_Succ then
               Cprint_Sum
                 (First (Expressions (Node)), Uint_1, Parens_Needed (Node));

            elsif Attribute_Name (Node) = Name_Pred then
               Cprint_Difference
                 (First (Expressions (Node)), Uint_1, Parens_Needed (Node));

            --  For other cases, raise PE for now

            else
               raise Program_Error;
            end if;

         when N_Block_Statement =>
            Write_Source_Lines (Sloc (Node));

            declare
               HSS : constant Node_Id := Handled_Statement_Sequence (Node);

            begin
               --  Detect case of dummy block with no declarations and a single
               --  statement. In this case we can omit the block junk.

               if Is_Empty_List (Declarations (Node))
                 and then List_Length (Statements (HSS)) = 1
               then
                  Cprint_Node (First (Statements (HSS)));

               --  Normal case, we need a block

               else
                  Write_Indent_Str ("{");

                  if Present (Declarations (Node)) then
                     Cprint_Indented_List (Declarations (Node));
                     Write_Indent;
                  end if;

                  Cprint_Node (Handled_Statement_Sequence (Node));
                  Write_Indent_Str ("}");
                  Write_Char (';');
               end if;
            end;

         when N_Case_Expression =>

            --  We should not see case expressions in a fully expanded tree,
            --  since they are always replaced by case statements.

            raise Program_Error;

         when N_Case_Expression_Alternative =>
            raise Program_Error;

         when N_Case_Statement =>
            Write_Source_Lines (Sloc (Node), Last_Line (Expression (Node)));

            declare
               Use_If : Boolean := False;
               Alt    : Node_Id;
               Choice : Node_Id;

            begin
               --  First we do a prescan to see if there are any ranges, if
               --  so, we will have to use an if/else translation since the C
               --  switch statement does not accomodate ranges. Note that we do
               --  not have to test the last alternative, since it translates
               --  to a default anyway without any range tests.

               Alt := First (Alternatives (Node));
               Outer : while Present (Next (Alt)) loop
                  Choice := First (Discrete_Choices (Alt));
                  Inner : while Present (Choice) loop
                     if Nkind (Choice) = N_Range
                       or else (Is_Entity_Name (Choice)
                                 and then Is_Type (Entity (Choice)))
                     then
                        Use_If := True;
                        exit Outer;
                     end if;

                     Next (Choice);
                  end loop Inner;

                  Next (Alt);
               end loop Outer;

               --  Case where we have to use if's

               if Use_If then
                  Alt := First (Alternatives (Node));
                  loop
                     Write_Source_Lines
                       (Sloc (Alt), Last_Line (Last (Discrete_Choices (Alt))));

                     --  First alternative, use if

                     if No (Prev (Alt)) then
                        Write_Indent_Str ("if (");

                     --  All but last alternative, use else if

                     elsif Present (Next (Alt)) then
                        Write_Indent_Str ("else if (");

                     --  Last alternative, use else and we are done

                     else
                        Write_Indent_Str ("else {");
                        Cprint_Indented_List (Statements (Alt));
                        Write_Source_Lines
                          (Sloc (Node) +
                             Text_Ptr (UI_To_Int (End_Span (Node))));
                        Write_Indent_Str ("}");
                        exit;
                     end if;

                     Choice := First (Discrete_Choices (Alt));
                     loop
                        --  Simple expression, equality test

                        if Nkind (Choice) /= N_Range
                          and then (not Is_Entity_Name (Choice)
                                     or else not Is_Type (Entity (Choice)))
                        then
                           Cprint_Node (Expression (Node));
                           Write_Str (" == ");
                           Cprint_Node (Choice);

                           --  Range, do range test

                        else
                           declare
                              LBD : Node_Id;
                              HBD : Node_Id;

                           begin
                              if Nkind (Choice) = N_Range then
                                 LBD := Low_Bound (Choice);
                                 HBD := High_Bound (Choice);
                              else
                                 LBD := Type_Low_Bound (Entity (Choice));
                                 HBD := Type_High_Bound (Entity (Choice));
                              end if;

                              Write_Char ('(');
                              Cprint_Node (Expression (Node));
                              Write_Str (" >= ");
                              Write_Uint_Col_Check (Expr_Value (LBD));
                              Write_Str (" && ");
                              Cprint_Node (Expression (Node));
                              Write_Str (" <= ");
                              Write_Uint_Col_Check (Expr_Value (HBD));
                              Write_Char (')');
                           end;
                        end if;

                        if Present (Next (Choice)) then
                           Write_Str_Col_Check (" || ");
                           Next (Choice);
                        else
                           exit;
                        end if;
                     end loop;

                     Write_Str (") {");
                     Cprint_Indented_List (Statements (Alt));
                     Write_Indent_Str ("}");

                     Next (Alt);
                  end loop;

               --  Case where we can use Switch

               else
                  Write_Indent_Str ("switch (");
                  Cprint_Node (Expression (Node));
                  Write_Str (") {");
                  Cprint_Indented_List (Alternatives (Node));
                  Write_Source_Lines
                    (Sloc (Node) + Text_Ptr (UI_To_Int (End_Span (Node))));
                  Write_Indent_Str ("}");
               end if;
            end;

         when N_Case_Statement_Alternative =>
            Write_Source_Lines
              (Sloc (Node), Last_Line (Last (Discrete_Choices (Node))));

            declare
               Choices : constant List_Id := Discrete_Choices (Node);
               Choice  : Node_Id;

            begin
               Choice := First (Choices);
               while Present (Choice) loop
                  if Nkind (Choice) = N_Others_Choice then
                     Write_Indent_Str ("default :");
                  else
                     Write_Indent_Str ("case ");
                     Cprint_Node (Choice);
                     Write_Str (" :");
                  end if;

                  Next (Choice);
               end loop;

               Cprint_Indented_List (Statements (Node));
               Write_Indent_Str ("   break;");
            end;

         when N_Character_Literal =>
            if Column > Sprint_Line_Limit - 2 then
               Write_Indent_Str ("  ");
            end if;

            Write_Char (''');
            Write_Char_Code (UI_To_CC (Char_Literal_Value (Node)));
            Write_Char (''');

         when N_Code_Statement =>
            Write_Source_Lines (Node);

            Write_Indent;
            Cprint_Node (Expression (Node));
            Write_Char (';');

         when N_Compilation_Unit =>
            Cprint_Node_List (Context_Items (Node));
            Cprint_Opt_Node_List (Declarations (Aux_Decls_Node (Node)));

            if Private_Present (Node) then
               Write_Indent_Str ("private ");
            end if;

            Cprint_Node (Unit (Node));

            if Present (Actions (Aux_Decls_Node (Node)))
                 or else
               Present (Pragmas_After (Aux_Decls_Node (Node)))
            then
               Write_Indent;
            end if;

            Cprint_Opt_Node_List (Actions (Aux_Decls_Node (Node)));
            Cprint_Opt_Node_List (Pragmas_After (Aux_Decls_Node (Node)));

         when N_Compilation_Unit_Aux =>
            null; -- nothing to do, never used, see above

         when N_Component_Association =>
            Cprint_Bar_List (Choices (Node));
            Write_Str (" => ");

            --  Ada 2005 (AI-287): Print the box if present

            if Box_Present (Node) then
               Write_Str_Col_Check ("<>");
            else
               Cprint_Node (Expression (Node));
            end if;

         when N_Component_Clause =>
            Write_Indent;
            Cprint_Node (Component_Name (Node));
            Write_Str (" at ");
            Cprint_Node (Position (Node));
            Write_Char (' ');
            Write_Str_Col_Check ("range ");
            Cprint_Node (First_Bit (Node));
            Write_Str (" .. ");
            Cprint_Node (Last_Bit (Node));
            Write_Char (';');

         when N_Component_Definition =>

            --  Ada 2005 (AI-230): Access definition components

            if Present (Access_Definition (Node)) then
               Cprint_Node (Access_Definition (Node));

            elsif Present (Subtype_Indication (Node)) then
               if Aliased_Present (Node) then
                  Write_Str_Col_Check ("aliased ");
               end if;

               --  Ada 2005 (AI-231)

               if Null_Exclusion_Present (Node) then
                  Write_Str (" not null ");
               end if;

               Cprint_Node (Subtype_Indication (Node));

            else
               Write_Str (" ??? ");
            end if;

         when N_Component_Declaration =>
            raise Program_Error;

         when N_Component_List =>
            if Null_Present (Node) then
               Indent_Begin;
               Write_Indent_Str ("null");
               Write_Char (';');
               Indent_End;

            else
               Cprint_Indented_List (Component_Items (Node));
               Cprint_Node (Variant_Part (Node));
            end if;

         when N_Compound_Statement =>
            if Is_Non_Empty_List (Actions (Node)) then
               Write_Char ('(');
               Cprint_Comma_List (Actions (Node));

               if Last_Char = ';' then
                  Delete_Last_Char;
               end if;

               Write_Char (')');
            end if;

         when N_Conditional_Entry_Call =>
            raise Program_Error;

         when N_Constrained_Array_Definition =>
            raise Program_Error;

         when N_Contract =>
            raise Program_Error;

         when N_Decimal_Fixed_Point_Definition =>
            raise Program_Error;

         when N_Defining_Character_Literal =>
            Write_Name_Col_Check (Chars (Node));

         when N_Defining_Identifier =>
            Write_Id (Node);

         when N_Defining_Operator_Symbol =>
            Write_Name_Col_Check (Chars (Node));

         when N_Defining_Program_Unit_Name =>
            Cprint_Node (Name (Node));
            Write_Char ('.');
            Write_Id (Defining_Identifier (Node));

         when N_Delay_Alternative =>
            raise Program_Error; -- should not occur in generated code

         when N_Delay_Relative_Statement =>
            raise Program_Error; -- should not occur in generated code

         when N_Delay_Until_Statement =>
            raise Program_Error; -- should not occur in generated code

         when N_Delta_Constraint =>
            Write_Str_Col_Check ("delta ");
            Cprint_Node (Delta_Expression (Node));
            Cprint_Opt_Node (Range_Constraint (Node));

         when N_Derived_Type_Definition =>
            if Abstract_Present (Node) then
               Write_Str_Col_Check ("abstract ");
            end if;

            Write_Str_Col_Check ("new ");

            --  Ada 2005 (AI-231)

            if Null_Exclusion_Present (Node) then
               Write_Str_Col_Check ("not null ");
            end if;

            Cprint_Node (Subtype_Indication (Node));

            if Present (Interface_List (Node)) then
               Write_Str_Col_Check (" and ");
               Cprint_And_List (Interface_List (Node));
               Write_Str_Col_Check (" with ");
            end if;

            if Present (Record_Extension_Part (Node)) then
               if No (Interface_List (Node)) then
                  Write_Str_Col_Check (" with ");
               end if;

               Cprint_Node (Record_Extension_Part (Node));
            end if;

         when N_Designator =>
            Cprint_Node (Name (Node));
            Write_Char ('.');
            Write_Id (Identifier (Node));

         when N_Digits_Constraint =>
            raise Program_Error;

         when N_Discriminant_Association =>
            if Present (Selector_Names (Node)) then
               Cprint_Bar_List (Selector_Names (Node));
               Write_Str (" => ");
            end if;

            Cprint_Node (Expression (Node));

         when N_Discriminant_Specification =>
            Cprint_Node (Defining_Identifier (Node));
            Write_Str (" : ");

            Cprint_Node (Discriminant_Type (Node));

            if Present (Expression (Node)) then
               Write_Str (" := ");
               Cprint_Node (Expression (Node));
            end if;

         when N_Elsif_Part =>
            Write_Source_Lines (Sloc (Node), Last_Line (Condition (Node)));
            Write_Indent_Str ("else if (");
            Cprint_Node (Condition (Node));
            Write_Char (')');

            Write_Str (" {");
            Cprint_Indented_List (Then_Statements (Node));
            Write_Indent_Str ("}");

         when N_Empty =>
            null;

         when N_Entry_Body =>
            raise Program_Error; -- should not occur in generated code

         when N_Entry_Body_Formal_Part =>
            raise Program_Error; -- should not occur in generated code

         when N_Entry_Call_Alternative =>
            raise Program_Error; -- should not occur in generated code

         when N_Entry_Call_Statement =>
            raise Program_Error; -- should not occur in generated code

         when N_Entry_Declaration =>
            raise Program_Error; -- should not occur in generated code

         when N_Entry_Index_Specification =>
            raise Program_Error; -- should not occur in generated code

         when N_Enumeration_Representation_Clause =>
            null; -- not output in C code

         when N_Enumeration_Type_Definition =>
            null; -- not output in C code

         when N_Error =>
            Write_Str_Col_Check ("<error>");

         when N_Exception_Declaration =>
            null; -- not output in C code

         when N_Exception_Handler =>
            null; -- not output in C code

         when N_Exception_Renaming_Declaration =>
            null; -- not output in C code

         when N_Exit_Statement =>
            Write_Source_Lines (Node);

            if Present (Condition (Node)) then
               Write_Indent_Str ("if (");
               Cprint_Node (Condition (Node));
               Write_Char (')');
               Indent_Begin;
            end if;

            if No (Name (Node)) then
               Write_Indent_Str ("break;");
            else
               Write_Indent_Str ("goto ");
               Cprint_Node (Name (Node));
               Write_Char (';');
            end if;

            if Present (Condition (Node)) then
               Indent_End;
            end if;

         when N_Expanded_Name =>
            Cprint_Node (Prefix (Node));
            Write_Char ('.');
            Cprint_Node (Selector_Name (Node));

         when N_Explicit_Dereference =>
            Write_Char ('*');
            Cprint_Node_Paren (Prefix (Node));

         when N_Expression_With_Actions =>
            if Is_Non_Empty_List (Actions (Node)) then
               Write_Char ('(');
               Cprint_Comma_List (Actions (Node));

               if Last_Char = ';' then
                  Delete_Last_Char;
               end if;

               Write_Str (", ");
               Cprint_Node (Expression (Node));
               Write_Char (')');
            else
               Cprint_Node (Expression (Node));
            end if;

         when N_Expression_Function =>
            Write_Indent;
            Cprint_Node (Specification (Node));
            Write_Str (" is");
            Indent_Begin;
            Write_Indent;
            Cprint_Node (Expression (Node));
            Write_Char (';');
            Indent_End;

         when N_Extended_Return_Statement =>
            Write_Indent_Str ("return ");
            Cprint_Node_List (Return_Object_Declarations (Node));

            if Present (Handled_Statement_Sequence (Node)) then
               Write_Str_Col_Check (" do");
               Cprint_Node (Handled_Statement_Sequence (Node));
               Write_Indent_Str ("end return;");
            else
               Write_Indent_Str (";");
            end if;

         when N_Extension_Aggregate =>
            Write_Str_Col_Check ("(");
            Cprint_Node (Ancestor_Part (Node));
            Write_Str_Col_Check (" with ");

            if Null_Record_Present (Node) then
               Write_Str_Col_Check ("null record");
            else
               if Present (Expressions (Node)) then
                  Cprint_Comma_List (Expressions (Node));

                  if Present (Component_Associations (Node)) then
                     Write_Str (", ");
                  end if;
               end if;

               if Present (Component_Associations (Node)) then
                  Cprint_Comma_List (Component_Associations (Node));
               end if;
            end if;

            Write_Char (')');

         when N_Floating_Point_Definition =>
            null; -- not output in C code

         when N_Formal_Decimal_Fixed_Point_Definition =>
            null; -- not output in C code

         when N_Formal_Derived_Type_Definition =>
            null; -- not output in C code

         when N_Formal_Abstract_Subprogram_Declaration =>
            null; -- not output in C code

         when N_Formal_Concrete_Subprogram_Declaration =>
            null; -- not output in C code

         when N_Formal_Discrete_Type_Definition =>
            null; -- not output in C code

         when N_Formal_Floating_Point_Definition =>
            null; -- not output in C code

         when N_Formal_Modular_Type_Definition =>
            null; -- not output in C code

         when N_Formal_Object_Declaration =>
            null; -- not output in C code

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            null; -- not output in C code

         when N_Formal_Package_Declaration =>
            null; -- not output in C code

         when N_Formal_Private_Type_Definition =>
            null; -- not output in C code

         when N_Formal_Incomplete_Type_Definition =>
            null; -- not output in C code

         when N_Formal_Signed_Integer_Type_Definition =>
            null; -- not output in C code

         when N_Formal_Type_Declaration =>
            null; -- not output in C code

         when N_Free_Statement =>
            Write_Source_Lines (Node);
            Write_Indent_Str ("free ");
            Cprint_Node (Expression (Node));
            Write_Char (';');

         when N_Freeze_Entity =>
            Freeze_Level := Freeze_Level + 1;
            Cprint_Node_List (Actions (Node));
            Freeze_Level := Freeze_Level - 1;

         when N_Freeze_Generic_Entity =>
            null; -- not output in C code

         when N_Full_Type_Declaration =>
            Write_Source_Lines (Node);

            declare
               Typ : constant Entity_Id := Defining_Identifier (Node);

            begin
               --  Output typedef if needed

               if Has_Typedef (Typ) then

                  --  If this is a first subtype, and base type is not the same
                  --  as the first subtype, output a typedef for that as well.

                  if Is_First_Subtype (Typ)
                    and then Base_Type (Typ) /= Typ
                  then
                     Write_Indent;
                     Cprint_Declare (Base_Type (Typ));
                     Write_Char (';');
                  end if;

                  --  Now the typedef for the type itself

                  Write_Indent;
                  Cprint_Declare (Typ);
                  Write_Char (';');
               end if;
            end;

         when N_Function_Call =>
            Write_Subprogram_Name (Name (Node));
            Cprint_Call (Node);

         when N_Function_Instantiation =>
            null; -- not output in C code

         when N_Function_Specification =>
            Cprint_Type_Name (Etype (Result_Definition (Node)));
            Write_Char (' ');
            Cprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);

         when N_Generic_Association =>
            null; -- not output in C code

         when N_Generic_Function_Renaming_Declaration =>
            null; -- not output in C code

         when N_Generic_Package_Declaration =>
            null; -- not output in C code

         when N_Generic_Package_Renaming_Declaration =>
            null; -- not output in C code

         when N_Generic_Procedure_Renaming_Declaration =>
            null; -- not output in C code

         when N_Generic_Subprogram_Declaration =>
            null; -- not output in C code

         when N_Goto_Statement =>
            Write_Source_Lines (Node);
            Write_Indent_Str ("goto ");
            Cprint_Node (Name (Node));
            Write_Char (';');

            if Nkind (Next (Node)) = N_Label then
               Write_Indent;
            end if;

         when N_Handled_Sequence_Of_Statements =>
            Cprint_Indented_List (Statements (Node));

         when N_Identifier =>

            --  If reference to parameter passed by pointer, add deference

            if Present (Entity (Node))
              and then Is_Formal (Entity (Node))
              and then Pass_Pointer (Entity (Node))
            then
               Write_Char ('*');
            end if;

            Write_Id (Node);

         when N_If_Expression =>
            declare
               Condition  : constant Node_Id := First (Expressions (Node));
               Then_Expr  : constant Node_Id := Next (Condition);
               Else_Expr  : constant Node_Id := Next (Then_Expr);
            begin
               Write_Char ('(');
               Cprint_Node (Condition);
               Write_Str (") ? ");
               Cprint_Node_Paren (Then_Expr);
               Write_Str (" : ");
               Cprint_Node_Paren (Else_Expr);
            end;

         when N_If_Statement =>
            Write_Source_Lines (Sloc (Node), Last_Line (Condition (Node)));
            Write_Indent_Str ("if (");
            Cprint_Node (Condition (Node));
            Write_Str_Col_Check (")");

            Write_Str (" {");
            Cprint_Indented_List (Then_Statements (Node));

            if No (Elsif_Parts (Node))
              and then No (Else_Statements (Node))
            then
               Write_Source_Lines
                 (Sloc (Node) + Text_Ptr (UI_To_Int (End_Span (Node))));
            end if;

            Write_Indent_Str ("}");

            Cprint_Opt_Node_List (Elsif_Parts (Node));

            if Present (Else_Statements (Node)) then

               --  Guess where ELSE keyword is

               declare
                  FES : constant Physical_Line_Number :=
                          First_Line (First (Else_Statements (Node)));
               begin
                  Write_Source_Lines (FES - 1, FES - 1);
               end;

               Write_Indent_Str ("else");
               Write_Str (" {");

               Cprint_Indented_List (Else_Statements (Node));

               Write_Source_Lines
                 (Sloc (Node) + Text_Ptr (UI_To_Int (End_Span (Node))));
               Write_Indent_Str ("}");
            end if;

         when N_Implicit_Label_Declaration =>
            null; -- not output in C code

         when N_In =>
            if Present (Right_Opnd (Node)) then
               Cprint_Left_Opnd (Node);
               Write_Str (" >= ");
               Cprint_Node (Low_Bound (Right_Opnd (Node)));
               Write_Str (" && ");
               Cprint_Left_Opnd (Node);
               Write_Str (" <= ");
               Cprint_Node (High_Bound (Right_Opnd (Node)));
            else
               Cprint_Bar_List (Alternatives (Node));
            end if;

         when N_Incomplete_Type_Declaration =>
            null; -- not output in C code

         when N_Index_Or_Discriminant_Constraint =>
            null; -- not output in C code

         when N_Indexed_Component =>
            if Nkind (Prefix (Node)) = N_Explicit_Dereference then
               Write_Str ("(*");
               Cprint_Node_Paren (Prefix (Prefix (Node)));
               Write_Char (')');
            else
               Cprint_Node_Paren (Prefix (Node));
            end if;

            declare
               Sub : Node_Id;
               Ind : Node_Id;
            begin
               Sub := First (Expressions (Node));
               Ind := First_Index (Etype (Prefix (Node)));
               loop
                  Write_Char ('[');
                  Cprint_Difference (Sub, Type_Low_Bound (Etype (Ind)), False);
                  Write_Char (']');
                  Next (Sub);
                  exit when No (Sub);
                  Next_Index (Ind);
               end loop;
            end;

         when N_Integer_Literal =>

            --  Note: do not bother with writing in hex in C output for now

            Write_Uint_Col_Check (Intval (Node));

         when N_Iteration_Scheme =>

            --  Iteration_Scheme was handled as part of loop handling

            raise Program_Error;

         when N_Iterator_Specification =>
            Write_Id (Defining_Identifier (Node));

            if Present (Subtype_Indication (Node)) then
               Write_Str_Col_Check (" : ");
               Cprint_Node (Subtype_Indication (Node));
            end if;

            if Of_Present (Node) then
               Write_Str_Col_Check (" of ");
            else
               Write_Str_Col_Check (" in ");
            end if;

            if Reverse_Present (Node) then
               Write_Str_Col_Check ("reverse ");
            end if;

            Cprint_Node (Name (Node));

         when N_Itype_Reference =>
            null; -- not output in C

         when N_Label =>
            Write_Source_Lines (Node);
            Write_Indent;
            Write_Id (Identifier (Node));
            Write_Char (':');

         when N_Loop_Parameter_Specification =>
            Write_Id (Defining_Identifier (Node));
            Write_Str_Col_Check (" in ");

            if Reverse_Present (Node) then
               Write_Str_Col_Check ("reverse ");
            end if;

            Cprint_Node (Discrete_Subtype_Definition (Node));

         when N_Loop_Statement =>
            declare
               ISS : constant Node_Id := Iteration_Scheme (Node);

               For_Loop_Var : Entity_Id := Empty;
               --  Set to defining identifier of for loop variable for FOR loop

               For_Loop_Reverse : Boolean;
               --  Set True if reverse for loop, False for normal for loop

               Incr : String (1 .. 2) := "++";
               --  Change to "--" if reverse FOR loop

               Use_While : Boolean := False;
               --  Set True if we have the case of a FOR loop that had to be
               --  expanded into a C while loop, and thus needs a statement
               --  adding at the end of the body that inctrement/decrements
               --  the loop variable.

            begin
               --  Handle iteration scheme

               if Present (ISS) then
                  Write_Source_Lines (Sloc (Node), Last_Line (ISS));

                  --  WHILE loop case, generates C while

                  if Present (Condition (ISS)) then
                     Write_Indent_Str ("while (");
                     Cprint_Node (Condition (ISS));
                     Write_Char (')');

                  --  FOR loop case

                  else
                     --  For loops are tricky, consider this example:

                     --     for X in Integer range 1 .. N loop

                     --  Suppose we decide to translate this to C as

                     --     {
                     --       int x;
                     --       for (x = 1; x <= N; x++) {
                     --          loop body
                     --       }
                     --     }

                     --  That seems right, but it does not work in the case
                     --  where N = Integer'Last, since we will increment
                     --  this value before the test, causing overflow. In the
                     --  case where we have that possibility, the required
                     --  translation is:

                     --     {
                     --        int x = 1;
                     --        while (x <= N) {
                     --           loop body
                     --           x++;
                     --        }
                     --     }

                     --  We could use this translation unconditionally, but
                     --  it is likely that the C back end is able to specially
                     --  optimize the for case, so what we do is to see if we
                     --  can reliably generate the for form, and use this
                     --  where possible.

                     declare
                        LPS : constant Node_Id :=
                                Loop_Parameter_Specification (ISS);
                        DSD : constant Node_Id :=
                                Discrete_Subtype_Definition (LPS);
                        LBD : Node_Id;
                        HBD : Node_Id;

                        Comp : String (1 .. 4) := " <= ";
                        --  Change to " >= " if reverse loop

                        Loop_Btype : Entity_Id;
                        --  Base type of type of loop variable

                        OK : Boolean;
                        Lo : Uint;
                        Hi : Uint;
                        --  Parameters for Determine_Range call

                     begin
                        For_Loop_Var := Defining_Identifier (LPS);
                        For_Loop_Reverse := Reverse_Present (LPS);
                        Loop_Btype := Base_Type (Etype (For_Loop_Var));

                        if Nkind (DSD) = N_Range then
                           LBD := Low_Bound (DSD);
                           HBD := High_Bound (DSD);
                        else
                           raise Program_Error;
                        end if;

                        --  Set things up for reverse loop case

                        if For_Loop_Reverse then
                           Incr := "--";
                           Comp := " >= ";

                           declare
                              Temp : constant Node_Id := LBD;
                           begin
                              LBD := HBD;
                              HBD := Temp;
                           end;
                        end if;

                        --  Now see whether we need a while loop

                        Determine_Range
                          (HBD, OK, Lo, Hi, Assume_Valid => True);

                        if For_Loop_Reverse then
                           Use_While :=
                             Lo <= Expr_Value (Type_Low_Bound (Loop_Btype));
                        else
                           Use_While :=
                             Hi >= Expr_Value (Type_High_Bound (Loop_Btype));
                        end if;

                        --  Create outer block defining the for variable

                        Write_Indent_Str ("{");
                        Indent_Begin;
                        Cprint_Type_Name (Etype (For_Loop_Var));
                        Write_Char (' ');
                        Cprint_Node (For_Loop_Var);

                        --  Case of using while loop

                        if Use_While then
                           Write_Str (" = ");
                           Cprint_Node (LBD);
                           Write_Char (';');

                           --  Write while header

                           Write_Indent_Str ("while (");
                           Cprint_Node (For_Loop_Var);
                           Write_Char (' ');
                           Write_Str (Comp);
                           Write_Char (' ');
                           Cprint_Node (HBD);
                           Write_Char (')');

                        --  Case where we can use for loop safely

                        else
                           Write_Char (';');
                           Write_Indent_Str ("for (");
                           Cprint_Node (For_Loop_Var);
                           Write_Str (" = ");
                           Cprint_Node (LBD);
                           Write_Str ("; ");
                           Cprint_Node (For_Loop_Var);
                           Write_Str (Comp);
                           Cprint_Node (HBD);
                           Write_Str ("; ");
                           Cprint_Node (For_Loop_Var);
                           Write_Str (Incr);
                           Write_Char (')');
                        end if;
                     end;
                  end if;

               --  No iteration scheme present

               else
                  Write_Source_Lines (Sloc (Node));
                  Write_Indent_Str ("while (1)");
               end if;

               --  Output the loop body

               Write_Str (" {");
               Indent_Begin;
               Cprint_Node_List (Statements (Node));

               --  Deal with loop closure

               Write_Source_Lines (End_Label (Node));

               --  Add statement to bump loop variable if FOR loop that
               --  needed to be expanded into a while loop.

               if Use_While then
                  Write_Indent;
                  Cprint_Node (For_Loop_Var);
                  Write_Str (Incr);
                  Write_Char (';');
               end if;

               Indent_End;
               Write_Indent_Str ("}");

               --  Close the outer block if FOR case

               if Present (For_Loop_Var) then
                  Indent_End;
                  Write_Indent_Str ("};");
               end if;

               --  Output label at end of loop as possible exit target

               if Present (Identifier (Node))
                 and then not Has_Created_Identifier (Node)
               then
                  Write_Source_Lines (End_Label (Node));
                  Write_Indent;
                  Write_Id (Identifier (Node));
                  Write_Char (':');
               end if;
            end;

         when N_Mod_Clause =>
            Cprint_Node_List (Pragmas_Before (Node));
            Write_Str_Col_Check ("at mod ");
            Cprint_Node (Expression (Node));

         when N_Modular_Type_Definition =>
            raise Program_Error;

         when N_Not_In =>
            if Present (Right_Opnd (Node)) then
               Cprint_Left_Opnd (Node);
               Write_Str ("<");
               Cprint_Node (Low_Bound (Right_Opnd (Node)));
               Write_Str (" && ");
               Cprint_Left_Opnd (Node);
               Write_Str (">");
               Cprint_Node (High_Bound (Right_Opnd (Node)));
            else
               Cprint_Bar_List (Alternatives (Node));
            end if;

         when N_Null =>
            Write_Str_Col_Check ("0");

         when N_Null_Statement =>
            Write_Source_Lines (Node);

            if Comes_From_Source (Node)
              or else not Is_List_Member (Node)
              or else (No (Prev (Node)) and then No (Next (Node)))
            then
               Write_Indent_Str ("{};");
            end if;

         when N_Number_Declaration =>

            --  No need to output anything, since front end already does the
            --  substitutions that we need of the actual constant values.

            null;

         when N_Object_Declaration =>
            Write_Source_Lines (Node);

            declare
               Id : constant Entity_Id := Defining_Identifier (Node);

            begin
               Write_Indent;
               Cprint_Declare (Id);

               --  Add initializer if present

               if Present (Expression (Node)) then
                  Write_Str_Col_Check (" = ");

                  if Is_Discrete_Type
                    (Etype (Defining_Identifier (Node)))
                  then
                     Cprint_Node (Expression (Node));
                  else
                     Write_Char ('{');
                     Cprint_Node (Expression (Node));
                     Write_Char ('}');
                  end if;
               end if;
            end;

            Write_Char (';');

         when N_Object_Renaming_Declaration =>
            null; -- not output in C mode

            --  Ada 2005 (AI-230): Access renamings

            if Present (Access_Definition (Node)) then
               Cprint_Node (Access_Definition (Node));

            elsif Present (Subtype_Mark (Node)) then

               --  Ada 2005 (AI-423): Object renaming with a null exclusion

               if Null_Exclusion_Present (Node) then
                  Write_Str ("not null ");
               end if;

               Cprint_Node (Subtype_Mark (Node));

            else
               Write_Str (" ??? ");
            end if;

            Write_Str_Col_Check (" renames ");
            Cprint_Node (Name (Node));
            Write_Char (';');

         when N_Op_Abs =>
            Write_Str ("abs ");
            Cprint_Right_Opnd (Node);

         when N_Op_Add =>
            Cprint_Left_Opnd (Node);
            Write_Str (" + ");
            Cprint_Right_Opnd (Node);

         when N_Op_And =>
            Cprint_Left_Opnd (Node);
            Write_Str (" & ");
            Cprint_Right_Opnd (Node);

         when N_Op_Concat =>
            Cprint_Left_Opnd (Node);
            Write_Str (" &??? ");
            Cprint_Right_Opnd (Node);

         when N_Op_Divide =>
            Cprint_Left_Opnd (Node);
            Process_TFAI_RR_Flags (Node);
            Write_Str (" / ");
            Cprint_Right_Opnd (Node);

         when N_Op_Eq =>
            Cprint_Left_Opnd (Node);
            Write_Str (" == ");
            Cprint_Right_Opnd (Node);

         when N_Op_Expon =>
            Cprint_Left_Opnd (Node);
            Write_Str (" ** ");
            Cprint_Right_Opnd (Node);

         when N_Op_Ge =>
            Cprint_Left_Opnd (Node);
            Write_Str (" >= ");
            Cprint_Right_Opnd (Node);

         when N_Op_Gt =>
            Cprint_Left_Opnd (Node);
            Write_Str (" > ");
            Cprint_Right_Opnd (Node);

         when N_Op_Le =>
            Cprint_Left_Opnd (Node);
            Write_Str (" <= ");
            Cprint_Right_Opnd (Node);

         when N_Op_Lt =>
            Cprint_Left_Opnd (Node);
            Write_Str (" < ");
            Cprint_Right_Opnd (Node);

         when N_Op_Minus =>
            Write_Str ("-");
            Cprint_Right_Opnd (Node);

         when N_Op_Mod =>
            Cprint_Left_Opnd (Node);
            Write_Str (" % ");
            Cprint_Right_Opnd (Node);

         when N_Op_Multiply =>
            Cprint_Left_Opnd (Node);
            Write_Str (" * ");
            Cprint_Right_Opnd (Node);

         when N_Op_Ne =>
            Cprint_Left_Opnd (Node);
            Write_Str (" != ");
            Cprint_Right_Opnd (Node);

         when N_Op_Not =>
            if Is_Boolean_Type (Etype (Node)) then
               Write_Str ("!");
            elsif Is_Modular_Integer_Type (Etype (Node)) then
               Write_Str ("~");
            else
               Write_Str ("???");
            end if;

            Cprint_Right_Opnd (Node);

         when N_Op_Or =>
            Cprint_Left_Opnd (Node);
            Write_Str (" | ");
            Cprint_Right_Opnd (Node);

         when N_Op_Plus =>
            Write_Str ("+");
            Cprint_Right_Opnd (Node);

         when N_Op_Rem =>
            Cprint_Left_Opnd (Node);
            Write_Str (" % ");
            Cprint_Right_Opnd (Node);

         when N_Op_Rotate_Left =>

            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Rotate_Right =>

            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Right =>
            Cprint_Left_Opnd (Node);
            Write_Str (" >> ");
            Cprint_Right_Opnd (Node);

         when N_Op_Shift_Right_Arithmetic =>

            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Left =>
            Cprint_Left_Opnd (Node);
            Write_Str (" << ");
            Cprint_Right_Opnd (Node);

         when N_Op_Subtract =>
            Cprint_Left_Opnd (Node);
            Write_Str (" - ");
            Cprint_Right_Opnd (Node);

         when N_Op_Xor =>
            Cprint_Left_Opnd (Node);
            Write_Str (" ^ ");
            Cprint_Right_Opnd (Node);

         when N_Operator_Symbol =>
            Write_Name_Col_Check (Chars (Node));

         when N_Ordinary_Fixed_Point_Definition =>
            Write_Str_Col_Check ("delta ");
            Cprint_Node (Delta_Expression (Node));
            Cprint_Opt_Node (Real_Range_Specification (Node));

         when N_Or_Else =>
            Cprint_Left_Opnd (Node);
            Write_Str (" || ");
            Cprint_Right_Opnd (Node);

         when N_Others_Choice =>
            raise Program_Error;

         when N_Package_Body =>
            Cprint_Node_List (Declarations (Node));

            Extra_Blank_Line;
            Write_Indent_Str ("void ");
            Cprint_Node (Defining_Unit_Name (Node));
            Write_Str ("_ELABB() {");

            if Present (Handled_Statement_Sequence (Node)) then
               Cprint_Node (Handled_Statement_Sequence (Node));
            end if;

            Write_Indent_Str ("}");

         when N_Package_Body_Stub =>
            Write_Indent_Str ("package body ");
            Cprint_Node (Defining_Identifier (Node));
            Write_Str_Col_Check (" is separate;");

         when N_Package_Declaration =>
            Extra_Blank_Line;
            Write_Indent;
            Cprint_Node (Specification (Node));

         when N_Package_Instantiation =>
            Extra_Blank_Line;
            Write_Indent_Str ("package ");
            Cprint_Node (Defining_Unit_Name (Node));
            Write_Str (" is new ");
            Cprint_Node (Name (Node));
            Cprint_Opt_Paren_Comma_List (Generic_Associations (Node));
            Write_Char (';');

         when N_Package_Renaming_Declaration =>
            Write_Indent_Str ("package ");
            Cprint_Node (Defining_Unit_Name (Node));
            Write_Str_Col_Check (" renames ");
            Cprint_Node (Name (Node));
            Write_Char (';');

         when N_Package_Specification =>
            Extra_Blank_Line;
            Write_Source_Lines (Node);

            Cprint_Node_List (Visible_Declarations (Node));

            if Present (Private_Declarations (Node)) then
               Cprint_Node_List (Private_Declarations (Node));
            end if;

            Write_Indent_Str ("void ");
            Cprint_Node (Defining_Unit_Name (Node));
            Write_Str ("_ELABS() {");
            Write_Indent_Str ("}");

         when N_Parameter_Association =>
            raise Program_Error;

         when N_Parameter_Specification =>
            declare
               Ent : constant Entity_Id := Defining_Identifier (Node);
               Typ : constant Entity_Id := Etype (Ent);

            begin
               Cprint_Declare
                 (Ent,
                  Add_Access => Pass_Pointer (Ent),
                  Virtual_OK => True);

               --  Add extra params nameF and nameL for unconstrained array to
               --  hold first/last bounds. For now, just use int for bounds ???

               if Is_Array_Type (Typ)
                 and then not Is_Constrained (Typ)
               then
                  Write_Str (", int ");
                  Cprint_Node (Ent);
                  Write_Char ('F');
                  Write_Str (", int ");
                  Cprint_Node (Ent);
                  Write_Char ('L');
               end if;
            end;

         when N_Pop_Constraint_Error_Label =>
            null;

         when N_Pop_Program_Error_Label =>
            null;

         when N_Pop_Storage_Error_Label =>
            null;

         when N_Private_Extension_Declaration =>
            Write_Indent_Str ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Write_Discr_Specs (Node);
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_Col_Check ("(<>)");
            end if;

            Write_Str_Col_Check (" is new ");
            Cprint_Node (Subtype_Indication (Node));

            if Present (Interface_List (Node)) then
               Write_Str_Col_Check (" and ");
               Cprint_And_List (Interface_List (Node));
            end if;

            Write_Str_Col_Check (" with private;");

         when N_Private_Type_Declaration =>
            Write_Indent_Str ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Write_Discr_Specs (Node);
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_Col_Check ("(<>)");
            end if;

            Write_Str (" is ");

            if Tagged_Present (Node) then
               Write_Str_Col_Check ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_Str_Col_Check ("limited ");
            end if;

            Write_Str_Col_Check ("private;");

         when N_Push_Constraint_Error_Label =>
            null;

         when N_Push_Program_Error_Label =>
            null;

         when N_Push_Storage_Error_Label =>
            null;

         when N_Pragma =>

            --  We only output pragma Comment and we don't even do that if
            --  we are printing the full source, since there is no point.

            if Pragma_Name (Node) = Name_Comment
              and then Is_Non_Empty_List (Pragma_Argument_Associations (Node))
              and then not Dump_Source_Text
            then
               --  Blank line, unless another Comment pragma precedes

               if not Is_List_Member (Node)
                 or else No (Prev (Node))
                 or else Nkind (Prev (Node)) /= N_Pragma
                 or else Pragma_Name (Prev (Node)) /= Name_Comment
               then
                  Write_Eol;
               end if;

               Write_Indent_Str ("/* ");
               String_To_Name_Buffer
                 (Strval
                   (Expression (First (Pragma_Argument_Associations (Node)))));
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Str (" */");

               --  Blank line unless another Comment pragma follows

               if not Is_List_Member (Node)
                 or else No (Next (Node))
                 or else Nkind (Next (Node)) /= N_Pragma
                 or else Pragma_Name (Next (Node)) /= Name_Comment
               then
                  Write_Eol;
               end if;
            end if;

         when N_Pragma_Argument_Association =>
            raise Program_Error;

         when N_Procedure_Call_Statement =>
            Write_Source_Lines (Node);
            Write_Indent;
            Cprint_Call (Node);
            Write_Char (';');

         when N_Procedure_Instantiation =>
            null;

         when N_Procedure_Specification =>
            Write_Source_Lines (Node);
            Write_Str_Col_Check ("void ");
            Cprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);

         when N_Protected_Body =>
            raise Program_Error;

         when N_Protected_Body_Stub =>
            Write_Indent_Str ("protected body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_Col_Check (" is separate;");

         when N_Protected_Definition =>
            raise Program_Error;

         when N_Protected_Type_Declaration =>
            Write_Indent_Str ("protected type ");
            Cprint_Node (Defining_Identifier (Node));
            Write_Discr_Specs (Node);

            if Present (Interface_List (Node)) then
               Write_Str (" is new ");
               Cprint_And_List (Interface_List (Node));
               Write_Str (" with ");
            else
               Write_Str (" is");
            end if;

            Cprint_Node (Protected_Definition (Node));
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Qualified_Expression =>

            --  At the C level, we can ignore the qualification

            Cprint_Node (Expression (Node));

         when N_Quantified_Expression =>
            Write_Str (" for");

            if All_Present (Node) then
               Write_Str (" all ");
            else
               Write_Str (" some ");
            end if;

            if Present (Iterator_Specification (Node)) then
               Cprint_Node (Iterator_Specification (Node));
            else
               Cprint_Node (Loop_Parameter_Specification (Node));
            end if;

            Write_Str (" => ");
            Cprint_Node (Condition (Node));

         when N_Raise_Expression =>
            declare
               Has_Parens : constant Boolean := Paren_Count (Node) > 0;

            begin
               --  The syntax for raise_expression does not include parentheses
               --  but sometimes parentheses are required, so unconditionally
               --  generate them here unless already present.

               if not Has_Parens then
                  Write_Char ('(');
               end if;

               Write_Str_Col_Check ("raise ");
               Cprint_Node (Name (Node));

               if Present (Expression (Node)) then
                  Write_Str_Col_Check (" with ");
                  Cprint_Node (Expression (Node));
               end if;

               if not Has_Parens then
                  Write_Char (')');
               end if;
            end;

         when N_Raise_Constraint_Error =>

            --  We generate nothing for this, usually we should not even
            --  generate these in C mode, since we suppress checks.

            null;

         when N_Raise_Program_Error =>

            --  We generate nothing for this, usually we should not even
            --  generate these in C mode, since we suppress checks.

            null;

         when N_Raise_Storage_Error =>

            --  We generate nothing for this, usually we should not even
            --  generate these in C mode, since we suppress checks.

            null;

         when N_Raise_Statement =>
            Write_Source_Lines (Node);
            Write_Indent_Str ("raise ");
            Cprint_Node (Name (Node));
            Write_Char (';');

         when N_Range =>
            raise Program_Error;

         when N_Range_Constraint =>
            raise Program_Error;

         when N_Real_Literal =>
            Write_Ureal_Col_Check (Realval (Node));

         when N_Real_Range_Specification =>
            raise Program_Error;

         when N_Record_Definition =>
            raise Program_Error;

         when N_Record_Representation_Clause =>
            Write_Indent_Str ("for ");
            Cprint_Node (Identifier (Node));
            Write_Str_Col_Check (" use record ");

            if Present (Mod_Clause (Node)) then
               Cprint_Node (Mod_Clause (Node));
            end if;

            Cprint_Indented_List (Component_Clauses (Node));
            Write_Indent_Str ("end record;");

         when N_Reference =>
            Write_Char ('&');
            Cprint_Node_Paren (Prefix (Node));

         when N_Requeue_Statement =>
            raise Program_Error;

         when N_SCIL_Dispatch_Table_Tag_Init =>
            raise Program_Error;

         when N_SCIL_Dispatching_Call =>
            raise Program_Error;

         when N_SCIL_Membership_Test =>
            raise Program_Error;

         when N_Simple_Return_Statement =>
            Write_Source_Lines (Node);

            if Present (Expression (Node)) then
               Write_Indent_Str ("return (");
               Cprint_Node (Expression (Node));
               Write_Str (");");
            else
               Write_Indent_Str ("return;");
            end if;

         when N_Selected_Component =>

            --  If reference to parameter passed by pointer, use -> notation

            if Is_Entity_Name (Prefix (Node))
              and then Is_Formal (Entity (Prefix (Node)))
              and then Pass_Pointer (Entity (Prefix (Node)))
            then
               --  For a->b, call Write_Id directly, we don't want Write_Node
               --  adding a star, this is a special case for handling params.

               Write_Id (Entity (Prefix (Node)));
               Write_Str ("->");

            --  Also use -> if prefix is explicit dereference

            elsif Nkind (Prefix (Node)) = N_Explicit_Dereference then
               Cprint_Node_Paren (Prefix (Prefix (Node)));
               Write_Str ("->");

            --  Normal case of using a.b

            else
               Cprint_Node_Paren (Prefix (Node));
               Write_Char ('.');
            end if;

            Cprint_Node (Selector_Name (Node));

         when N_Selective_Accept =>
            raise Program_Error;

         when N_Signed_Integer_Type_Definition =>
            raise Program_Error;

         when N_Single_Protected_Declaration =>
            Write_Indent_Str ("protected ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is");
            Cprint_Node (Protected_Definition (Node));
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Single_Task_Declaration =>
            Write_Indent_Str ("task ");
            Cprint_Node (Defining_Identifier (Node));

            if Present (Task_Definition (Node)) then
               Write_Str (" is");
               Cprint_Node (Task_Definition (Node));
            end if;

            Write_Char (';');

         when N_Slice =>
            declare
               Typ : constant Entity_Id := Etype (Prefix (Node));
               Itp : constant Entity_Id := Etype (First_Index (Typ));
               Lbd : constant Node_Id   := Type_Low_Bound (Itp);
               Rng : constant Node_Id   := Discrete_Range (Node);
               Lo  : Node_Id;

            begin
               --  We generate &arr[slice-low-bound - index-low-bound]

               if Nkind (Rng) = N_Range then
                  Lo := Low_Bound (Rng);
               else
                  Lo := Type_Low_Bound (Etype (Rng));
               end if;

               --  Omit & if prefix is a pointer to an array

               if Nkind (Prefix (Node)) = N_Explicit_Dereference then
                  Cprint_Node_Paren (Prefix (Prefix (Node)));

               --  Normal case of an array, where we need the &

               else
                  Write_Char ('&');
                  Cprint_Node_Paren (Prefix (Node));
               end if;

               Write_Char ('[');
               Cprint_Difference (Lo, Lbd, False);
               Write_Char (']');
            end;

         when N_String_Literal =>

            --  This test for line overflow is not quite right because of the
            --  business of escaping back slashes, but it's near enough!

            if String_Length (Strval (Node)) + Column > Sprint_Line_Limit then
               Write_Indent_Str ("  ");
            end if;

            --  Output string literal

            Write_Char ('"');

            declare
               Str : constant String_Id := Strval (Node);

            begin
               for J in 1 .. String_Length (Str) loop
                  declare
                     CC : constant Char_Code := Get_String_Char (Str, J);
                     C  : Character;

                  begin
                     --  For now, output wide characters simply as ?

                     if CC > 255 then
                        Write_Char ('?');

                     --  Characters in range 0 .. 255, output with appropriate
                     --  C escape sequence where needed.

                     else
                        C := Character'Val (CC);

                        case C is
                           when '\' | '?' | '"' | ''' =>
                              Write_Char ('\');
                              Write_Char (C);

                           when ASCII.BS =>
                              Write_Str ("\b");

                           when ASCII.FF =>
                              Write_Str ("\f");

                           when ASCII.LF =>
                              Write_Str ("\n");

                           when ASCII.CR =>
                              Write_Str ("\r");

                           when ASCII.HT =>
                              Write_Str ("\t");

                           when ASCII.VT =>
                              Write_Str ("\v");

                           when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'    |
                                '!' | '#' | '$' | '%' | '&' | ':' | ';' |
                                '@' | '[' | ']' | '^' | '_' | '`' | '{' |
                                '|' | '}' | '~'                         =>
                              Write_Char (C);

                           when others =>
                              declare
                                 Hex : constant array (Char_Code range 0 .. 15)
                                         of Character := "0123456789abcdef";
                              begin
                                 Write_Str ("\x");
                                 Write_Char (Hex (CC / 16));
                                 Write_Char (Hex (CC mod 16));
                              end;
                        end case;
                     end if;
                  end;
               end loop;
            end;

            Write_Char ('"');

         when N_Subprogram_Body =>

            --  Skip writing of discriminant check function

            if Is_Discriminant_Check_Function
                 (Defining_Unit_Name (Specification (Node)))
            then
               null;

            --  Otherwise write subprogram body

            else
               Extra_Blank_Line;
               Write_Source_Lines (Specification (Node));
               Write_Indent;
               Cprint_Node (Specification (Node));

               Write_Str (" {");

               if Is_Non_Empty_List (Declarations (Node)) then
                  Cprint_Indented_List (Declarations (Node));
               end if;

               Cprint_Node (Handled_Statement_Sequence (Node));
               Write_Indent_Str ("};");
            end if;

         when N_Subprogram_Body_Stub =>
            Write_Indent;
            Cprint_Node (Specification (Node));
            Write_Str_Col_Check (" is separate;");

         when N_Subprogram_Declaration =>

            --  Don't print out intrinsics, since calls will be expanded

            if Convention (Defining_Unit_Name (Specification (Node))) /=
                                                 Convention_Intrinsic
            then
               Write_Indent;
               Cprint_Node (Specification (Node));

               if Nkind (Specification (Node)) = N_Procedure_Specification
                 and then Null_Present (Specification (Node))
               then
                  Write_Str_Col_Check (" is null???");
               end if;

               Write_Char (';');
            end if;

         when N_Subprogram_Renaming_Declaration =>
            Write_Indent;
            Cprint_Node (Specification (Node));
            Write_Str_Col_Check (" renames ");
            Cprint_Node (Name (Node));
            Write_Char (';');

         when N_Subtype_Declaration =>

            --  We output a typedef for subtypes that come from source. Any
            --  other subtypes are handled by generating anonymous C types.

            if Comes_From_Source (Defining_Identifier (Node)) then
               Write_Source_Lines (Node);
               Write_Indent;
               Cprint_Declare (Defining_Identifier (Node));
               Write_Char (';');
            end if;

         when N_Subtype_Indication =>

            --  Should have been handled higher up in tree

            raise Program_Error;

         when N_Subunit =>
            Write_Indent_Str ("separate (");
            Cprint_Node (Name (Node));
            Write_Char (')');
            Extra_Blank_Line;
            Cprint_Node (Proper_Body (Node));

         when N_Task_Body =>
            raise Program_Error;

         when N_Task_Body_Stub =>
            Write_Indent_Str ("task body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_Col_Check (" is separate;");

         when N_Task_Definition =>
            raise Program_Error;

         when N_Task_Type_Declaration =>
            Write_Indent_Str ("task type ");
            Cprint_Node (Defining_Identifier (Node));
            Write_Discr_Specs (Node);

            if Present (Interface_List (Node)) then
               Write_Str (" is new ");
               Cprint_And_List (Interface_List (Node));
            end if;

            if Present (Task_Definition (Node)) then
               if No (Interface_List (Node)) then
                  Write_Str (" is");
               else
                  Write_Str (" with ");
               end if;

               Cprint_Node (Task_Definition (Node));
            end if;

            Write_Char (';');

         when N_Terminate_Alternative =>
            raise Program_Error;

         when N_Timed_Entry_Call =>
            raise Program_Error;

         when N_Triggering_Alternative =>
            raise Program_Error;

         when N_Type_Conversion =>
            Write_Char ('(');
            Cprint_Type_Name (Entity (Subtype_Mark (Node)));
            Write_Char (')');
            Cprint_Node_Paren (Expression (Node));

         when N_Unchecked_Expression =>
            Col_Check (10);
            Write_Str ("`(");
            Cprint_Node (Expression (Node));
            Write_Char (')');

         when N_Unchecked_Type_Conversion =>
            Write_Char ('(');
            Cprint_Type_Name (Entity (Subtype_Mark (Node)));
            Write_Char (')');
            Cprint_Node_Paren (Expression (Node));

         when N_Unconstrained_Array_Definition =>
            raise Program_Error;

         when N_Unused_At_Start | N_Unused_At_End =>
            raise Program_Error;

         when N_Use_Package_Clause =>
            null;

         when N_Use_Type_Clause =>
            null;

         when N_Validate_Unchecked_Conversion =>
            null;

         when N_Variant =>
            raise Program_Error;

         when N_Variant_Part =>
            raise Program_Error;

         when N_With_Clause =>

            --  Ada 2005 (AI-50217): Print limited with_clauses

            if Private_Present (Node) and Limited_Present (Node) then
               Write_Indent_Str ("limited private with ");

            elsif Private_Present (Node) then
               Write_Indent_Str ("private with ");

            elsif Limited_Present (Node) then
               Write_Indent_Str ("limited with ");

            else
               Write_Indent_Str ("with ");
            end if;

            Cprint_Node (Name (Node));
            Write_Char (';');

      end case;

      Dump_Node := Save_Dump_Node;
   end Cprint_Node;

   ----------------------
   -- Cprint_Node_List --
   ----------------------

   procedure Cprint_Node_List (List : List_Id; New_Lines : Boolean := False) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Cprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;

      if New_Lines and then Column /= 1 then
         Write_Eol;
      end if;
   end Cprint_Node_List;

   -----------------------
   -- Cprint_Node_Paren --
   -----------------------

   procedure Cprint_Node_Paren (N : Node_Id) is
   begin
      --  Add parens if we have an operator or short circuit operation. But
      --  don't add the parens if already parenthesized, since we will get
      --  then anyway and don't add if definitely not needed

      if (Nkind (N) in N_Op
           or else Nkind_In (N, N_And_Then,
                                N_Explicit_Dereference,
                                N_If_Expression,
                                N_In,
                                N_Not_In,
                             N_Or_Else))
        and then Parens_Needed (N)
      then
         Write_Char ('(');
         Cprint_Node (N);
         Write_Char (')');
      else
         Cprint_Node (N);
      end if;
   end Cprint_Node_Paren;

   ---------------------
   -- Cprint_Opt_Node --
   ---------------------

   procedure Cprint_Opt_Node (Node : Node_Id) is
   begin
      if Present (Node) then
         Write_Char (' ');
         Cprint_Node (Node);
      end if;
   end Cprint_Opt_Node;

   --------------------------
   -- Cprint_Opt_Node_List --
   --------------------------

   procedure Cprint_Opt_Node_List (List : List_Id) is
   begin
      if Present (List) then
         Cprint_Node_List (List);
      end if;
   end Cprint_Opt_Node_List;

   ---------------------------------
   -- Cprint_Opt_Paren_Comma_List --
   ---------------------------------

   procedure Cprint_Opt_Paren_Comma_List (List : List_Id) is
   begin
      if Is_Non_Empty_List (List) then
         Write_Char (' ');
         Cprint_Paren_Comma_List (List);
      end if;
   end Cprint_Opt_Paren_Comma_List;

   -----------------------------
   -- Cprint_Paren_Comma_List --
   -----------------------------

   procedure Cprint_Paren_Comma_List (List : List_Id) is
   begin
      if Is_Non_Empty_List (List) then
         Write_Str_Col_Check ("(");
         Cprint_Comma_List (List);
         Write_Char (')');
      end if;
   end Cprint_Paren_Comma_List;

   -----------------------
   -- Cprint_Right_Opnd --
   -----------------------

   procedure Cprint_Right_Opnd (N : Node_Id) is
      Opnd : constant Node_Id := Right_Opnd (N);
   begin
      Cprint_Node_Paren (Opnd);
   end Cprint_Right_Opnd;

   ----------------
   -- Cprint_Sum --
   ----------------

   procedure Cprint_Sum (Val1 : Node_Id; Val2 : Uint; B : Boolean) is
   begin
      if Compile_Time_Known_Value (Val1) then
         Write_Uint_Col_Check (Expr_Value (Val1) + Val2);

      elsif Val2 = 0 then
         Cprint_Node (Val1);

      elsif B then
         Write_Str_Col_Check ("(");
         Cprint_Node (Val1);
         Write_Str_Col_Check (" + ");
         Write_Uint_Col_Check (Val2);
         Write_Str_Col_Check (")");

      else
         Cprint_Node (Val1);
         Write_Str_Col_Check (" + ");
         Write_Uint_Col_Check (Val2);
      end if;
   end Cprint_Sum;

   procedure Cprint_Sum (Val1 : Uint; Val2 : Node_Id; B : Boolean) is
   begin
      if Compile_Time_Known_Value (Val2) then
         Write_Uint_Col_Check (Val1 + Expr_Value (Val2));

      elsif Val1 = 0 then
         Cprint_Node (Val2);

      elsif B then
         Write_Str_Col_Check ("(");
         Write_Uint_Col_Check (Val1);
         Write_Str_Col_Check (" + ");
         Cprint_Node (Val2);
         Write_Str_Col_Check (")");

      else
         Write_Uint_Col_Check (Val1);
         Write_Str_Col_Check (" + ");
         Cprint_Node (Val2);
      end if;
   end Cprint_Sum;

   procedure Cprint_Sum (Val1, Val2 : Node_Id; B : Boolean) is
   begin
      if Compile_Time_Known_Value (Val2) then
         Cprint_Sum (Val1, Expr_Value (Val2), B);

      elsif B then
         Write_Str_Col_Check ("(");
         Cprint_Node (Val1);
         Write_Str_Col_Check (" + ");
         Cprint_Node (Val2);
         Write_Str_Col_Check (")");

      else
         Cprint_Node (Val1);
         Write_Str_Col_Check (" + ");
         Cprint_Node (Val2);
      end if;
   end Cprint_Sum;

   ----------------------
   -- Cprint_Type_Name --
   ----------------------

   procedure Cprint_Type_Name (Typ : Entity_Id; No_TD : Boolean := False) is
   begin
      --  Print typedef name if available unless inhibited

      if Has_Typedef (Typ) and then not No_TD then
         Cprint_Node (Typ);

      --  Discrete types

      elsif Is_Discrete_Type (Typ) then
         if Typ = Standard_Character then
            Write_Str_Col_Check ("character");

         elsif Is_Unsigned_Type (Typ) then
            if Esize (Typ) = 8 then
               Write_Str_Col_Check ("unsigned_8");
            elsif Esize (Typ) = 16 then
               Write_Str_Col_Check ("unsigned_16");
            elsif Esize (Typ) = 32 then
               Write_Str_Col_Check ("unsigned");
            elsif Esize (Typ) = 64 then
               Write_Str_Col_Check ("long long unsigned");
            else
               Write_Str_Col_Check ("???");
            end if;

         else
            if Esize (Typ) = 8 then
               Write_Str_Col_Check ("short_short_integer");
            elsif Esize (Typ) = 16 then
               Write_Str_Col_Check ("short_integer");
            elsif Esize (Typ) = 32 then
               Write_Str_Col_Check ("integer");
            elsif Esize (Typ) = 64 then
               Write_Str_Col_Check ("long long");
            else
               Write_Str_Col_Check ("???");
            end if;
         end if;

      --  One dimensional unconstrained array type

      elsif Is_Array_Type (Typ)
        and then Number_Dimensions (Typ) = 1
        and then not Is_Constrained (Typ)
      then
         Cprint_Type_Name (Component_Type (Typ));
         Write_Char ('*');

      --  Constrained array type

      elsif Is_Array_Type (Typ) and then Is_Constrained (Typ) then
         declare
            Indx : Node_Id;
            LBD  : Node_Id;
            UBD  : Node_Id;

         begin
            Cprint_Type_Name (Component_Type (Typ));

            --  Loop through subscripts

            Indx := First_Index (Typ);
            loop
               Write_Char ('[');
               LBD := Type_Low_Bound (Etype (Indx));
               UBD := Type_High_Bound (Etype (Indx));

               if Compile_Time_Known_Value (LBD) then
                  if Expr_Value (LBD) = 1 then
                     Cprint_Node (UBD);
                  else
                     Cprint_Difference (UBD, Expr_Value (LBD) - 1, False);
                  end if;
               else
                  Cprint_Difference (UBD, LBD, True);
                  Write_Str (" + 1");
               end if;

               Write_Char (']');
               Next_Index (Indx);
               exit when No (Indx);
            end loop;
         end;

      --  Access type

      elsif Is_Access_Type (Typ)
        and then Is_Discrete_Type (Designated_Type (Typ))
      then
         Write_Char ('*');
         Cprint_Type_Name (Designated_Type (Typ));

      --  Otherwise assume we have typedef reference

      else
         Cprint_Node (Typ);
      end if;
   end Cprint_Type_Name;

   ----------------------
   -- Extra_Blank_Line --
   ----------------------

   procedure Extra_Blank_Line is
   begin
      if not Dump_Source_Text and then Last_Char /= '{' then
         Write_Eol;

         for J in 1 .. Indent loop
            Write_Char (' ');
         end loop;
      end if;
   end Extra_Blank_Line;

   ----------------
   -- First_Line --
   ----------------

   function First_Line (N : Node_Id) return Physical_Line_Number is
   begin
      Get_First_Last_Line (N);
      return FLCache_FL;
   end First_Line;

   -------------------------
   -- Get_First_Last_Line --
   -------------------------

   procedure Get_First_Last_Line (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      First_Sloc : Source_Ptr;
      Last_Sloc  : Source_Ptr;

      function Process (N : Node_Id) return Traverse_Result;
      --  Process function for traversal

      procedure Traverse is new Traverse_Proc (Process);

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
         Loc : constant Source_Ptr := Sloc (N);

      begin
         if Loc > No_Location then
            if First_Sloc = No_Location or else Loc < First_Sloc then
               First_Sloc := Loc;
            end if;

            if Last_Sloc = No_Location or else Loc > Last_Sloc then
               Last_Sloc := Loc;
            end if;
         end if;

         return OK;
      end Process;

   --  Start of processing for Get_First_Last_Line

   begin
      --  Nothing to do if this is cached value

      if N = FLCache_N then
         return;
      else
         FLCache_N := N;
      end if;

      --  If not from current source file, or no source location available,
      --  then set no line number results

      if Loc <= No_Location
        or else Get_Source_File_Index (Loc) /= Current_Source_File
      then
         FLCache_FL := No_Physical_Line_Number;
         FLCache_LL := No_Physical_Line_Number;
         return;
      end if;

      --  Otherwise do the traversal

      First_Sloc := No_Location;
      Last_Sloc  := No_Location;
      Traverse (N);

      if First_Sloc = No_Location then
         FLCache_FL := No_Physical_Line_Number;
      else
         FLCache_FL := Get_Physical_Line_Number (First_Sloc);
      end if;

      if Last_Sloc = No_Location then
         FLCache_LL := No_Physical_Line_Number;
      else
         FLCache_LL := Get_Physical_Line_Number (Last_Sloc);
      end if;

      FLCache_N := N;
   end Get_First_Last_Line;

   -----------------
   -- Has_Typedef --
   -----------------

   function Has_Typedef (Typ : Entity_Id) return Boolean is
   begin
      return Comes_From_Source (Typ)
        or else (Is_Base_Type (Typ)
                 and then Comes_From_Source (First_Subtype (Typ)));
   end Has_Typedef;

   ------------------
   -- Indent_Begin --
   ------------------

   procedure Indent_Begin is
   begin
      Indent := Indent + 3;
   end Indent_Begin;

   ----------------
   -- Indent_End --
   ----------------

   procedure Indent_End is
   begin
      Indent := Indent - 3;
   end Indent_End;

   ---------------
   -- Last_Line --
   ---------------

   function Last_Line (N : Node_Id) return Physical_Line_Number is
   begin
      Get_First_Last_Line (N);
      return FLCache_LL;
   end Last_Line;

   -------------------
   -- Parens_Needed --
   -------------------

   function Parens_Needed (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);
   begin
      if Nkind (P) = N_Assignment_Statement then
         return N /= Expression (P);
      else
         return True;
      end if;
   end Parens_Needed;

   ------------------
   -- Pass_Pointer --
   ------------------

   function Pass_Pointer (Ent : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Ent);
   begin
      if Is_Array_Type (Typ) then
         return False;
      elsif Ekind_In (Ent, E_In_Out_Parameter, E_Out_Parameter) then
         return True;
      elsif Is_Array_Type (Typ) and then Is_Formal (Ent) then
         return True;
      elsif Is_Record_Type (Typ) then
         return not C_Pass_By_Copy (Typ);
      else
         return False;
      end if;
   end Pass_Pointer;

   ---------------------------
   -- Process_TFAI_RR_Flags --
   ---------------------------

   procedure Process_TFAI_RR_Flags (Nod : Node_Id) is
   begin
      if Treat_Fixed_As_Integer (Nod) then
         Write_Char ('#');
      end if;

      if Rounded_Result (Nod) then
         Write_Char ('@');
      end if;
   end Process_TFAI_RR_Flags;

   -----------------
   -- Source_Dump --
   -----------------

   procedure Source_Dump is
   begin
      --  Initialize constants for Write_Uint_Col_Check

      LNegInt  := -(Uint_2 ** (ints - 1));
      LPosInt  := abs (LNegInt + 1);
      LNegLong := -(Uint_2 ** (longs - 1));
      LPosLong := abs (LNegLong + 1);
      LNegLL   := -(Uint_2 ** (lls - 1));
      LPosLL   := abs (LNegLL + 1);

      LPosU    := (Uint_2 ** ints) - 1;
      LNegU    := -LPosU;
      LPosUL   := (Uint_2 ** longs) - 1;
      LNegUL   := -LPosUL;
      LPosULL  := (Uint_2 ** lls) - 1;
      LNegULL  := -LPosULL;

      --  Generate standard headers

      Write_Line ("#include <stdlib.h>");
      Write_Str ("#include ""standard.h""");

      --  Dump requested units

      for U in Main_Unit .. Last_Unit loop
         Current_Source_File := Source_Index (U);

         --  Dump all units if -gnatdf set, otherwise we dump only the source
         --  files that are in the extended main source.

         if Debug_Flag_F
           or else In_Extended_Main_Source_Unit (Cunit_Entity (U))
         then
            Cprint_Node (Cunit (U));
            Write_Eol;
         end if;
      end loop;
   end Source_Dump;

   -----------------------
   -- Write_Discr_Specs --
   -----------------------

   procedure Write_Discr_Specs (N : Node_Id) is
      Specs : List_Id;
      Spec  : Node_Id;

   begin
      Specs := Discriminant_Specifications (N);

      if Present (Specs) then
         Write_Str_Col_Check (" (");
         Spec := First (Specs);

         loop
            Cprint_Node (Spec);
            Next (Spec);
            exit when Spec = Empty;
            Write_Str ("; ");
         end loop;

         Write_Char (')');
      end if;
   end Write_Discr_Specs;

   --------------
   -- Write_Id --
   --------------

   procedure Write_Id (N : Node_Id) is
   begin
      --  Case of a defining identifier

      if Nkind (N) = N_Defining_Identifier then

         --  If defining identifier has an interface name (and no
         --  address clause), then we output the interface name.

         if (Is_Imported (N) or else Is_Exported (N))
           and then Present (Interface_Name (N))
           and then No (Address_Clause (N))
         then
            String_To_Name_Buffer (Strval (Interface_Name (N)));
            Write_Str_Col_Check (Name_Buffer (1 .. Name_Len));

         --  If no interface name (or inactive because there was
         --  an address clause), then just output the Chars name.

         else
            Write_Name_Col_Check (Chars (N));
         end if;

      --  Case of selector of an expanded name where the expanded name has
      --  an associated entity, output this entity. Check that the entity
      --  or associated node is of the right kind, see above.

      elsif Nkind (Parent (N)) = N_Expanded_Name
        and then Selector_Name (Parent (N)) = N
        and then Present (Entity_Or_Associated_Node (Parent (N)))
        and then Nkind (Entity (Parent (N))) in N_Entity
      then
         Write_Id (Entity (Parent (N)));

      --  For enumeration literal, print representation value

      elsif Nkind (N) in N_Has_Entity
        and then Ekind (Entity (N)) = E_Enumeration_Literal
      then
         Write_Uint_Col_Check (Enumeration_Rep (Entity (N)));

      --  For any other node with an associated entity, output entity name

      elsif Nkind (N) in N_Has_Entity
        and then Present (Entity_Or_Associated_Node (N))
        and then Nkind (Entity_Or_Associated_Node (N)) in N_Entity
      then
         Write_Id (Entity (N));

      --  All other cases, we just print the Chars field

      else
         Write_Name_Col_Check (Chars (N));
      end if;
   end Write_Id;

   ------------------
   -- Write_Indent --
   ------------------

   procedure Write_Indent is
   begin
      Write_Eol;

      for J in 1 .. Indent loop
         Write_Char (' ');
      end loop;
   end Write_Indent;

   ----------------------
   -- Write_Indent_Str --
   ----------------------

   procedure Write_Indent_Str (S : String) is
   begin
      Write_Indent;
      Write_Str (S);
   end Write_Indent_Str;

   --------------------------
   -- Write_Name_Col_Check --
   --------------------------

   procedure Write_Name_Col_Check (N : Name_Id) is
   begin
      Get_Name_String (N);
      Write_Str_Col_Check (Name_Buffer (1 .. Name_Len));
   end Write_Name_Col_Check;

   -----------------------
   -- Write_Param_Specs --
   -----------------------

   procedure Write_Param_Specs (N : Node_Id) is
      Specs  : List_Id;
      Spec   : Node_Id;
      Formal : Node_Id;

   begin
      Specs := Parameter_Specifications (N);

      Write_Str_Col_Check ("(");

      if Is_Non_Empty_List (Specs) then
         Spec := First (Specs);
         loop
            Cprint_Node (Spec);
            Formal := Defining_Identifier (Spec);
            Next (Spec);
            exit when Spec = Empty;
            Write_Str (", ");
         end loop;

         --  Write out any extra formals

         while Present (Extra_Formal (Formal)) loop
            Formal := Extra_Formal (Formal);
            Write_Str (", ");
            Cprint_Type_Name (Etype (Formal));
            Write_Char (' ');
            Write_Name_Col_Check (Chars (Formal));
         end loop;
      end if;

      Write_Char (')');
   end Write_Param_Specs;

   ------------------------
   -- Write_Source_Lines --
   ------------------------

   procedure Write_Source_Lines (N : Node_Id) is
   begin
      Write_Source_Lines (First_Line (N), Last_Line (N));
   end Write_Source_Lines;

   procedure Write_Source_Lines (S : Source_Ptr) is
      L : constant Physical_Line_Number := Get_Physical_Line_Number (S);
   begin
      Write_Source_Lines (L, L);
   end Write_Source_Lines;

   procedure Write_Source_Lines
     (From : Source_Ptr;
      To   : Physical_Line_Number)
   is
   begin
      Write_Source_Lines (Get_Physical_Line_Number (From), To);
   end Write_Source_Lines;

   procedure Write_Source_Lines (From, To : Physical_Line_Number) is
      Src : constant Source_Buffer_Ptr := Source_Text (Current_Source_File);

      Write_Blank_Line : Boolean;
      --  If this is True, then a blank line is printed before outputting a
      --  source line, and the flag is reset.

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean;
      --  Returns true if line L is a comment line or blank line

      procedure Write_Line_Directive (L : Physical_Line_Number);
      --  Write line directive for line L, no effect if L is a comment line

      procedure Write_Source_Line (L : Physical_Line_Number);
      --  Write source line L as C comment, no effect if L is a comment line.
      --  Outputs initial blank line if Write_Blank_Line flag is set and then
      --  resets the flag.

      ---------------------
      -- Is_Comment_Line --
      ---------------------

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean is
         Scn : Source_Ptr;

      begin
         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
            Scn := Scn + 1;
         end loop;

         return Src (Scn) in Line_Terminator
           or else Src (Scn .. Scn + 1) = "--";
      end Is_Comment_Line;

      --------------------------
      -- Write_Line_Directive --
      --------------------------

      procedure Write_Line_Directive (L : Physical_Line_Number) is
      begin
         if Is_Comment_Line (L) then
            return;
         end if;

         Write_Eol;
         Write_Str ("#line ");
         Write_Int (Int (L));
         Write_Str (" """);
         Write_Str (Get_Name_String (File_Name (Current_Source_File)));
         Write_Char ('"');
      end Write_Line_Directive;

      -----------------------
      -- Write_Source_Line --
      -----------------------

      procedure Write_Source_Line (L : Physical_Line_Number) is
         Scn : Source_Ptr;

      begin
         if Is_Comment_Line (L) then
            return;
         end if;

         if Write_Blank_Line then
            Write_Eol;
            Write_Blank_Line := False;
         end if;

         Write_Eol;
         Write_Str ("/* ");
         Write_Int (Int (L));
         Write_Str (": ");

         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) not in Line_Terminator loop
            Write_Char (Src (Scn));
            Scn := Scn + 1;
         end loop;

         Write_Str (" */");
      end Write_Source_Line;

      --  Local Variables

      From_Line : Physical_Line_Number := From;
      To_Line   : Physical_Line_Number := To;
      --  Effective from and to lines as adjusted below

   --  Start of processing for Write_Source_Lines

   begin
      --  Deal with no line number values

      if From_Line = No_Physical_Line_Number then
         if To_Line = No_Physical_Line_Number then
            return;
         else
            From_Line := To_Line;
         end if;
      end if;

      if To_Line = No_Physical_Line_Number then
         To_Line := From_Line;
      end if;

      --  If some lines already dealt with, adjust From_Line

      if Last_Line_Printed >= From_Line then
         From_Line := Last_Line_Printed + 1;
      end if;

      --  Return if all lines already printed

      if From_Line > To_Line then
         return;
      end if;

      --  If we are in Dump_Source_Text mode, and there are unprinted source
      --  lines before the first line for the current construct, print these
      --  source lines, but without line directives.

      if Dump_Source_Text and then Last_Line_Printed < From_Line - 1 then
         Write_Blank_Line := True;

         loop
            Last_Line_Printed := Last_Line_Printed + 1;
            exit when Last_Line_Printed = From_Line - 1;
            Write_Source_Line (Last_Line_Printed);
         end loop;
      end if;

      --  If we are in Dump_Source_Text mode, then print the source lines
      --  for the current construct, preceded by a blank line.

      if Dump_Source_Text then
         Write_Blank_Line := True;

         for J in From_Line .. To_Line loop
            Write_Source_Line (J);
         end loop;
      end if;

      --  Write line directives for lines of current construct

      for J in From_Line .. To_Line loop
         Write_Line_Directive (J);
      end loop;

      --  Note all lines up to To processed and we are done

      Last_Line_Printed := To_Line;
      return;
   end Write_Source_Lines;

   -------------------------
   -- Write_Str_Col_Check --
   -------------------------

   procedure Write_Str_Col_Check (S : String) is
   begin
      if Int (S'Last) + Column > Sprint_Line_Limit then
         Write_Indent_Str ("  ");

         if S (S'First) = ' ' then
            Write_Str (S (S'First + 1 .. S'Last));
         else
            Write_Str (S);
         end if;

      else
         Write_Str (S);
      end if;
   end Write_Str_Col_Check;

   ---------------------------
   -- Write_Subprogram_Name --
   ---------------------------

   procedure Write_Subprogram_Name (N : Node_Id) is
   begin
      if not Comes_From_Source (N)
        and then Is_Entity_Name (N)
      then
         declare
            Ent : constant Entity_Id := Entity (N);
         begin
            if not In_Extended_Main_Source_Unit (Ent)
              and then
                Is_Predefined_File_Name
                  (Unit_File_Name (Get_Source_Unit (Ent)))
            then
               --  Run-time routine name, output name with a preceding dollar
               --  making sure that we do not get a line split between them.

               Col_Check (Length_Of_Name (Chars (Ent)) + 1);
               Write_Char ('$');
               Write_Name (Chars (Ent));
               return;
            end if;
         end;
      end if;

      --  Normal case, not a run-time routine name

      Cprint_Node (N);
   end Write_Subprogram_Name;

   --------------------------
   -- Write_Uint_Col_Check --
   --------------------------

   --  Note: we go out of our way to be compatible with ancient versions of C
   --  here, since we anticipate the output being compiled on such compilers.

   procedure Write_Uint_Col_Check (U : Uint) is
      DDH : constant Nat := UI_Decimal_Digits_Hi (U);

   begin
      --  Output largest negative int value as (-X-1) where X is largest
      --  positive int value, to avoid generating out of range int value.

      if U = LNegInt then
         Col_Check (DDH + 4);
         Write_Char ('(');
         UI_Write (U + 1, Decimal);
         Write_Str ("-1)");

      --  Most common case of in int range other than largest neg number

      elsif LNegInt < U and then U <= LPosInt then
         Col_Check (DDH);
         UI_Write (U, Decimal);

      --  Output largest negative long value as (-XL-1) where X is largest
      --  positive long value, to avoid generating out of range long value.

      elsif U = LNegLong then
         Col_Check (DDH + 5);
         Write_Char ('(');
         UI_Write (U + 1, Decimal);
         Write_Str ("L-1)");

      --  If in range of long then output with suffix L

      elsif LNegLong < U and then U <= LPosLong then
         Col_Check (DDH + 1);
         UI_Write (U, Decimal);
         Write_Char ('L');

      --  If in range of unsigned but not int, output with suffix U. This
      --  can happen if long and int have the same range, which is often true.

      elsif LNegU <= U and then U <= LPosU then
         Col_Check (DDH + 1);
         UI_Write (U, Decimal);
         Write_Char ('U');

      --  Remaining processing depends on whether we are allowing long long,
      --  which is controlled by restriction No_Long_Long_Integers.

      else
         --  Long_Long_Integer not allowed

         if Restriction_Active (No_Long_Long_Integers) then

            --  We must be in range of long unsigned, output with suffix LU

            if LNegUL <= U and then U <= LPosUL then
               Col_Check (DDH + 2);
               UI_Write (U, Decimal);
               Write_Str ("LU");

            --  Anything else should be impossible!

            else
               raise Program_Error;
            end if;

         --  Long_Long_Integer is allowed

         else
            --  If in range of long long, output with suffix LL. Note that we
            --  do not bother with largest negative number case here. We assume
            --  that if long long is allowed, the compiler is more modern.

            if LNegLL <= U and then U <= LPosLL then
               Col_Check (DDH + 2);
               UI_Write (U, Decimal);
               Write_Str ("LL");

            --  if in range of long long unsigned, output with suffix LLU

            elsif LNegULL <= U and then U <= LPosULL then
               Col_Check (DDH + 3);
               UI_Write (U, Decimal);
               Write_Str ("LLU");

               --  Anything else should be impossible!

            else
               raise Program_Error;
            end if;
         end if;
      end if;
   end Write_Uint_Col_Check;

   ---------------------------
   -- Write_Ureal_Col_Check --
   ---------------------------

   procedure Write_Ureal_Col_Check (U : Ureal) is
      D : constant Uint := Denominator (U);
      N : constant Uint := Numerator (U);
   begin
      Col_Check (UI_Decimal_Digits_Hi (D) + UI_Decimal_Digits_Hi (N) + 4);
      UR_Write (U, Brackets => True);
   end Write_Ureal_Col_Check;

end Cprint;
