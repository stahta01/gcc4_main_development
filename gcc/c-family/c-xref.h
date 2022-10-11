/* Interface for -fdump-xref capability.
   Copyright (C) 2010, Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef C_XREF_H
#define C_XREF_H

#include "c-family/c-ada-spec.h"

/* In c-xref.c  */
extern void generate_reference (tree, location_t, char);
extern void generate_method_decl_reference (tree, const_tree);
extern void generate_macro_reference (const char *, location_t, char, int);
extern void generate_include_reference (const char *, location_t, location_t, bool);
extern void generate_type_reference (tree, location_t, char);
extern void generate_enum_reference (tree, location_t, char);
extern void generate_namespace_reference (tree, location_t, location_t, char);

extern void traverse_tree_xref (tree, location_t, char, bool);
extern void traverse_tree_xref_cpp (tree, int (*)(tree, cpp_operation), bool);
extern void output_references (const char *, bool);

#endif
