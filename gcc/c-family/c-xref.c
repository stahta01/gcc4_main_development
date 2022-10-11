/* Traverse GENERIC tree and generate source cross reference information.
   Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.
   Contributed by Arnaud Charlet  <charlet@adacore.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "gli.h"
#include "c-xref.h"
#include "toplev.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "real.h"
#include "fixed-value.h"
#include "version.h"
#include "c-family/c-common.h"

static location_t tree_sloc (const_tree, location_t);
static location_t tree_sloc2 (const_tree, location_t);
static location_t tree_slocs (const_tree, int, location_t);

/* shortcut with default value for typ */
#define generate_ref(e,l)  generate_reference (e, l, typ)

/* shortcut with default value for chain */
#define traverse_tree(t, loc, typ) traverse_tree_xref (t, loc, typ, true)

typedef enum {NONE, MACRO, FUN_MACRO, INCLUDE, INCLUDE_SYSTEM} cpp_kind;

struct GTY (()) xref_entry {
  tree ent;
  const char *name;
  location_t dloc;
  location_t loc;
  char typ;
  cpp_kind kind;
};

typedef struct xref_entry *xref_entry_ref;

DEF_VEC_P(xref_entry_ref);
DEF_VEC_ALLOC_P(xref_entry_ref,gc);

static GTY(()) VEC(xref_entry_ref,gc) *xrefs = NULL;
static bool xrefs_freed = false;

static const char **files = NULL;
static int last_file = 0;

static int (*cpp_check) (tree, cpp_operation) = NULL;

/* tree declarations hash table */

static htab_t tree_decls = NULL;

typedef struct {
  int index;
  const_tree node;
} tree_hash_entry;

/* ??? */

static hashval_t
tree_hash (const void *p)
{
  const tree_hash_entry *m = (const tree_hash_entry *)p;
  return htab_hash_pointer (m->node);
}

/* ??? */

static int
tree_eq (const void *p, const void *q)
{
  const tree_hash_entry *a = (const tree_hash_entry *)p;
  const tree_hash_entry *b = (const tree_hash_entry *)q;

  return a->node == b->node;
}

static xref_entry_ref
find_decl_tree (const_tree t)
{
  tree_hash_entry key;
  tree_hash_entry *res;

  if (!tree_decls)
    return NULL;

  key.node = t;
  res = (tree_hash_entry *) htab_find (tree_decls, &key);
  return res ? VEC_index (xref_entry_ref, xrefs, res->index) : NULL;
}

static void
add_decl_tree (const_tree ent, int index)
{
  tree_hash_entry *m;
  if (!tree_decls)
    tree_decls = htab_create (4096, tree_hash, tree_eq, free);

  m = (tree_hash_entry *)xmalloc (sizeof (tree_hash_entry));
  m->node = ent;
  m->index = index;
  *htab_find_slot (tree_decls, m, INSERT) = m;
}

/* name declarations hash table */

static htab_t name_decls = NULL;

typedef struct {
  int index;
  const char *name;
} name_hash_entry;

static hashval_t
name_hash (const void *p)
{
  unsigned result, i;
  const char *string = ((const name_hash_entry *)p)->name;

  for (result = i = 0; *string++ != '\0'; i++)
    result += ((unsigned char) *string << (i % CHAR_BIT));
  return result;
}

static int
name_eq (const void *p, const void *q)
{
  const name_hash_entry *a = (const name_hash_entry *)p;
  const name_hash_entry *b = (const name_hash_entry *)q;

  return a->name == b->name;
}

static xref_entry_ref
find_decl_name (const char *name)
{
  name_hash_entry key;
  name_hash_entry *res;

  if (!name_decls)
    return NULL;

  key.name = name;
  res = (name_hash_entry *) htab_find (name_decls, &key);
  return res ? VEC_index (xref_entry_ref, xrefs, res->index) : NULL;
}

static void
add_decl_name (const char *name, int index)
{
  name_hash_entry *m;
  if (!name_decls)
    name_decls = htab_create (4096, name_hash, name_eq, free);

  m = (name_hash_entry *)xmalloc (sizeof (name_hash_entry));
  m->name = name;
  m->index = index;
  *htab_find_slot (name_decls, m, INSERT) = m;
}

/* Given two references, return nonzero if they correspond to the same
   entity (definition).  */

static bool
same_entity_p (const xref_entry_ref xref1, const xref_entry_ref xref2)
{
  /* It is not sufficient to verify that two references point to the same
     tree node (or have the same name for macros), because two entities
     may be sharing the same tree node and yet be distinct.  Consider
     for instance the code "typedef enum e { ... } e_t;".  "e" and "e_t"
     share the same type and yet are distinct definitions.

     This is why we also check the location of both definitions.  */
  return (xref1->ent == xref2->ent
	  && xref1->name == xref2->name
	  && xref1->dloc == xref2->dloc);
}

/* Return true if LOC should be skipped.

   For C++, only consider user headers for xrefs and not e.g. stl includes
   to avoid generating huge number of xrefs on e.g. templates.  */

static bool
skip_location (location_t loc)
{
  if (loc == UNKNOWN_LOCATION)
    return false;

  if (loc <= RESERVED_LOCATION_COUNT + 1)
    return true;

  if (c_dialect_cxx ())
    {
      xref_entry_ref e = find_decl_name (LOCATION_FILE (loc));
      return e != NULL && e->kind == INCLUDE_SYSTEM;
    }

  return false;
}

/* Ignore error_mark nodes and null nodes.
   Also skip all xrefs if flag_dump_xref has not been set, or xrefs have
   been dumped (and freed) already.
   T is a node to consider for cross-references.
   NODE_LOC is the location of node T.  */

static bool
skip_xref (const_tree t, location_t node_loc)
{
  location_t loc;

  if (!flag_dump_xref || xrefs_freed)
    return true;

  /* We are never interested in error_mark nodes.  */
  if (!t || t == error_mark_node)
    return true;

  if (node_loc)
    loc = node_loc;
  else
    loc = DECL_P (t) ? DECL_SOURCE_LOCATION (t) : EXPR_LOCATION (t);

  return skip_location (loc);
}

/* Return template result corresponding to t if any.
   Assume cpp_check is not null.  */

static tree
get_decl_template (tree t)
{
  tree tmp[2];

  tmp[0] = t;
  (*cpp_check) ((tree)tmp, GET_DECL_TEMPLATE);
  return tmp[1];
}

/* Allocate xrefs if not already done.  Do nothing otherwise.  */
static void
allocate_xrefs (void)
{
  if (xrefs == NULL)
    xrefs = VEC_alloc (xref_entry_ref, gc, 8192);
}

/* Generate a reference to entity E at location LOC. TYP is one of:
   '\0' declaration
   'b'  body entity
   'c'  completion of type/variable
   'e'  end of construct
   'H'  abstract type
   'i'  implicit reference
   'm'  modification
   'p'  primitive operation
   'P'  overriding primitive operation
   'r'  standard reference
   's'  subprogram reference in a static call
   'R'  subprogram reference in dispatching call
   't'  end of body
   'x'  type extension
   'z'  generic formal parameter
   '>'  default subprogram parameter
   '='  reference subprogram parameter
   '^'  pointer subprogram parameter

   See ada/lib-xref.ads for a more detailed explanation.  */

void
generate_reference (tree e, location_t loc, char typ)
{
  xref_entry_ref xref;

  /* Skip artificial function declarations such as implicit
     constructors. */
  if (TREE_CODE (e) == FUNCTION_DECL && DECL_ARTIFICIAL (e))
    return;

  if (skip_xref (e, loc))
    return;

  if (cpp_check && TREE_CODE (e) == FUNCTION_DECL && DECL_NAME (e))
    {
      /* We skip declarations with names ending in a space since these are
	 compiler generated and don't refer to anything in the sources. */
      const char *name = IDENTIFIER_POINTER (DECL_NAME (e));
      if (name && name[IDENTIFIER_LENGTH (DECL_NAME (e)) - 1] == ' ')
	return;
    }

  allocate_xrefs ();

  if (typ == '\0' || loc == 0)
    {
      /* This is a definition.  */
      xref_entry_ref ref = find_decl_tree (e);

      /* If we have created already a reference for an identical
	 definition, then ignore this one.

	 Note that two definitions may have the same type and yet be
	 distinct, which means it is also important to verify that
	 the definitions' locations (when available) are identical before
	 declaring the definitions as identical.  Consider for instance
	 the following code: "typedef enum e { ... } e_t;". In that case,
	 "enum e" and "e_t" both share the same type, and yet require
	 distinct references.  */
      if (ref && (loc == 0 || ref->dloc == loc))
	return;

      xref = ggc_alloc_xref_entry ();
      xref->dloc = loc ? loc : DECL_SOURCE_LOCATION (e);
      xref->loc = 0;
      xref->typ = '\0';
      xref->ent = e;
      xref->name = NULL;
      xref->kind = NONE;

      VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
      add_decl_tree (xref->ent, VEC_length (xref_entry_ref, xrefs) - 1);
    }
  else
    {
      xref = ggc_alloc_xref_entry ();
      xref->dloc = DECL_SOURCE_LOCATION (e);
      xref->loc = loc;
      xref->typ = typ;
      xref->ent = e;
      xref->name = NULL;
      xref->kind = NONE;

      if (TREE_CODE (e) == FUNCTION_DECL
	  || (TREE_CODE (e) == VAR_DECL
	      && (DECL_EXTERNAL (e) || TREE_PUBLIC (e))))
	{
	  xref_entry_ref def = find_decl_tree (e);
	  if (def)
	    /* found previous definition, override dloc */
	    xref->dloc = def->dloc;
	  else if (typ == 'b')
	    {
	      /* Finish current entry and add new decl entry for this function
		 body-as-spec.  */
	      VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
	      xref = ggc_alloc_xref_entry ();
	      xref->dloc = loc;
	      xref->loc = 0;
	      xref->typ = '\0';
	      xref->ent = e;
	      xref->name = NULL;
	      xref->kind = NONE;
	      VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
	      add_decl_tree (xref->ent, VEC_length (xref_entry_ref, xrefs) - 1);
	      return;
	    }
	}
      VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
    }
}

/* Generate a reference to namespace.  */

void
generate_namespace_reference (tree e, location_t dloc, location_t loc, char typ)
{
  xref_entry_ref xref;

  if (skip_xref (e, dloc))
    return;

  allocate_xrefs ();
  xref = ggc_alloc_xref_entry ();
  xref->dloc = dloc;
  xref->loc = loc;
  xref->typ = typ;
  xref->ent = e;
  xref->name = NULL;
  xref->kind = NONE;
  VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
}

/* Generate a reference to macro.  */

void
generate_macro_reference (const char *name, location_t loc, char typ,
			  int fun_like)
{
  xref_entry_ref def, xref;

  if (skip_location (loc))
    return;

  /* Create a new xref for our macro reference, but do not push it
     just yet, because we need to iterate over the already-existing
     references (not pushing the xref makes it easier to write the
     iteration; see below).  */
  allocate_xrefs ();
  def = find_decl_name (name);
  xref = ggc_alloc_xref_entry ();
  xref->name = name;
  xref->ent = NULL_TREE;
  xref->typ = typ;
  xref->kind = fun_like ? FUN_MACRO : MACRO;

  if (!typ)
    {
      if (def)
	{
	  int i;
	  xref_entry_ref prev_xref;

	  /* The macro was referenced earlier, and/or even possibly
	     defined earlier.  We need to change all such previous
	     references to point to this new definition instead of
	     the old one.  */
	  FOR_EACH_VEC_ELT (xref_entry_ref, xrefs, i, prev_xref)
	    if (prev_xref->name == name)
	      {
		if (prev_xref->typ == '\0')
		  {
		    /* This is one of the previous macro definitions
		       which is now canceled out by this new xref.
		       Change the old xref's type to 'm', to reflect
		       this fact.

		       Also, we need to set the prev_xref's loc (being
		       a former definition, it was set to zero).  It
		       needs to be replaced with the previous definition's
		       location.  */
		    prev_xref->typ = 'm';
		    prev_xref->loc = prev_xref->dloc;
		  }
		prev_xref->dloc = loc;
	      }
        }

      xref->dloc = loc;
      xref->loc = 0;
    }
  else
    {
      xref->dloc = def ? def->dloc : loc;
      xref->loc = loc;
    }

  /* Now we can push our xref, and possibly record this new entry
     in our name_decls hash table.  */
  VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
  if (!def)
    add_decl_name (name, VEC_length (xref_entry_ref, xrefs) - 1);
}

/* Generate a reference to a method declaration.  */

void
generate_method_decl_reference (tree type, const_tree method)
{
  const_tree vindex = DECL_VINDEX (method);

  generate_type_reference
    (type, DECL_SOURCE_LOCATION (method),
     vindex
     && TREE_CODE (vindex) != INTEGER_CST
     && TREE_CODE (vindex) != ERROR_MARK ? 'P' : 'p');
}

/* Generate a reference to an #include directive.  */

void
generate_include_reference (const char *path, location_t loc, location_t dloc,
			    bool system_header)
{
  xref_entry_ref xref;

  allocate_xrefs ();

  if (!find_decl_name (path))
    {
      xref = ggc_alloc_xref_entry ();
      xref->name = path;
      xref->dloc = dloc;
      xref->loc = 0;
      xref->ent = NULL_TREE;
      xref->typ = '\0';
      xref->kind = system_header ? INCLUDE_SYSTEM : INCLUDE;
      VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
      add_decl_name (xref->name, VEC_length (xref_entry_ref, xrefs) - 1);
    }

  xref = ggc_alloc_xref_entry ();
  xref->name = path;
  xref->dloc = dloc;
  xref->loc = loc;
  xref->ent = NULL_TREE;
  xref->typ = 'r';
  xref->kind = INCLUDE;
  VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
}

void
generate_type_reference (tree t, location_t loc, char typ)
{
  xref_entry_ref xref;

  if (skip_xref (t, loc))
    return;

  allocate_xrefs ();
  xref = ggc_alloc_xref_entry ();
  xref->loc = loc;
  xref->dloc = DECL_SOURCE_LOCATION (t);
  xref->ent = TREE_TYPE (t);
  xref->typ = typ;
  xref->name = NULL;
  xref->kind = NONE;
  VEC_safe_push (xref_entry_ref, gc, xrefs, xref);
}

void
generate_enum_reference (tree t, location_t loc, char typ)
{
  xref_entry_ref xref;

  if (skip_xref (t, loc))
    return;

  allocate_xrefs ();
  xref = ggc_alloc_xref_entry ();
  xref->loc = loc;
  xref->dloc = DECL_SOURCE_LOCATION (t);
  xref->ent = t;
  xref->typ = typ;
  xref->name = NULL;
  xref->kind = NONE;
  VEC_safe_push (xref_entry_ref, gc, xrefs, xref);

  if (typ == '\0')
    add_decl_tree (xref->ent, VEC_length (xref_entry_ref, xrefs) - 1);
}

/* Return letter corresponding to entity designated by T.
   See ada/lib-xref.ads for documentation on each letter.  */

static char
xref_entity_letter (tree t)
{
  switch (TREE_CODE (t))
    {
      case OFFSET_TYPE:
      case POINTER_TYPE:
      case REFERENCE_TYPE:
      case FUNCTION_TYPE:
	return 'P';

      case ENUMERAL_TYPE:
	return 'E';

      case BOOLEAN_TYPE:
	return 'B';

      case INTEGER_TYPE:
	return TYPE_UNSIGNED (t) ? 'M' : 'I';

      case REAL_TYPE:
	return 'F';

      case FIXED_POINT_TYPE:
	return 'D';

      case COMPLEX_TYPE: /* no equivalent in Ada. Consider using 'G' ??? */
      case VOID_TYPE:
	return 'R';

      case RECORD_TYPE:
      case UNION_TYPE:
      case QUAL_UNION_TYPE:
	/* Differentiate between classes and simple structs */
	return cpp_check && (*cpp_check) (t, MAYBE_CLASS) ? 'J' : 'R';

      case VECTOR_TYPE:  /* no equivalent in Ada ??? */
      case ARRAY_TYPE:
	return 'A';

      case FUNCTION_DECL:
	{
	  char res = VOID_TYPE_P (TREE_TYPE (TREE_TYPE (t))) ? 'U' : 'V';

	  if (cpp_check && (*cpp_check) (t, IS_ABSTRACT))
	    return res == 'U' ? 'x' : 'y';
	  else
	    return res;
	}

      case LABEL_DECL:
	return 'L';

      case FIELD_DECL:
      case VAR_DECL:
      case PARM_DECL:
	return TOLOWER (xref_entity_letter (TREE_TYPE (t)));

      case NAMESPACE_DECL:
	return 'K';

      case CONST_DECL:
	if (TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
	  return 'n';
	else
	  return TOLOWER (xref_entity_letter (TREE_TYPE (t)));

      default:
	return cpp_check && (*cpp_check) (t, MAYBE_CLASS) ? 'J' : ' ';
    }
}

/* Return file number corresponding to file F.  */

static int
get_filenum (const char *f)
{
  unsigned filenum = 0;
  static const char *cached_file = NULL;
  static unsigned cached_filenum = 0;

  if (f == cached_file)
    return cached_filenum;

  /* This function is not called very often, so probably not worth optimizing
     this look up.  */
  filenum = gli_filenum (f);

  if (filenum != 0)
    {
      cached_file = f;
      cached_filenum = filenum;
    }

  return filenum;
}

/* Return an expanded_location corresponding to LOC.
   If column is not set, default to 1.  */

static inline expanded_location
expand_loc (location_t loc)
{
  expanded_location result = expand_location (loc);
  if (result.column == 0)
    result.column = 1;
  return result;
}

/* Output a type reference to TYPE in file F. C1 and C2 and delimiter
   characters (e.g. '<' and '>') as defined in lib-xref.ads.
   FILE is the last file referenced.
   If USE_STUB is true, only consider TYPE_STUB_DECL, not TYPE_NAME.  */

static void
output_typeref (FILE *f, tree type, char c1, char c2, const char *file,
		bool use_stub)
{
  tree typedecl = TYPE_STUB_DECL (type);
  tree name = TYPE_NAME (type);
  tree final_type;

  if (!typedecl && TREE_CODE (type) == RECORD_TYPE)
    {
      tree field = TYPE_FIELDS (type);

      if (field)
	{
	  while (TREE_CHAIN (field))
	    field = TREE_CHAIN (field);

	  typedecl = TYPE_STUB_DECL (DECL_CONTEXT (field));
	}
    }

  if (use_stub || !name || !DECL_P (name))
    final_type = typedecl;
  else
    final_type = name;

  if (final_type
      && DECL_SOURCE_LOCATION (final_type)
      && !DECL_IS_BUILTIN (final_type))
    {
      expanded_location xloc = expand_loc (DECL_SOURCE_LOCATION (final_type));

      if (file == xloc.file)
	fprintf (f, "%c%d%c%d%c",
		 c1, xloc.line, xref_entity_letter (type), xloc.column, c2);
      else
	fprintf (f, "%c%d|%d%c%d%c",
		 c1,
		 get_filenum (xloc.file),
		 xloc.line,
		 xref_entity_letter (type),
		 xloc.column,
		 c2);
    }
  else if (name)
    fprintf
      (f, "%c%s%c", c1,
       IDENTIFIER_POINTER (DECL_P (name) ? DECL_NAME (name) : name),
       c2);
}

/* Compare two filenames, F1 and F2 may be NULL.  Also ensure that
   main_input_filename is always last.  */
static int
safe_filename_cmp (const char *f1, const char *f2)
{
  if (f1 == f2)
    return 0;

  if (f1 == NULL)
    return -1;

  if (f2 == NULL)
    return 1;

  if (f1 == main_input_filename)
    return 1;

  if (f2 == main_input_filename)
    return -1;

  return filename_cmp (f1, f2);
}

/* Compare two locations LHS and RHS.  */

static int
compare_location (location_t lhs, location_t rhs)
{
  expanded_location xlhs = expand_location (lhs);
  expanded_location xrhs = expand_location (rhs);

  if (xlhs.file != xrhs.file)
    return safe_filename_cmp (xlhs.file, xrhs.file);

  if (xlhs.line != xrhs.line)
    return xlhs.line - xrhs.line;

  if (xlhs.column != xrhs.column)
    return xlhs.column - xrhs.column;

  return 0;
}

/* Comparison function between NODE1 and NODE2, to be used by VEC_qsort.
   This is a critical function which will put the xrefs in proper order, in
   particular each entity sorted by file, and each ref sorted by line within
   each file.  */

static int
compare_xref (const void *node1, const void *node2)
{
  const xref_entry_ref n1 = *(const xref_entry_ref *)node1;
  const xref_entry_ref n2 = *(const xref_entry_ref *)node2;
  expanded_location x1 = expand_location (n1->dloc);
  expanded_location x2 = expand_location (n2->dloc);
  int res;

  /* First test: if entity is in different unit, sort by unit.  */
  if (x1.file != x2.file)
    return safe_filename_cmp (x1.file, x2.file);

  /* Second test: within same unit, sort by def sloc.  */
  else if (! same_entity_p (n1, n2))
    return x1.line != x2.line ? x1.line - x2.line : x1.column - x2.column;

  /* Third test: sort definitions ahead of references */
  else if (!n1->loc)
    return -1;
  else if (!n2->loc)
    return 1;

  /* Fourth test: for same entity, sort by ref sloc */
  res = compare_location (n1->loc, n2->loc);

  if (res != 0)
    return res;

  /* Finally for two refs at the same sloc, prefer the one that
     does NOT have the type 'r' so that e.g. a modification takes preference. */
  else
    return n2->typ == 'r' ? -1 : 1;
}

/* Output in file F a simple reference to xrefs[I] at the specified location
   XLOC.  COUNT is the count of references output in the current line, and
   CURRF the last file referenced.  */

static void
output_simple_reference (FILE *f, int i, expanded_location xloc, int *count,
			 const char **currf)
{
#define MAX_ENTRIES_PER_LINE 11
  xref_entry_ref xref = VEC_index (xref_entry_ref, xrefs, i);
  int length = VEC_length (xref_entry_ref, xrefs);

  if (*count == MAX_ENTRIES_PER_LINE)
    {
      fprintf (f, "\n. ");
      *count = 0;
    }
  else
    fprintf (f, " ");

  if (xloc.file != *currf)
    {
      fprintf (f, "%d|", get_filenum (xloc.file));
      *currf = xloc.file;
    }
  fprintf (f, "%d%c%d",
	   xloc.line,
	   /* If typ is null, it means e.g. a type completion */
	   xref->typ ? xref->typ : 'c',
	   xloc.column);
  (*count)++;

  /* Detect function bodies with no 'end of body' reference.
     Can happen in the case of e.g. static inline function in header
     files with no code generation. In this case, we add a dummy 't'
     reference.  */
  if (xref->typ == 'b'
      && (i == length - 1
	  || xref->ent != VEC_index (xref_entry_ref, xrefs, i+1)->ent))
    fprintf (f, " %d%c%d", xloc.line, 't', xloc.column);
}

/* Output specifial reference to assembly name ASM_NAME in file F, using
   source location from XLOC.  */

static void
output_asm_reference (FILE *f, const char *asm_name, expanded_location xloc)
{
  fprintf (f, " %di<cpp,", xloc.line);

  for (; *asm_name; asm_name++)
    { 
      if (*asm_name == ' ')
	break;
      fputc (*asm_name, f);
    }

  fprintf (f, ">%d", xloc.column);
}

/* Output header and then all references from xrefs in MAIN_FILE.gli in the
   ali format as defined in lib-xref.ads and ali.ads.  If CLEANUP is true, then
   clean up data structures before exiting.  */

void
output_references (const char *main_file, bool cleanup)
{
  static bool sorted = false;

  int i;
  int count = 0;
  int num_files = 0;
  bool file_changed = false;
  bool main_file_found = false;
  xref_entry_ref xref, prev_xref = NULL;

  const char *tmp_file;
  const char *current_file = NULL;
  const char *currf = NULL;  /* Current file for one entity */
  FILE *f = gli_open (main_file);

  if (!f)
    return;

  if (!sorted)
    {
      if (cpp_check)
	{

	  /* adjust entities to always point to the template rather than the
	     instantiation when relevant.  */
	  FOR_EACH_VEC_ELT (xref_entry_ref, xrefs, i, xref)
	    {
	      tree tmp = xref->ent;
	      if (tmp
		  && RECORD_OR_UNION_TYPE_P (tmp)
		  && TYPE_NAME (tmp))
		xref->ent = TREE_TYPE (get_decl_template (TYPE_NAME (tmp)));
	    }
	}

      /* sort references  */
      VEC_qsort (xref_entry_ref, xrefs, compare_xref);

      /* compute number of files referenced */
      FOR_EACH_VEC_ELT (xref_entry_ref, xrefs, i, xref)
	{
	  const char *f = LOCATION_FILE (xref->dloc);

	  if (f && f != current_file)
	    {
	      if (main_file == f)
		main_file_found = true;
	      current_file = f;
	      num_files++;
	    }
	}
      if (!main_file_found)
	num_files++;

      /* compute files referenced */
      files = (const char **)xmalloc (num_files * sizeof (char*));
      current_file = NULL;

      FOR_EACH_VEC_ELT (xref_entry_ref, xrefs, i, xref)
	{
	  const char *f = LOCATION_FILE (xref->dloc);

	  if (f && f != current_file)
	    {
	      current_file = f;
	      files[last_file++] = current_file;
	    }
	}
      if (!main_file_found)
	files[last_file++] = main_file;

      sorted = true;
    }

  current_file = NULL;

  /* Generate li file header */

  for (i = 0; i < last_file; i++)
    gli_register_file (f, files[i]);

  FOR_EACH_VEC_ELT (xref_entry_ref, xrefs, i, xref)
    {
      expanded_location xloc;

      if (!xref->loc && !xref->dloc)
	continue;

      /* start new entity line if new entity */
      if (! (prev_xref && same_entity_p (xref, prev_xref)))
	{
	  char scope;
	  tree type;
	  bool is_type;
	  char letter;
	  const char *name = NULL;
	  const char *asm_name = NULL;
	  bool extra_ref = false;

	  if (xref->dloc)
	    {
	      xloc = expand_loc (xref->dloc);
	      if (xref->loc && xref->loc != xref->dloc)
		extra_ref = true;
	    }
	  else
	    xloc = expand_loc (xref->loc);

	  count = 0;

	  if (xref->name)
	    {
	      if (xref->kind == INCLUDE || xref->kind == INCLUDE_SYSTEM)
		{
		  /* #include reference */
		  letter = 'Q';
		  /* Strip leading path */
		  name = lbasename (xref->name);
		}
	      else
		{
		  if (xref->kind == FUN_MACRO)
		    letter = 'G';
		  else
		    letter = 'g';
		  name = xref->name;
		}
	      scope = '*';
	      is_type = 0;
	    }
	  else
	    {
	      letter = xref_entity_letter (xref->ent);
	      is_type = TREE_CODE_CLASS (TREE_CODE (xref->ent)) == tcc_type;
	      if (is_type
		|| letter == 'n'
		|| TREE_CODE (xref->ent) == FIELD_DECL
		|| (DECL_P (xref->ent) && DECL_EXTERNAL (xref->ent))
		|| TREE_PUBLIC (xref->ent))  /* ??? To be refined */
		scope = '*';
	      else if (TREE_STATIC (xref->ent))
		scope = '+';
	      else
		scope = ' ';

	      if (is_type)
		{
		  tree n;
		  n = TYPE_NAME (xref->ent);

		  if (n && TREE_CODE (n) == TYPE_DECL)
		    n = DECL_NAME (n);

		  if (n)
		    name = IDENTIFIER_POINTER (n);
		  else if (RECORD_OR_UNION_TYPE_P (xref->ent)
			   || TREE_CODE (xref->ent) == ENUMERAL_TYPE)
		    {
		      /* No name on this tree, try to find a suitable name on
			 another tree, to handle cases such as:

			 typedef struct { ...} x;

			 or

			 typedef enum { ...} x;

			 where the type decl for 'x' can be found.
			 Otherwise, use an arbitrary name.
                      */
		      int j = 0;
		      xref_entry_ref xref2;
		      const tree stub = TYPE_STUB_DECL (xref->ent);

		      name = "_anon";
		      FOR_EACH_VEC_ELT_FROM (xref_entry_ref, xrefs, j,
					     xref2, i + 1)
			{ 
			  tree ent2 = xref2->ent;

			  if (ent2 && ent2 != xref->ent
			      && (RECORD_OR_UNION_TYPE_P (ent2)
				  || TREE_CODE (ent2) == ENUMERAL_TYPE)
			      && stub == TYPE_STUB_DECL (ent2))
			    {
			      n = TYPE_NAME (ent2);

			      if (n && TREE_CODE (n) == TYPE_DECL)
				n = DECL_NAME (n);

			      if (n)
				name = IDENTIFIER_POINTER (n);
			      break;
			    }
			}
		    }
		}
	      else
		{
		  const_tree n;

		  if (DECL_P (xref->ent))
		    {
		      n = DECL_NAME (xref->ent);

		      if (scope == '*' && HAS_DECL_ASSEMBLER_NAME_P (xref->ent)
			  && TREE_CODE (xref->ent) != NAMESPACE_DECL
			  && DECL_ASSEMBLER_NAME (xref->ent))
			{
			  asm_name = IDENTIFIER_POINTER
				       (DECL_ASSEMBLER_NAME (xref->ent));
			  if (asm_name
			      && (asm_name [0] != '_' || asm_name [1] != 'Z'))
			    asm_name = NULL;
			}
		    }
		  else
		    n = xref->ent;

		  if (n && TREE_CODE (n) != ERROR_MARK)
		    name = IDENTIFIER_POINTER (n);
		}
	    }

	  if (!name)
	    continue;

	  tmp_file = LOCATION_FILE (xref->dloc);

	  if (!tmp_file)
	    continue;

	  if (tmp_file != current_file)
	    {
	      current_file = tmp_file;
	      file_changed = true;
	    }

	  /* start new xref section if new xref unit */
	  if (file_changed)
	    {
	      fprintf (f, "\nX %d %s",
		       get_filenum (current_file), lbasename (current_file));
	      file_changed = false;
	    }

	  currf = current_file;

	  fprintf
	    (f, "\n%d%c%d%c%s",
	     xloc.line,
	     letter,
	     xloc.column,
	     scope,
	     name);

	  /* ??? add instref when relevant */

	  /* add typeref info */
	  switch (letter)
	    {
	      case 'e': case 'b': case 'm': case 'i': case 'n':
	      case 'f': case 'd': case 'j': case 'o': case 'r':
		/* object */
		output_typeref
		  (f, TREE_TYPE (xref->ent), '{', '}', currf, false);
		break;

	      case 'p':  /* pointer object */
	      case 'P':  /* pointer */
		for (type = TREE_TYPE (xref->ent); POINTER_TYPE_P (type);
		     type = TREE_TYPE (type))
		  ;
		output_typeref (f, type, '(', ')', currf, false);
		break;

	      case 'a':  /* array object */
	      case 'A':  /* array */
		for (type = TREE_TYPE (xref->ent);
		     TREE_CODE (type) == ARRAY_TYPE;
		     type = TREE_TYPE (type))
		  ;
		output_typeref (f, type, '(', ')', currf, false);
		break;

	      case 'E':  /* enums */
	      case 'R':  /* structs */
	      case 'J':  /* classes */
		if (TYPE_NAME (xref->ent) && TYPE_STUB_DECL (xref->ent))
		  {
		    type = TREE_TYPE (TYPE_STUB_DECL (xref->ent));
		    if (xref->dloc !=
			  DECL_SOURCE_LOCATION (TYPE_STUB_DECL (xref->ent)))
		      output_typeref (f, type, '{', '}', currf, true);
		  }
		break;

	      case 'V':  /* function */
		output_typeref
		  (f, TREE_TYPE (TREE_TYPE (xref->ent)), '{', '}',
		   currf, false);
		break;

	      default:
		break;
	    }

	  /* add base class info for classes */
	  if (letter == 'J' && RECORD_OR_UNION_TYPE_P (xref->ent))
	    {
	      tree tmp;

	      /* Look for ancestors.  */
	      for (tmp = TYPE_FIELDS (xref->ent); tmp; tmp = TREE_CHAIN (tmp))
		{
		  type = TREE_TYPE (tmp);

		  if (!DECL_NAME (tmp)
		      && type
		      && RECORD_OR_UNION_TYPE_P (type))
		    output_typeref (f, type, '<', '>', currf, false);
		  else
		    break;
		}
	    }

	  /* ??? add overref when relevant */

	  /* output special implicit ('i') reference to provide the mangled
	     name if relevant.  */
	  if (asm_name != NULL)
	    output_asm_reference (f, asm_name, xloc);

	  /* generate function parameter information */
	  if (letter == 'U' || letter == 'V')
	    {
	      int j, generate_params = 0;
	      xref_entry_ref xref2;

	      /* Generate function parameter information if the body is
		 available.  */
	      FOR_EACH_VEC_ELT_FROM (xref_entry_ref, xrefs, j, xref2, i + 1)
		{
		  if (xref->ent != xref2->ent)
		    break;

		  if (xref2->typ == 'b')
		    {
		      generate_params = 1;
		      break;
		    }
		}

	      if (generate_params)
		{
		  const_tree arg = DECL_ARGUMENTS (xref->ent);

                  for (; arg; arg = TREE_CHAIN (arg))
		    {
		      /* Skip implicit parameters (e.g. this parameter for C++
			 methods. */
		      if (!DECL_ARTIFICIAL (arg))
			{
			  enum tree_code code = TREE_CODE (TREE_TYPE (arg));
			  const expanded_location l =
			    expand_loc (DECL_SOURCE_LOCATION (arg));

			  fprintf (f, " ");
			  if (l.file != currf)
			    {
			      fprintf (f, "%d|", get_filenum (l.file));
			      currf = l.file;
			    }
			  fprintf (f, "%d%c%d", l.line,
				   code == POINTER_TYPE   ? '^' :
			  	   code == REFERENCE_TYPE ? '=' : '>',
			  	   l.column);
			}
		    }
		}
	    }

	  if (extra_ref)
	    {
	      /* xref points to a reference with an implicit declaration.
	         The code above has generated the declaration, we now need to
	         generate the reference itself.  */
	      output_simple_reference
		(f, i, expand_loc (xref->loc), &count, &currf);
	    }
	}
      else
	{
	  xloc = expand_loc (xref->loc ? xref->loc : xref->dloc);

	  /* Skip duplicate definitions/entries and skip 'r' reference which
	     duplicates a declaration.  */
	  if (xref->typ == ' '
	      || (prev_xref
		  && ((xref->loc == prev_xref->loc &&
		       xref->typ == prev_xref->typ)
		      || (prev_xref->typ == '\0' &&
			  xref->loc == prev_xref->dloc &&
			  xref->typ == 'r'))))
	    {
	      prev_xref = xref;
	      continue;
	    }

	  output_simple_reference (f, i, xloc, &count, &currf);
	}

      prev_xref = xref;
    }

  if (main_file_found)
    fprintf (f, "\n");
  else
    fprintf (f, "\nX %d %s\n", get_filenum (main_file), main_file);
  gli_close (f);

  if (cleanup)
    {
      if (xrefs)
        VEC_free (xref_entry_ref, gc, xrefs);
      if (tree_decls)
        htab_delete (tree_decls);
      if (name_decls)
        htab_delete (name_decls);
      xrefs_freed = true;
    }
}

/* Return location associated with expression t.
   If location is 0, defaults to def */

static location_t
tree_sloc (const_tree t, location_t def)
{
  location_t res = EXPR_LOCATION (t);
  return res ? res : def;
}

static location_t
tree_sloc2 (const_tree t, location_t def)
{
  location_t *res = EXPR_LOCATIONS (t);
  return res && res [0] ? res[0] : tree_sloc (t, def);
}

static location_t
tree_slocs (const_tree t, int n, location_t def)
{
  location_t res = expr_location_n (t, n);

  if (res != UNKNOWN_LOCATION)
    return res;
  else
    return tree_sloc (t, def);
}

/* Traverse tree T recursively and generate reference to each entity found
   from sources.
   NODE_LOC is a default location for T if relevant.
   TYP is a reference type, as defined by generate_reference.
   If CHAIN is true, look at all TREE_CHAIN of t
   A special value of ' ' means unknown.
   A special value of '\0' means entity may be a duplicate.  */

void
traverse_tree_xref (tree t, location_t node_loc, char typ, bool chain)
{
  static bool builtin_unit = 0;
  enum tree_code code;
  enum tree_code_class code_class;
  tree op;
  int i;

  if (!t)
    return;

  if (skip_xref (t, node_loc))
    return;

  while (t) {
    /* Figure out what kind of node this is.  */
    code = TREE_CODE (t);
    code_class = TREE_CODE_CLASS (code);

    /* Skip built-in and compiler-generated declarations */
    if (DECL_P (t)
	&& code != TRANSLATION_UNIT_DECL
	&& (DECL_IS_BUILTIN (t)
	    || (DECL_ARTIFICIAL (t)
		&& code != TYPE_DECL && code != CONST_DECL)))
      {
	if (!chain)
	  return;

	t = TREE_CHAIN (t);
	continue;
      }

    if (TREE_VISITED (t))
      goto cont_loop;

    if (chain)
      TREE_VISITED (t) = 1;

    /* We can knock off a bunch of expression nodes in exactly the same
       way.  */
    if (IS_EXPR_CODE_CLASS (code_class))
      {
      switch (code_class)
	{
	case tcc_unary:
	  traverse_tree (TREE_OPERAND (t, 0), tree_sloc2 (t, node_loc), 'r');
	  break;

	case tcc_binary:
	case tcc_comparison:
	  traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), 'r');
	  traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), 'r');
	  break;

	case tcc_expression:
	case tcc_reference:
	case tcc_statement:
	case tcc_vl_exp:
	  /* These nodes are handled explicitly below.  */
	  break;

	default:
	  gcc_unreachable ();
	}
      }

  /* Now handle the various kinds of nodes.  */
  switch (code)
    {
    case TYPE_DECL:
      /* Normally, we ignore artificial entities when generating references.

	 But we also need to make sure we generate a reference for anonymous
	 enum types, whose nodes are marked as artificial, because others
	 are going to point to this anonymous enum type.  For instance,
	 consider the following typedef: "typedef enum {...} enum_t;";
	 The reference generated for the "enum_t" typedef will point to
	 our anonymous enum.  */
      if (!DECL_ARTIFICIAL (t)
	  || TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
	generate_reference (TREE_TYPE (t), DECL_SOURCE_LOCATION (t), '\0');
      traverse_tree (TREE_TYPE (t), 0, typ);
      break;

    case TREE_LIST:
      {
	tree tmp;

	for (tmp = t; tmp; tmp = TREE_CHAIN (tmp))
	  {
	    traverse_tree (TREE_PURPOSE (tmp), node_loc, typ);
	    traverse_tree (TREE_VALUE (tmp), node_loc, typ);
	  }
      }
      break;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator it;
	for (it = tsi_start (t); !tsi_end_p (it); tsi_next (&it))
	  traverse_tree (tsi_stmt (it), node_loc, typ);
      }
      break;

    case BLOCK:
      traverse_tree (BLOCK_VARS (t), 0, typ);
      traverse_tree (BLOCK_SUBBLOCKS (t), 0, typ);
      break;

    case TREE_VEC:
      for (i = 0; i < TREE_VEC_LENGTH (t); ++i)
	traverse_tree (TREE_VEC_ELT (t, i), node_loc, typ);
      break;

    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      if (!node_loc)
	traverse_tree (TREE_TYPE (t), 0, typ);
      break;

    case METHOD_TYPE:
      if (!node_loc)
	traverse_tree (TYPE_METHOD_BASETYPE (t), 0, typ);
      /* Fall through.  */

    case FUNCTION_TYPE:
      if (!node_loc)
	{
	  traverse_tree (TREE_TYPE (t), 0, typ);
	  traverse_tree (TYPE_ARG_TYPES (t), 0, typ);
	}
      break;

    case ARRAY_TYPE:
      if (!node_loc)
	{
	  traverse_tree (TREE_TYPE (t), 0, typ);
	  traverse_tree (TYPE_DOMAIN (t), 0, typ);
	}
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (!node_loc)
	{
	  tree tmp;
	  traverse_tree (TYPE_FIELDS (t), 0, typ);

	  /* Generate reference for methods declarations with no available
	     bodies. Methods with bodies have already been traversed via
	     cp-gimplify.c:cp_genericize().  */
	  for (tmp = TYPE_METHODS (t); tmp; tmp = TREE_CHAIN (tmp))
	    if (TREE_CODE (tmp) != FUNCTION_DECL || !DECL_SAVED_TREE (tmp))
	      traverse_tree_xref (tmp, 0, typ, false);
	}
      break;

    case CONST_DECL:
      if (!node_loc)
	traverse_tree (DECL_INITIAL (t), node_loc, typ);
      else if (TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE || DECL_NAME (t))
	generate_ref (t, node_loc);
      break;

    case PARM_DECL:
      generate_ref (t, node_loc);

      /* Generate reference to types which have an associated sloc */
      if (!node_loc
	  && DECL_ARG_TYPE (t)
	  && DECL_P (DECL_ARG_TYPE (t))
	  && DECL_SOURCE_LOCATION (DECL_ARG_TYPE (t)))
	generate_ref (DECL_ARG_TYPE (t), DECL_SOURCE_LOCATION (t));
      /* ??? won't that generate ref to pointer, but not to foo in:
	struct foo *x;
	*/
      break;

    case VAR_DECL:
      if (node_loc || !builtin_unit)
	generate_ref (t, node_loc);

      if (!node_loc)
	traverse_tree (DECL_INITIAL (t), DECL_SOURCE_LOCATION (t), 'r');
      break;

    case FIELD_DECL:
      if (node_loc)
	generate_ref (t, node_loc);
      else
	{
	  generate_reference (t, 0, '\0');
	  traverse_tree (DECL_INITIAL (t), DECL_SOURCE_LOCATION (t), 'r');
	}
      break;

    case FUNCTION_DECL:
      {
	tree tmp = cpp_check ? get_decl_template (t) : t;

	if (DECL_SAVED_TREE (tmp))
	  {
	    if (!typ)
	      break;

	    if (node_loc)
	      generate_reference (tmp, node_loc, typ > ' ' ? typ : 'r');
	    else
	      generate_reference (tmp, DECL_SOURCE_LOCATION (tmp), 'b');
	  }
	else if (node_loc || !builtin_unit)
	  generate_reference (tmp, node_loc, typ > ' ' ? typ : 'r');

	if (!node_loc)
	  {
	    traverse_tree (DECL_ARGUMENTS (tmp), 0, 'r');
	    traverse_tree (DECL_SAVED_TREE (tmp), 0, 'r');
	  }
      }
      break;

    case TRANSLATION_UNIT_DECL:
      {
	bool prev_builtin_unit = builtin_unit;
	builtin_unit = DECL_IS_BUILTIN (t);
	traverse_tree (DECL_INITIAL (t), 0, typ);
	builtin_unit = prev_builtin_unit;
      }
      break;

    case EXPR_STMT:
    case TRUTH_NOT_EXPR:
    case INDIRECT_REF:
    case CLEANUP_POINT_EXPR:
    case SAVE_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      traverse_tree
	(TREE_OPERAND (t, 0), tree_sloc2 (t, node_loc), typ > ' ' ? typ : 'r');
      break;

    case ADDR_EXPR:
      op = TREE_OPERAND (t, 0);
      if (TREE_CODE (op) == FUNCTION_DECL)
	{ 
	  location_t tmp_loc = node_loc ? node_loc : tree_sloc2 (t, 0);
	  if (tmp_loc)
	    traverse_tree (op, tmp_loc, typ > ' ' ? typ : 'r');
	}
      else
	{ 
	  location_t tmp_loc = tree_sloc2 (t, node_loc);
	  if (tmp_loc)
	    traverse_tree (op, tmp_loc, typ > ' ' ? typ : 'r');
	}
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case COMPOUND_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), 'r');
      traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), 'r');
      break;

    case INIT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), 'm');
      traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), 'r');
      break;

    case MODIFY_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), 'm');
      traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), 'r');
      break;

    case COMPONENT_REF:
      traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), typ);
      traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), typ);
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), typ);
      traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), 'r');
      break;

    case COND_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), 'r');
      traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), 'r');
      traverse_tree (TREE_OPERAND (t, 2), tree_slocs (t, 2, node_loc), 'r');
      break;

    case TRY_FINALLY_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), tree_slocs (t, 0, node_loc), typ);
      traverse_tree (TREE_OPERAND (t, 1), tree_slocs (t, 1, node_loc), typ);
      break;

    case CALL_EXPR:
      {
	tree arg;
	const tree fn = CALL_EXPR_FN (t);
	call_expr_arg_iterator iter;
	int cnt = 1;
	bool is_method_call = TREE_CODE (fn) == OBJ_TYPE_REF;

	if (!is_method_call)
	  {
	    /* if TREE_CODE (fn) == OBJ_TYPE_REF then a 'R' reference has been
	       generated already.  */
	    traverse_tree (fn, tree_sloc2 (t, node_loc), 's');

	    if (TREE_CODE (fn) == ADDR_EXPR)
	      {
		tree tmp = TREE_TYPE (fn);
		while (TREE_CODE (tmp) == POINTER_TYPE)
		  tmp = TREE_TYPE (tmp);
		if (TREE_CODE (tmp) == METHOD_TYPE)
		  is_method_call = 1;
	      }
	  }

	FOR_EACH_CALL_EXPR_ARG (arg, iter, t)
	  {
	    if (iter.i == 1 && is_method_call)
	      {
		tree tmp = arg;

		/* Simple DECL refs are handled in cp/parser.c directly,
		   with a more precise sloc.  */
		if (TREE_OPERAND_LENGTH (tmp) == 1)
		  tmp = TREE_OPERAND (tmp, 0);

		if (!DECL_P (tmp))
		  traverse_tree (arg, tree_slocs (t, 0, node_loc), 'r');
	      }
	    else
	      traverse_tree (arg, tree_slocs (t, cnt++, node_loc), 'r');
	  }
      }
      break;

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT cnt;
	tree index, value;

	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), cnt, index, value)
	  {
	    traverse_tree (index, 0, typ);
	    traverse_tree (value, 0, typ);
	  }
      }
      break;

    case BIND_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), 0, 'r');  /* vars */
      traverse_tree (TREE_OPERAND (t, 1), 0, 'r');  /* body */
      break;

    case LOOP_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), 0, 'r');
      break;

    case EXIT_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), 0, 'r');
      break;

    case RETURN_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), tree_sloc (t, node_loc), 'r');
      break;

    case TARGET_EXPR:
      traverse_tree  /* use tree_slocs(0/1) instead??? */
	(TREE_OPERAND (t, 0), tree_sloc (t, node_loc), 'm');   /* target */
      traverse_tree
	(TREE_OPERAND (t, 1), tree_sloc2 (t, node_loc), 'r');  /* initializer */
      traverse_tree (TREE_OPERAND (t, 2), 0, 'r');	       /* clean up */
      break;

    case CASE_LABEL_EXPR:
      traverse_tree (CASE_LABEL (t), tree_sloc (t, node_loc), 'r');
      break;

    case LABEL_EXPR:
      generate_reference (TREE_OPERAND (t, 0), node_loc, 'r');
      break;

    case GOTO_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), tree_sloc (t, node_loc), 'r');
      break;

    case SWITCH_EXPR:
      traverse_tree (TREE_OPERAND (t, 0), 0, 'r');  /* cond */
      traverse_tree (TREE_OPERAND (t, 1), 0, 'r');  /* body */

      if (TREE_OPERAND (t, 2))
	traverse_tree (TREE_OPERAND (t, 2), 0, 'r');  /* label */
      break;

    case DECL_EXPR:
      traverse_tree (DECL_EXPR_DECL (t), 0, '\0');
      break;

    default:
      if (cpp_check)
	{
	  if ((*cpp_check) (t, IS_TEMPLATE))
	    {
	      /* DECL_RESULT_FLD is DECL_TEMPLATE_RESULT in this context.  */
	      traverse_tree (DECL_RESULT_FLD (t), 0, 'r');
	      break;
	    }
	  else if ((*cpp_check) (t, IS_TEMPLATE_PARM_INDEX))
	    {
	      tree tmp[2];
	      tmp[0] = t;
	      (*cpp_check) ((tree)tmp, GET_TEMPLATE_PARM_DECL);
	      traverse_tree (tmp[1], node_loc, 'r');
	      break;
	    }
	}
      /* generic fallback for unknown statements */
      if (code_class == tcc_statement)
	{
	  int len = TREE_OPERAND_LENGTH (t), i;
	  location_t loc = tree_sloc (t, node_loc);

	  for (i = 0; i < len; i++)
	    traverse_tree (TREE_OPERAND (t, i), loc, 'r');
	}
      break;

    }

  TREE_VISITED (t) = 0;

cont_loop:
  if (!DECL_P (t) || node_loc || !chain)
    return;

  t = TREE_CHAIN (t);

  if (!t || (code == FIELD_DECL && TREE_CODE (t) != FIELD_DECL))
    return;
  }
}

/* Wrapper around traverse_tree_xref to recursively traverse T, using CHECK and
   CHAIN.  */

void
traverse_tree_xref_cpp (tree t, int (*check)(tree, cpp_operation), bool chain)
{
  if (xrefs_freed)
    return;

  cpp_check = check;
  traverse_tree_xref (t, 0, ' ', chain);
}

#include "gt-c-family-c-xref.h"                                          
