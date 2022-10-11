/* Output to GLI files.
   Copyright (C) 2010, 2011, 2012, 2013 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"
#include "gli.h"
#include "flags.h"
#include "options.h"
#include "version.h"
#include "vec.h"


/* True when the GLI file has already been started for the current compile
   unit.  Used to output the header only the first time the file is opened, and
   to open the file in "append" mode the next times.  */
static bool started = false;

/* True when the GLI file is opened, false when it's closed.  Used to assert
   only one is open at a time.  */
static bool opened = false;

/* True when gli_finalized has been called.  */
static bool finalized = false;

/* List of registered filenames.  */
typedef const char *filename_t;
DEF_VEC_P (filename_t);
DEF_VEC_ALLOC_P (filename_t, heap);
static VEC (filename_t, heap) *files;

/* Open and return the GLI file corresponding to the given MAIN_FILE.  Output
   library headers if it is opened for the first time.  */
FILE *
gli_open (const char *main_file)
{
  FILE *f;
  const char *file_name = lbasename (main_file);
  char *gli_name;

  if (opened)
    internal_error ("GLI file is opened twice.");
  else if (finalized)
    internal_error ("Tried to open GLI file after finalization.");

  gli_name = concat (file_name, ".gli", NULL);

  /* Override the GLI file if it is opened for the first time.  */
  f = fopen (gli_name, started ? "a" : "w");
  free (gli_name);

  if (!f)
    return NULL;

  /* Output the GLI header if needed.  */
  if (!started)
    {
      fprintf (f, "V \"%s\"\n", version_string);
      fprintf (f, "P\n");
      fprintf (f, "R\n");
      fprintf (f, "U %s %s\n", file_name, file_name);
      if (flag_dump_scos)
        fprintf (f, "A -fdump-scos\n");
      if (flag_preserve_control_flow)
        fprintf (f, "A -fpreserve-control-flow\n");
      if (debug_info_level >= DINFO_LEVEL_NORMAL)
        fprintf (f, "A -g\n");
      fprintf (f, "\n");
    }

  started = true;
  opened = true;

  return f;
}

/* Close the given GLI file F.  Do not use fclose () instead, since internal
   data must be updated.  */
void
gli_close (FILE *f)
{
  fclose (f);
  opened = false;
}

/* Print some path to file F, adding quotes if needed.  */
static void
print_path (FILE *f, const char *path)
{
  int i;

  /* If a not printable/space/quote character is found, quote the path.  */
  for (i = 0; path[i] != '\0'; ++i)
    if (path[i] <= '!' || path[i] == '"' || path[i] > '~')
      {
        fputc ('"', f);
        for (i = 0; path[i] != '\0'; ++i)
          {
            fputc (path[i], f);
            /* Double existing quotes.  */
            if (path[i] == '"')
              fputc ('"', f);
          }
        fputc ('"', f);
        return;
      }

  /* If this point is reached, then no "special" character was found: print the
     path as-is.  */
  fprintf (f, "%s", path);
}

/* Return FILENAME's file number, or 0 if FILENAME is not registered.  */
unsigned
gli_filenum (const char *filename)
{
  unsigned idx;
  const char *fn;

  FOR_EACH_VEC_ELT (filename_t, files, idx, fn)
    {
      if (!strcmp(filename, fn))
        return idx + 1;
    }

  return 0;
}

/* Register a FILENAME to be referenced later in the GLI file, and produce a D
   line for it if needed.  Return the corresponding D line index (indices start
   at 1).  Nothing must have been written in the F GLI file (D lines must
   follow the header.  */
unsigned
gli_register_file (FILE *f, const char *filename)
{
  unsigned idx;
  char *path;

  /* First check whether the filename is already registered.  */
  idx = gli_filenum (filename);
  if (idx != 0)
    return idx;

  /* The filename is not registered yet: register it.  */
  VEC_safe_push (filename_t, heap, files, filename);
  fprintf (f, "D ");
  path = lrealpath (filename);
  print_path (f, path ? path : filename);
  free (path);
  fprintf (f, " 00000000000000 00000000\n");
  return VEC_length (filename_t, files);
}

/* Must be called when the GLI production is completely done.  No GLI file must
   be open.  Free all allocated datastructures.  */
void
gli_finalize (void)
{
  if (opened)
    internal_error ("Tried to finalize while GLI file still open.");

  VEC_free (filename_t, heap, files);

  finalized = true;
}
