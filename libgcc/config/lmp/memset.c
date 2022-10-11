/* This file must be kept in sync with newlib/libc/machine/lmp/memset.c  */

#include <stddef.h>

#include "memset.h"
#include "memset_objects.h"


static inline void
__int_memset (void *__restrict s1, int val, size_t n)
{
  int value = n;
  int loop_var;
  int *out = s1;
  int count;
  int m0 = val;


  /* This code currently give a stall for any value with a 1->2 in the low 5
     bits, i.e.  1,2, 33,34 ? not acceptable! */

  switch (value & 0x1f)
    {
    case (0):
      break;
    case (1):
      SET_1_OBJECT (out);
      break;
    case (2):
      SET_2_OBJECTS (out);
      break;
    case (3):
      SET_3_OBJECTS (out);
      break;
    case (4):
      SET_4_OBJECTS (out);
      break;
    case (5):
      SET_5_OBJECTS (out);
      break;
    case (6):
      SET_6_OBJECTS (out);
      break;
    case (7):
      SET_7_OBJECTS (out);
      break;
    case (8):
      SET_8_OBJECTS (out);
      break;
    case (9):
      SET_9_OBJECTS (out);
      break;
    case (10):
      SET_10_OBJECTS (out);
      break;
    case (11):
      SET_11_OBJECTS (out);
      break;
    case (12):
      SET_12_OBJECTS (out);
      break;
    case (13):
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (14):
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case (15):
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (16):
      SET_16_OBJECTS (out);
      break;
    case (17):
      SET_11_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case (18):
      SET_9_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case (19):
      SET_16_OBJECTS (out);
      SET_3_OBJECTS (out);
      break;
    case (20):
      SET_16_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (21):
      SET_16_OBJECTS (out);
      SET_5_OBJECTS (out);
      break;
    case (22):
      SET_16_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case (23):
      SET_16_OBJECTS (out);
      SET_7_OBJECTS (out);
      break;
    case (24):
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      break;
    case (25):
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case (26):
      SET_16_OBJECTS (out);
      SET_10_OBJECTS (out);
      break;
    case (27):
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      break;
    case (28):
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (29):
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (30):
      SET_16_OBJECTS (out);
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case (31):
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    }


  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies */

  count = value >> 5;

  for (loop_var = 0; loop_var < count; loop_var++)
    SET_32_OBJECTS (out);
}


static inline void
__short_int_memset (void *__restrict s1, int val, size_t n)
{
  int value = n;
  int loop_var;
  int short *out = s1;
  int count;
  int m0 = val;


  switch (value & 0x1f)
    {
    case (0):
      break;
    case (1):
      SET_1_OBJECT (out);
      break;
    case (2):
      SET_2_OBJECTS (out);
      break;
    case (3):
      SET_3_OBJECTS (out);
      break;
    case (4):
      SET_4_OBJECTS (out);
      break;
    case (5):
      SET_5_OBJECTS (out);
      break;
    case (6):
      SET_6_OBJECTS (out);
      break;
    case (7):
      SET_7_OBJECTS (out);
      break;
    case (8):
      SET_8_OBJECTS (out);
      break;
    case (9):
      SET_9_OBJECTS (out);
      break;
    case (10):
      SET_10_OBJECTS (out);
      break;
    case (11):
      SET_11_OBJECTS (out);
      break;
    case (12):
      SET_12_OBJECTS (out);
      break;
    case (13):
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (14):
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case (15):
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (16):
      SET_16_OBJECTS (out);
      break;
    case (17):
      SET_11_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case (18):
      SET_9_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case (19):
      SET_16_OBJECTS (out);
      SET_3_OBJECTS (out);
      break;
    case (20):
      SET_16_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (21):
      SET_16_OBJECTS (out);
      SET_5_OBJECTS (out);
      break;
    case (22):
      SET_16_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case (23):
      SET_16_OBJECTS (out);
      SET_7_OBJECTS (out);
      break;
    case (24):
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      break;
    case (25):
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case (26):
      SET_16_OBJECTS (out);
      SET_10_OBJECTS (out);
      break;
    case (27):
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      break;
    case (28):
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (29):
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (30):
      SET_16_OBJECTS (out);
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case (31):
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    }


  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies.  */

  count = value >> 5;

  for (loop_var = 0; loop_var < count; loop_var++)
    SET_32_OBJECTS (out);

}

static inline void
__byte_memset (void *__restrict s1, int val, size_t n)
{
  int value = n;
  int loop_var;
  char *out = s1;
  int count;
  int m0 = val;


  switch (value & 0x1f)
    {
    case (0):
      break;
    case (1):
      SET_1_OBJECT (out);
      break;
    case (2):
      SET_2_OBJECTS (out);
      break;
    case (3):
      SET_3_OBJECTS (out);
      break;
    case (4):
      SET_4_OBJECTS (out);
      break;
    case (5):
      SET_5_OBJECTS (out);
      break;
    case (6):
      SET_6_OBJECTS (out);
      break;
    case (7):
      SET_7_OBJECTS (out);
      break;
    case (8):
      SET_8_OBJECTS (out);
      break;
    case (9):
      SET_9_OBJECTS (out);
      break;
    case (10):
      SET_10_OBJECTS (out);
      break;
    case (11):
      SET_11_OBJECTS (out);
      break;
    case (12):
      SET_12_OBJECTS (out);
      break;
    case (13):
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (14):
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case (15):
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (16):
      SET_16_OBJECTS (out);
      break;
    case (17):
      SET_11_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case (18):
      SET_9_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case (19):
      SET_16_OBJECTS (out);
      SET_3_OBJECTS (out);
      break;
    case (20):
      SET_16_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (21):
      SET_16_OBJECTS (out);
      SET_5_OBJECTS (out);
      break;
    case (22):
      SET_16_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case (23):
      SET_16_OBJECTS (out);
      SET_7_OBJECTS (out);
      break;
    case (24):
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      break;
    case (25):
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case (26):
      SET_16_OBJECTS (out);
      SET_10_OBJECTS (out);
      break;
    case (27):
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      break;
    case (28):
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (29):
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case (30):
      SET_16_OBJECTS (out);
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case (31):
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    }


  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word  */

  count = value >> 5;

  for (loop_var = 0; loop_var < count; loop_var++)
    SET_32_OBJECTS (out);

}

/* Exposed interface.  */

void
__long_int_memset (void *__restrict s, int c, size_t n)
{
  int ic = (c << 24) + ((char) c << 16) + ((char) c << 8) + (char) c;
  __int_memset (s, ic, n);
}

void
__wrd_memset (void *__restrict s, int c, size_t n)
{
  int sc = ((c << 8) + (char) c);
  __short_int_memset (s, sc, n);
}

void
__byt_memset (void *__restrict s, int c, size_t n)
{
  __byte_memset (s, c, n);
}
