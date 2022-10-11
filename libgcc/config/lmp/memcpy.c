/* This file must be kept in sync with newlib/libc/machine/lmp/memcpy.c  */

#include <stddef.h>

#include "memcpy.h"

#define INST_BARRIER   __asm__ __volatile__ ("":::"memory");

#include "memtrans_bytes.h"
#include "memtrans_words.h"
#include "memtrans_long_words.h"


static inline void
__int_memcpy (void *__restrict s1, const void *__restrict s2, size_t n) 
{
  int value = n;
  int loop_var;
  const int *in = s2;
  int *out = s1;
  int count;
  int m0,m1,m2,m3;

  /* This code currently give a stall for any value with a 1->2 in the low 5
     bits, i.e.  1,2, 33,34 ? not acceptable!  */

  switch (value & 0x1f)
    {
    case (0):
      break;
    case (1):
      MOVE_1_LONG_WORD (in, out);
      break;
    case (2):
      MOVE_2_LONG_WORDS (in, out);
      break;
    case (3):
      MOVE_3_LONG_WORDS (in, out);
      break;
    case (4):
      MOVE_4_LONG_WORDS (in, out);
      break;
    case (5):
      MOVE_5_LONG_WORDS (in, out);
      break;
    case (6):
      MOVE_6_LONG_WORDS (in, out);
      break;
    case (7):
      MOVE_7_LONG_WORDS (in, out);
      break;
    case (8):
      MOVE_8_LONG_WORDS (in, out);
      break;
    case (9):
      MOVE_9_LONG_WORDS (in, out);
      break;
    case (10):
      MOVE_10_LONG_WORDS (in, out);
      break;
    case (11):
      MOVE_11_LONG_WORDS (in, out);
      break;
    case (12):
      MOVE_12_LONG_WORDS (in, out);
      break;
    case (13):
      MOVE_9_LONG_WORDS (in, out);
      MOVE_4_LONG_WORDS (in, out);
      break;
    case (14):
      MOVE_12_LONG_WORDS (in, out);
      MOVE_2_LONG_WORDS (in, out);
      break;
    case (15):
      MOVE_11_LONG_WORDS (in, out);
      MOVE_4_LONG_WORDS (in, out);
      break;
    case (16):
      MOVE_16_LONG_WORDS (in, out);
      break;
    case (17):
      MOVE_11_LONG_WORDS (in, out);
      MOVE_6_LONG_WORDS (in, out);
      break;
    case (18):
      MOVE_9_LONG_WORDS (in, out);
      MOVE_9_LONG_WORDS (in, out);
      break;
    case (19):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_3_LONG_WORDS (in, out);
      break;
    case (20):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_4_LONG_WORDS (in, out);
      break;
    case (21):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_5_LONG_WORDS (in, out);
      break;
    case (22):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_6_LONG_WORDS (in, out);
      break;
    case (23):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_7_LONG_WORDS (in, out);
      break;
    case (24):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_8_LONG_WORDS (in, out);
      break;
    case (25):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_9_LONG_WORDS (in, out);
      break;
    case (26):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_10_LONG_WORDS (in, out);
      break;
    case (27):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_11_LONG_WORDS (in, out);
      break;
    case (28):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_8_LONG_WORDS (in, out);
      MOVE_4_LONG_WORDS (in, out);
      break;
    case (29):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_9_LONG_WORDS (in, out);
      MOVE_4_LONG_WORDS (in, out);
      break;
    case (30):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_12_LONG_WORDS (in, out);
      MOVE_2_LONG_WORDS (in, out);
      break;
    case (31):
      MOVE_16_LONG_WORDS (in, out);
      MOVE_11_LONG_WORDS (in, out);
      MOVE_4_LONG_WORDS (in, out);
      break;
    }


  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies.  */
  
  count = value >> 5;

  for (loop_var = 0; loop_var < count; loop_var++)
    MOVE_32_LONG_WORDS (in, out);
}


static inline void
__shrt_int_memcpy (void *__restrict s1, const void *__restrict s2, size_t n) 
{
  int value = n;
  int loop_var;
  const short int *in = s2;
  int short *out = s1;
  int count;
  int m0,m1,m2,m3;

 /* This code currently give a stall for any value with a 1->2 in the low 5
    bits, i.e.  1,2, 33,34 ? not acceptable!  */

  switch (value & 0x1f)
    {
    case (0):
      break;
    case (1):
      MOVE_1_WORD (in, out);
      break;
    case (2):
      MOVE_2_WORDS (in, out);
      break;
    case (3):
      MOVE_3_WORDS (in, out);
      break;
    case (4):
      MOVE_4_WORDS (in, out);
      break;
    case (5):
      MOVE_5_WORDS (in, out);
      break;
    case (6):
      MOVE_6_WORDS (in, out);
      break;
    case (7):
      MOVE_7_WORDS (in, out);
      break;
    case (8):
      MOVE_8_WORDS (in, out);
      break;
    case (9):
      MOVE_9_WORDS (in, out);
      break;
    case (10):
      MOVE_10_WORDS (in, out);
      break;
    case (11):
      MOVE_11_WORDS (in, out);
      break;
    case (12):
      MOVE_12_WORDS (in, out);
      break;
    case (13):
      MOVE_9_WORDS (in, out);
      MOVE_4_WORDS (in, out);
      break;
    case (14):
      MOVE_12_WORDS (in, out);
      MOVE_2_WORDS (in, out);
      break;
    case (15):
      MOVE_11_WORDS (in, out);
      MOVE_4_WORDS (in, out);
      break;
    case (16):
      MOVE_16_WORDS (in, out);
      break;
    case (17):
      MOVE_11_WORDS (in, out);
      MOVE_6_WORDS (in, out);
      break;
    case (18):
      MOVE_9_WORDS (in, out);
      MOVE_9_WORDS (in, out);
      break;
    case (19):
      MOVE_16_WORDS (in, out);
      MOVE_3_WORDS (in, out);
      break;
    case (20):
      MOVE_16_WORDS (in, out);
      MOVE_4_WORDS (in, out);
      break;
    case (21):
      MOVE_16_WORDS (in, out);
      MOVE_5_WORDS (in, out);
      break;
    case (22):
      MOVE_16_WORDS (in, out);
      MOVE_6_WORDS (in, out);
      break;
    case (23):
      MOVE_16_WORDS (in, out);
      MOVE_7_WORDS (in, out);
      break;
    case (24):
      MOVE_16_WORDS (in, out);
      MOVE_8_WORDS (in, out);
      break;
    case (25):
      MOVE_16_WORDS (in, out);
      MOVE_9_WORDS (in, out);
      break;
    case (26):
      MOVE_16_WORDS (in, out);
      MOVE_10_WORDS (in, out);
      break;
    case (27):
      MOVE_16_WORDS (in, out);
      MOVE_11_WORDS (in, out);
      break;
    case (28):
      MOVE_16_WORDS (in, out);
      MOVE_8_WORDS (in, out);
      MOVE_4_WORDS (in, out);
      break;
    case (29):
      MOVE_16_WORDS (in, out);
      MOVE_9_WORDS (in, out);
      MOVE_4_WORDS (in, out);
      break;
    case (30):
      MOVE_16_WORDS (in, out);
      MOVE_12_WORDS (in, out);
      MOVE_2_WORDS (in, out);
      break;
    case (31):
      MOVE_16_WORDS (in, out);
      MOVE_11_WORDS (in, out);
      MOVE_4_WORDS (in, out);
      break;
    }


  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies.  */

  count = value >> 5;

  for (loop_var = 0; loop_var < count; loop_var++)
    MOVE_32_WORDS (in, out);
}


static inline void
__byte_memcpy (void *__restrict s1, const void *__restrict s2, size_t n) 
{
  int value = n;
  int loop_var;
  const char *in = s2;
  char *out = s1;
  int count;
  int m0,m1,m2,m3;

 /* This code currently give a stall for any value with a 1->2 in the low 5
    bits, i.e.  1,2, 33,34 ? not acceptable! */

  switch (value & 0x1f)
    {
    case (0):
      break;
    case (1):
      MOVE_1_BYTE (in, out);
      break;
    case (2):
      MOVE_2_BYTES (in, out);
      break;
    case (3):
      MOVE_3_BYTES (in, out);
      break;
    case (4):
      MOVE_4_BYTES (in, out);
      break;
    case (5):
      MOVE_5_BYTES (in, out);
      break;
    case (6):
      MOVE_6_BYTES (in, out);
      break;
    case (7):
      MOVE_7_BYTES (in, out);
      break;
    case (8):
      MOVE_8_BYTES (in, out);
      break;
    case (9):
      MOVE_9_BYTES (in, out);
      break;
    case (10):
      MOVE_10_BYTES (in, out);
      break;
    case (11):
      MOVE_11_BYTES (in, out);
      break;
    case (12):
      MOVE_12_BYTES (in, out);
      break;
    case (13):
      MOVE_9_BYTES (in, out);
      MOVE_4_BYTES (in, out);
      break;
    case (14):
      MOVE_12_BYTES (in, out);
      MOVE_2_BYTES (in, out);
      break;
    case (15):
      MOVE_11_BYTES (in, out);
      MOVE_4_BYTES (in, out);
      break;
    case (16):
      MOVE_16_BYTES (in, out);
      break;
    case (17):
      MOVE_11_BYTES (in, out);
      MOVE_6_BYTES (in, out);
      break;
    case (18):
      MOVE_9_BYTES (in, out);
      MOVE_9_BYTES (in, out);
      break;
    case (19):
      MOVE_16_BYTES (in, out);
      MOVE_3_BYTES (in, out);
      break;
    case (20):
      MOVE_16_BYTES (in, out);
      MOVE_4_BYTES (in, out);
      break;
    case (21):
      MOVE_16_BYTES (in, out);
      MOVE_5_BYTES (in, out);
      break;
    case (22):
      MOVE_16_BYTES (in, out);
      MOVE_6_BYTES (in, out);
      break;
    case (23):
      MOVE_16_BYTES (in, out);
      MOVE_7_BYTES (in, out);
      break;
    case (24):
      MOVE_16_BYTES (in, out);
      MOVE_8_BYTES (in, out);
      break;
    case (25):
      MOVE_16_BYTES (in, out);
      MOVE_9_BYTES (in, out);
      break;
    case (26):
      MOVE_16_BYTES (in, out);
      MOVE_10_BYTES (in, out);
      break;
    case (27):
      MOVE_16_BYTES (in, out);
      MOVE_11_BYTES (in, out);
      break;
    case (28):
      MOVE_16_BYTES (in, out);
      MOVE_8_BYTES (in, out);
      MOVE_4_BYTES (in, out);
      break;
    case (29):
      MOVE_16_BYTES (in, out);
      MOVE_9_BYTES (in, out);
      MOVE_4_BYTES (in, out);
      break;
    case (30):
      MOVE_16_BYTES (in, out);
      MOVE_12_BYTES (in, out);
      MOVE_2_BYTES (in, out);
      break;
    case (31):
      MOVE_16_BYTES (in, out);
      MOVE_11_BYTES (in, out);
      MOVE_4_BYTES (in, out);
      break;
    }

  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies.  */

  count = value >> 5;

  for (loop_var = 0; loop_var < count; loop_var++)
    MOVE_32_BYTES (in, out);
}


/* Exposed interface.  */

#ifndef __LMP_ARCH_BMI__

void
__long_int_memcpy (void *__restrict s1, const void *__restrict s2, size_t n)
{
  __int_memcpy (s1, s2, n);
}

#endif /* !__LMP_ARCH_BMI__ */

void
__wrd_memcpy (void *__restrict s1, const void *__restrict s2, size_t n)
{
  __shrt_int_memcpy (s1, s2, n);
}

void
__byt_memcpy (void *__restrict s1, const void *__restrict s2, size_t n)
{
  __byte_memcpy (s1, s2, n);
}
