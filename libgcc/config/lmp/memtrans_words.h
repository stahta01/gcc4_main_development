


/* assumming hardware implements a 4 long word copy, this should only be used if that hardware isn't available*/

#undef MOVE_32_WORDS
#define MOVE_32_WORDS(in, out) do {                 \
  __asm__ __volatile__ ("":::"memory");		\
  m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  m3 = in [7];                  \
  out [4] = m0;                 \
  out [5] = m1;                 \
  out [6] = m2;                 \
  out [7] = m3;                 \
 INST_BARRIER 			\
  m0 = in [8];                  \
  m1 = in [9];                  \
  m2 = in [10];                  \
  m3 = in [11];                  \
  out [8] = m0;                 \
  out [9] = m1;                 \
  out [10] = m2;                 \
  out [11] = m3;                 \
 INST_BARRIER 			\
  m0 = in [12];                  \
  m1 = in [13];                  \
  m2 = in [14];                  \
  m3 = in [15];                  \
  out [12] = m0;                 \
  out [13] = m1;                 \
  out [14] = m2;                 \
  out [15] = m3;                 \
 INST_BARRIER 			\
  m0 = in [16];                  \
  m1 = in [17];                  \
  m2 = in [18];                  \
  m3 = in [19];                  \
  out [16] = m0;                 \
  out [17] = m1;                 \
  out [18] = m2;                 \
  out [19] = m3;                 \
 INST_BARRIER 			\
  m0 = in [20];                  \
  m1 = in [21];                  \
  m2 = in [22];                  \
  m3 = in [23];                  \
  out [20] = m0;                 \
  out [21] = m1;                 \
  out [22] = m2;                 \
  out [23] = m3;                 \
 INST_BARRIER 			\
  m0 = in [24];                  \
  m1 = in [25];                  \
  m2 = in [26];                  \
  m3 = in [27];                  \
  out [24] = m0;                 \
  out [25] = m1;                 \
  out [26] = m2;                 \
  out [27] = m3;                 \
 INST_BARRIER 			\
  m0 =	in [28];                  \
  m1 = in [29];                  \
  m2 = in [30];                  \
  m3 = in [31];                  \
  out [28] = m0;                 \
  out [29] = m1;                 \
  out [30] = m2;                 \
  out [31] = m3;                 \
 INST_BARRIER 			\
  in += 32;			\
  out += 32;			\
} while(0)





#undef MOVE_16_WORDS
#define MOVE_16_WORDS(in, out) do {                 \
 INST_BARRIER 			\
 m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  m3 = in [7];                  \
  out [4] = m0;                 \
  out [5] = m1;                 \
  out [6] = m2;                 \
  out [7] = m3;                 \
 INST_BARRIER 			\
  m0 = in [8];                  \
  m1 = in [9];                  \
  m2 = in [10];                  \
  m3 = in [11];                  \
  out [8] = m0;                 \
  out [9] = m1;                 \
  out [10] = m2;                 \
  out [11] = m3;                 \
 INST_BARRIER 			\
  m0 = in [12];                  \
  m1 = in [13];                  \
  m2 = in [14];                  \
  m3 = in [15];                  \
  out [12] = m0;                 \
  out [13] = m1;                 \
  out [14] = m2;                 \
  out [15] = m3;                 \
 INST_BARRIER 			\
  in += 16;                      \
  out += 16;                     \
} while(0)

#undef MOVE_12_WORDS
#define MOVE_12_WORDS(in, out) do {                \
 INST_BARRIER 			\
 m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  m3 = in [7];                  \
  out [4] = m0;                 \
  out [5] = m1;                 \
  out [6] = m2;                 \
  out [7] = m3;                 \
 INST_BARRIER 			\
  m0 = in [8];                  \
  m1 = in [9];                  \
  m2 = in [10];                  \
  m3 = in [11];                  \
  out [8] = m0;                 \
  out [9] = m1;                 \
  out [10] = m2;                 \
  out [11] = m3;                 \
 INST_BARRIER 			\
  in += 12;                      \
  out += 12;                     \
} while(0)




#undef MOVE_11_WORDS
#define MOVE_11_WORDS(in, out) do {              \
 INST_BARRIER 			\
  m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  m3 = in [7];                  \
  out [4] = m0;                 \
  out [5] = m1;                 \
  out [6] = m2;                 \
  out [7] = m3;                 \
 INST_BARRIER 			\
  m0 = in [8];                  \
  m1 = in [9];                  \
  m2 = in [10];                 \
  out [8] = m0;                 \
  out [9] = m1;                 \
  out [10] = m2;                \
 INST_BARRIER 			\
  in += 11;                     \
  out += 11;                    \
} while(0)



#undef MOVE_10_WORDS
#define MOVE_10_WORDS(in, out) do {                 \
 INST_BARRIER 			\
 m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  m3 = in [7];                  \
  out [4] = m0;                 \
  m0 = in [8];                  \
  out [5] = m1;                 \
  m1 = in [9];                  \
  out [6] = m2;                 \
  out [7] = m3;                 \
  out [8] = m0;                  \
  out [9] = m1;                  \
 INST_BARRIER 			\
  in += 10;                      \
  out += 10;                    \
} while(0)







#undef MOVE_9_WORDS
#define MOVE_9_WORDS(in, out) do {            \
 INST_BARRIER 			\
  m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  m3 = in [7];                  \
  out [4] = m0;                 \
  out [5] = m1;                 \
  out [6] = m2;                 \
  out [7] = m3;                 \
 INST_BARRIER 			\
  m0 = in [8];                  \
  out [8] = m0;                 \
  in += 9;                      \
  out += 9;                     \
} while(0)



#undef MOVE_8_WORDS
#define MOVE_8_WORDS(in, out) do {          \
 INST_BARRIER 			\
  m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  m3 = in [7];                  \
  out [4] = m0;                 \
  out [5] = m1;                 \
  out [6] = m2;                 \
  out [7] = m3;                 \
 INST_BARRIER 			\
  in += 8;                      \
  out += 8;                     \
} while(0)





/* Move memory as long words, long word aligned */

#undef MOVE_7_WORDS
#define MOVE_7_WORDS(in, out) do {                 \
  m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  m1 = in [5];                  \
  m2 = in [6];                  \
  out [4] = m0;                 \
  out [5] = m1;                 \
  out [6] = m2;                 \
  in += 7;                      \
  out += 7;                     \
} while(0)



#undef MOVE_6_WORDS
#define MOVE_6_WORDS(in, out) do {                \
 INST_BARRIER 			\
  m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
  out [0] = m0;                 \
 INST_BARRIER 			\
  m0 = in [4];                  \
  out [1] = m1;                 \
 INST_BARRIER 			\
  m1 = in [5];                  \
  out [2] = m2;                 \
  out [3] = m3;                 \
  out [4] = m0;                 \
  out [5] = m1;                 \
 INST_BARRIER 			\
  in += 6;                      \
  out += 6;                     \
} while(0)



#undef MOVE_5_WORDS
#define MOVE_5_WORDS(in, out) do {                \
 INST_BARRIER 			\
 m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  m3 = in [3];                  \
 INST_BARRIER 			\
  out [0] = m0;                 \
  m0 = in [4];                  \
 INST_BARRIER 			\
  out [1] = m1;                 \
  out [2] = m2;                 \
  out [3] = m3;                 \
  out [4] = m0;                 \
 INST_BARRIER 			\
  in += 5;                      \
  out += 5;                     \
} while(0)




#undef MOVE_4_WORDS
#define MOVE_4_WORDS(in, out) do { 		\
  m0 = in [0];			\
  m1 = in [1];			\
  m2 = in [2];			\
  m3 = in [3];			\
  out [0] = m0;			\
  out [1] = m1;			\
  out [2] = m2;			\
  out [3] = m3;			\
  in += 4;			\
  out += 4;			\
} while(0)

/* These kernels should be avoided, though only the cases 2 and 1 incur latency overhead */

#undef MOVE_3_WORDS
#define MOVE_3_WORDS(in, out) do {                 \
 INST_BARRIER 			\
  m0 = in [0];                  \
  m1 = in [1];                  \
  m2 = in [2];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
  out [2] = m2;                 \
 INST_BARRIER 			\
  in += 3;                      \
  out += 3;                     \
} while(0)




#undef MOVE_2_WORDS
#define MOVE_2_WORDS(in, out) do {                 \
 INST_BARRIER 			\
  m0 = in [0];                  \
  m1 = in [1];                  \
  out [0] = m0;                 \
  out [1] = m1;                 \
 INST_BARRIER 			\
  in += 2;                      \
  out += 2;                     \
} while(0)



#undef MOVE_1_WORD
#define MOVE_1_WORD(in, out) do {                 \
 INST_BARRIER 			\
  m0 = in [0];                  \
  out [0] = m0;                 \
 INST_BARRIER 			\
  in += 1;                      \
  out += 1;                    \
} while(0)

 
