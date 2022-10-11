/* Specialized variants of memset called directly from compiled code.  */

extern void
__long_int_memset (void *__restrict s, int c, size_t n);

extern void
__wrd_memset (void *__restrict s, int c, size_t n);

extern void
__byt_memset (void *__restrict s, int c, size_t n);
