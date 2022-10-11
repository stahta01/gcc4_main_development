/* Specialized variants of memcpy called directly from compiled code.  */

extern void
__long_int_memcpy (void *__restrict s1, const void *__restrict s2, size_t n) ;

extern void
__wrd_memcpy (void *__restrict s1, const void *__restrict s2, size_t n);

extern void
__byt_memcpy (void *__restrict s1, const void *__restrict s2, size_t n);
