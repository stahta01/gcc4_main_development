#include  "__fixunssfsi.h"


#if LIBGCC2_HAS_SF_MODE
/* Reenable the normal types, in case limits.h needs them.  */
#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double
#undef MIN
#undef MAX
#include <limits.h>
inline __attribute__ ((__always_inline__))
UWtype
__fixunssfSI (SFtype a)
{

  if (a >= - (SFtype) Wtype_MIN)
    return (Wtype) (a + Wtype_MIN) - Wtype_MIN;
  return (Wtype) a;
}
#endif

