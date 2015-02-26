#ifndef MACHDEPS_H
#define MACHDEPS_H
#define SIZEOF_HSCHAR           SIZEOF_WORD32
#define ALIGNMENT_HSCHAR        ALIGNMENT_WORD32
#define SIZEOF_HSINT            SIZEOF_VOID_P
#define ALIGNMENT_HSINT         ALIGNMENT_VOID_P
#define SIZEOF_HSWORD           SIZEOF_VOID_P
#define ALIGNMENT_HSWORD        ALIGNMENT_VOID_P
#define SIZEOF_HSDOUBLE         SIZEOF_DOUBLE
#define ALIGNMENT_HSDOUBLE      ALIGNMENT_DOUBLE
#define SIZEOF_HSFLOAT          SIZEOF_FLOAT
#define ALIGNMENT_HSFLOAT       ALIGNMENT_FLOAT
#define SIZEOF_HSPTR            SIZEOF_VOID_P
#define ALIGNMENT_HSPTR         ALIGNMENT_VOID_P
#define SIZEOF_HSFUNPTR         SIZEOF_VOID_P
#define ALIGNMENT_HSFUNPTR      ALIGNMENT_VOID_P
#define SIZEOF_HSSTABLEPTR      SIZEOF_VOID_P
#define ALIGNMENT_HSSTABLEPTR   ALIGNMENT_VOID_P
#define SIZEOF_INT8             SIZEOF_CHAR
#define ALIGNMENT_INT8          ALIGNMENT_CHAR
#define SIZEOF_WORD8            SIZEOF_UNSIGNED_CHAR
#define ALIGNMENT_WORD8         ALIGNMENT_UNSIGNED_CHAR
#define SIZEOF_INT16            SIZEOF_SHORT
#define ALIGNMENT_INT16         ALIGNMENT_SHORT
#define SIZEOF_WORD16           SIZEOF_UNSIGNED_SHORT
#define ALIGNMENT_WORD16        ALIGNMENT_UNSIGNED_SHORT
#define SIZEOF_INT32            SIZEOF_INT
#define ALIGNMENT_INT32         ALIGNMENT_INT
#define SIZEOF_WORD32           SIZEOF_UNSIGNED_INT
#define ALIGNMENT_WORD32        ALIGNMENT_UNSIGNED_INT
#if SIZEOF_LONG == 8
#define SIZEOF_INT64            SIZEOF_LONG
#define ALIGNMENT_INT64         ALIGNMENT_LONG
#define SIZEOF_WORD64           SIZEOF_UNSIGNED_LONG
#define ALIGNMENT_WORD64        ALIGNMENT_UNSIGNED_LONG
#elif HAVE_LONG_LONG && SIZEOF_LONG_LONG == 8
#define SIZEOF_INT64            SIZEOF_LONG_LONG
#define ALIGNMENT_INT64         ALIGNMENT_LONG_LONG
#define SIZEOF_WORD64           SIZEOF_UNSIGNED_LONG_LONG
#define ALIGNMENT_WORD64        ALIGNMENT_UNSIGNED_LONG_LONG
#else
#error Cannot find a 64bit type.
#endif
#ifndef WORD_SIZE_IN_BITS
#if SIZEOF_HSWORD == 4
#define WORD_SIZE_IN_BITS       32
#define WORD_SIZE_IN_BITS_FLOAT 32.0
#else
#define WORD_SIZE_IN_BITS       64
#define WORD_SIZE_IN_BITS_FLOAT 64.0
#endif
#endif
#ifndef TAG_BITS
#if SIZEOF_HSWORD == 4
#define TAG_BITS                2
#else
#define TAG_BITS                3
#endif
#endif
#define TAG_MASK ((1 << TAG_BITS) - 1)
#endif /* MACHDEPS_H */
