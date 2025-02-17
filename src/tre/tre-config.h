/* lib/tre-config.h.  Generated from tre-config.h.in by configure.  */
/* tre-config.h.in.  This file has all definitions that are needed in
   `tre.h'.  Note that this file must contain only the bare minimum
   of definitions without the TRE_ prefix to avoid conflicts between
   definitions here and definitions included from somewhere else. */

/*
   HE: this first section is identical to the "tre-config.h" in R's source code.
*/

/* Define if you want to enable approximate matching functionality. */
#define TRE_APPROX 1

/* Define to enable multibyte character set support. */
#define TRE_MULTIBYTE 1

/* Define to a field in the regex_t struct where TRE should store a pointer to
   the internal tre_tnfa_t structure */
#define TRE_REGEX_T_FIELD value

/* Define if you want TRE to use alloca() instead of malloc() when allocating
   memory needed for regexec operations. */
/* #define TRE_USE_ALLOCA 1 */

/* Define to enable wide character (wchar_t) support. */
#define TRE_WCHAR 1

/* TRE version string. */
#define TRE_VERSION "0.8.0"


/*
   HE: add additional definitions used in the TRE library that
      are always defined in the R source at:
      src/gnuwin32/fixed/h/config.h
*/

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define to 1 if you have the 'wchar_t' type. (For intl) */
#define HAVE_WCHAR_T 1

/* Define to 1 if you have the `mbrtowc' function. */
#define HAVE_MBRTOWC 1

/* Define to 1 if the system has the type `mbstate_t'. */
#define HAVE_MBSTATE_T 1

/* Define to 1 if you have the `wcstombs` function. */
#define HAVE_WCSTOMBS 1 

/* Define to 1 if you have the <wctpye.h> header file. */
#define HAVE_WCTYPE_H 1
#include <wctype.h>

/* Define to 1 if you have the `wctype` function. */
#define HAVE_WCTYPE 1

/* Define to 1 if the system has type `mbstate_t` */
#define HAVE_MBSTATE_T 1

/* Define to 1 if you have the `iswctype` function. */
#define HAVE_ISWCTYPE 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1