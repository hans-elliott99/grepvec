#include <R.h>
#include <Rinternals.h>
#include "tre/tre.h"


typedef int regex_t;
typedef int pcre;
typedef int pcre_extra;

SEXP do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ind, ans;
    regex_t reg;
    R_xlen_t i, j, n;
    int nmatches = 0, ov[3], rc;
    int igcase_opt, value_opt, perl_opt, fixed_opt, useBytes, invert;
    const char *spat = NULL;
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;
    Rboolean use_UTF8 = FALSE, use_WC = FALSE;
    const void *vmax;
    int nwarn = 0;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    invert = asLogical(CAR(args));
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (value_opt == NA_INTEGER) value_opt = 0;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (invert == NA_INTEGER) invert = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    if (!isString(pat) || LENGTH(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (LENGTH(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    if (!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = XLENGTH(text);
    if (STRING_ELT(pat, 0) == NA_STRING) {
	    if (value_opt) {
	        SEXP nmold = PROTECT(getAttrib(text, R_NamesSymbol));
	        PROTECT(ans = allocVector(STRSXP, n));
	        for (i = 0; i < n; i++)  SET_STRING_ELT(ans, i, NA_STRING);
	        if (!isNull(nmold))
	    	setAttrib(ans, R_NamesSymbol, duplicate(nmold));
	        UNPROTECT(2); /* ans, nmold */
	    } else {
	        ans = allocVector(INTSXP, n);
	        for (i = 0; i < n; i++)  INTEGER(ans)[i] = NA_INTEGER;
	    }
	    return ans;
    }

    if (!useBytes) {
	Rboolean onlyASCII = IS_ASCII(STRING_ELT(pat, 0));
	if (onlyASCII)
	    for (i = 0; i < n; i++) {
	    	if(STRING_ELT(text, i) == NA_STRING) continue;
	    	if (!IS_ASCII(STRING_ELT(text, i))) {
	    	    onlyASCII = FALSE;
	    	    break;
	    	}
	    }
	    useBytes = onlyASCII;
    }
    if (!useBytes) {
	Rboolean haveBytes = IS_BYTES(STRING_ELT(pat, 0));
	if (!haveBytes)
	    for (i = 0; i < n; i++)
		if (IS_BYTES(STRING_ELT(text, i))) {
		    haveBytes = TRUE;
		    break;
		}
	    if(haveBytes) {
	        useBytes = TRUE;
	    }
    }
    if (!useBytes) {
	/* As from R 2.10.0 we use UTF-8 mode in PCRE in all MBCS locales */
	    if (IS_UTF8(STRING_ELT(pat, 0))) use_UTF8 = TRUE;
	    if (!use_UTF8)
	        for (i = 0; i < n; i++)
	    	    if (IS_UTF8(STRING_ELT(text, i))) {
	    	        use_UTF8 = TRUE;
	    	        break;
	    	    }
    }

    if (!fixed_opt) {
	    /* if we have non-ASCII text in a DBCS locale, we need to use wchar */
	    use_WC = use_UTF8; use_UTF8 = FALSE;
    }
    if (useBytes)
	    spat = CHAR(STRING_ELT(pat, 0));
    else if (use_WC) ; // has to be a regex
    else if (use_UTF8) {
	    spat = translateCharUTF8(STRING_ELT(pat, 0));
	    if (!utf8Valid(spat)) error(_("regular expression is invalid UTF-8"));
    } else {
	    spat = translateChar(STRING_ELT(pat, 0));
	    if (mbcslocale && !mbcsValid(spat))
	        error(_("regular expression is invalid in this locale"));
    }

    if (fixed_opt) ;
    } else {
	    int cflags = REG_NOSUB | REG_EXTENDED;
	    if (igcase_opt) cflags |= REG_ICASE;
	    if (!use_WC)
	        rc = tre_regcompb(&reg, spat, cflags);
	    else
	        rc = tre_regwcomp(&reg, wtransChar(STRING_ELT(pat, 0)), cflags);
	    if (rc) reg_report(rc, &reg, spat);
    }

    PROTECT(ind = allocVector(LGLSXP, n));
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(ind)[i] = 0;
	    if (STRING_ELT(text, i) != NA_STRING) {
	        const char *s = NULL;
	        if (useBytes)
	    	    s = CHAR(STRING_ELT(text, i));
	        else if (use_WC) ;
	        else if (use_UTF8) {
	    	    s = translateCharUTF8(STRING_ELT(text, i));
	    	    if (!utf8Valid(s)) {
	    	        if(nwarn++ < NWARN)
	    	    	warning(_("input string %d is invalid UTF-8"), i+1);
	    	        continue;
	    	    }
	        } else {
	    	    s = translateChar(STRING_ELT(text, i));
	    	    if (mbcslocale && !mbcsValid(s)) {
	    	        if(nwarn++ < NWARN)
	    	    	warning(_("input string %d is invalid in this locale"), i+1);
	    	        continue;
	    	    }
	        }
	        if (fixed_opt)
	    	    LOGICAL(ind)[i] = fgrep_one(spat, s, useBytes, use_UTF8, NULL) >= 0;
	        } else {
	    	    if (!use_WC)
	    	        rc = tre_regexecb(&reg, s, 0, NULL, 0);
	    	    else
	    	        rc = tre_regwexec(&reg, wtransChar(STRING_ELT(text, i)),
	    	    		      0, NULL, 0);
	    	    if (rc == 0) LOGICAL(ind)[i] = 1;
	        }
	    }
	    vmaxset(vmax);
	    if (invert ^ LOGICAL(ind)[i]) nmatches++;
    }

    if (fixed_opt);
    else tre_regfree(&reg);

    if (PRIMVAL(op)) {/* grepl case */
	    UNPROTECT(1); /* ind */
	    return ind;
    }

    if (value_opt) {
	    SEXP nmold = PROTECT(getAttrib(text, R_NamesSymbol)), nm;
	    PROTECT(ans = allocVector(STRSXP, nmatches));
	    for (i = 0, j = 0; i < n ; i++)
	        if (invert ^ LOGICAL(ind)[i])
	    	SET_STRING_ELT(ans, j++, STRING_ELT(text, i));
	    /* copy across names and subset */
	    if (!isNull(nmold)) {
	        nm = allocVector(STRSXP, nmatches);
	        for (i = 0, j = 0; i < n ; i++)
	    	if (invert ^ LOGICAL(ind)[i])
	    	    SET_STRING_ELT(nm, j++, STRING_ELT(nmold, i));
	        setAttrib(ans, R_NamesSymbol, nm);
	    }
	    UNPROTECT(2); /* ans, nmold */
    } else {
#ifdef LONG_VECTOR_SUPPORT
	if (n > INT_MAX) {
	    ans = allocVector(REALSXP, nmatches);
	    j = 0;
	    for (i = 0 ; i < n ; i++)
		if (invert ^ LOGICAL(ind)[i]) REAL(ans)[j++] = (double)(i + 1);
	} else
#endif
	{
	    ans = allocVector(INTSXP, nmatches);
	    j = 0;
	    for (i = 0 ; i < n ; i++)
		if (invert ^ LOGICAL(ind)[i])
		    INTEGER(ans)[j++] = (int) (i + 1);
	}
    }
    UNPROTECT(1); /* ind */
    return ans;
}
