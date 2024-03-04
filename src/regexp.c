#include "stringutil.h" //do_translate_char
#include "widestring.h" //RwtransChar
#include "regexp.h"

/* compile regex for pattern if valid regex, return 0 if valid, 1 if invalid.
 * rgx_empty is a pointer to an empty regex_t struct
 * flags are the regex flags to use (e.g., REG_EXTENDED|REG_NOSUB)
*/
int init_regex(SEXP ndl, regex_t *rgx_empty, int flags, ttype_t tt) {
    char errbuff[1001];
    int reti;

    const void *vmax = vmaxget();
    if (tt == use_wchar) {
        const wchar_t *ndl_w = RwtransChar(ndl);
        reti = tre_regwcomp(rgx_empty, ndl_w, flags);
        if (reti != 0) {
            tre_regerror(reti, rgx_empty, errbuff, sizeof(errbuff));
            warning("invalid regular expression '%ls': %s", ndl_w, errbuff);
            return 1;
        }
    } else {
        const char *ndl_c = do_translate_char(ndl, tt);
        reti = tre_regcompb(rgx_empty, ndl_c, flags);
        if (reti != 0) {
            tre_regerror(reti, rgx_empty, errbuff, sizeof(errbuff));
            warning("invalid regular expression '%s': %s", ndl_c, errbuff);
            return 1;
        }
    }
    vmaxset(vmax); // rm space potentially allocated by R_alloc in char transl
    return 0;
}


int strrgx(const char **str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regexecb(rgx, *str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}

/* test if regex finds a match in the string */
int wstrrgx(const wchar_t **str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regwexec(rgx, *str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}