#include "stringutil.h" //do_translate_char
#include "widestring.h" //RwtransChar
#include "regexp.h"

/* compile regex for pattern, return 0 if valid, 1 if invalid.
 * rgx_empty is a pointer to an empty regex_t struct
 * flags are the regex flags to use (e.g., REG_EXTENDED|REG_NOSUB)
 * specify the ttype_t of the pattern
*/
// int init_regex(SEXP ndl, regex_t *rgx_empty, int flags, ttype_t tt, R_xlen_t idx) {
int init_regex(SEXP ndl, struct RegexInfo *rgxo, R_xlen_t idx) {
    char errbuff[1001];
    int reti;
    const void *vmax = vmaxget();
    if (rgxo->tt == use_wchar) {
        int err;
        const wchar_t *ndl_w = RwtransChar(ndl, &err);
        if (err) {
            Riconv_warning(err, idx, 0); // 0 for needle
            return 1;
        }
        reti = tre_regwcomp(&rgxo->rgx, ndl_w, rgxo->flags);
        vmaxset(vmax); // rm space potentially allocated by R_alloc
        if (reti != 0) {
            tre_regerror(reti, &rgxo->rgx, errbuff, sizeof(errbuff));
            warning("invalid regular expression '%ls': %s", ndl_w, errbuff);
            return 1;
        }
    } else {
        const char *ndl_c = do_translate_char(ndl, rgxo->tt);
        reti = tre_regcompb(&rgxo->rgx, ndl_c, rgxo->flags);
        vmaxset(vmax); // rm space potentially allocated by R_alloc
        if (reti != 0) {
            tre_regerror(reti, &rgxo->rgx, errbuff, sizeof(errbuff));
            warning("invalid regular expression '%s': %s", ndl_c, errbuff);
            return 1;
        }
    }
    return 0;
}


/* test if regex finds  match in string */
int strrgx(const char *str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regexecb(rgx, str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}


/* test if regex finds  match in wide string */
int wstrrgx(const wchar_t *str, regex_t *rgx) {
    char errbuff[1001];
    int reti = tre_regwexec(rgx, str, 0, NULL, 0);
    if (reti == 0)
        return 1;
    if (reti != REG_NOMATCH) {
        tre_regerror(reti, rgx, errbuff, sizeof(errbuff));
        warning("regex match failed with error: %s", errbuff);
    }
    return 0;
}