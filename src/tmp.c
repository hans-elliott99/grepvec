SEXP C_grepvec(SEXP needles,
               SEXP haystck,
               SEXP ignorecase,
               SEXP fixed,
               SEXP usebytes,
               SEXP invert,
               SEXP matchrule,
               SEXP keepdim) {
    const R_xlen_t Nn = XLENGTH(needles);
    const R_xlen_t Nh = XLENGTH(haystck);
    const int fxd = asInteger(fixed);
    const int bytes = asInteger(usebytes);
    const int inv = asInteger(invert);
    const int mrule = asInteger(matchrule);
    const int keep = asInteger(keepdim);
    int rgx_flags = REG_EXTENDED|REG_NOSUB;
    if (asInteger(ignorecase)) rgx_flags |= REG_ICASE;
    /*initial length of match vector for each needle*/
    const R_xlen_t Nm = (!keep && mrule != RETURNALL) ? 1 : Nh;

    /*determine encoding of inputs*/
    ttype_t tt = use_char;
    if (!bytes)
        tt = get_ttype(needles, haystck, fxd);
    if (tt == use_wchar) {
        Rprintf("using wchar\n");
    } else if (tt == use_utf8) {
        Rprintf("using utf8\n");
    } else if (tt == use_native) {
        Rprintf("using native\n");
    } else {
        Rprintf("using char\n");
    }

    /*result vector - list of integer vectors*/
    SEXP output = PROTECT(allocVector(VECSXP, Nn));
    /*cache for translated haystacks*/
    init_str_cache(&string_cache, Nh, tt);

    R_xlen_t i, j, nmatch;
    int skip, res;
    char *ndl_s = NULL;
    regex_t rgx;
    SEXP indices;
    /*
        iterate and compare compiled regex j with string i
    */
    for (j=0; j < Nn; ++j) {
        if ((j + 1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        skip = (fxd) ? 0 : init_regex(STRING_ELT(needles, j), &rgx, rgx_flags, tt);
        if (skip || STRING_ELT(needles, j) == NA_STRING) {
            SET_VECTOR_ELT(output, j, allocVector(INTSXP, 0));
            continue;
        }
        indices = PROTECT(allocVector(INTSXP, Nm));
        if (fxd)
            ndl_s = (char *) do_translate_char(STRING_ELT(needles, j), tt);

        nmatch = 0;   // num matches for pattern j
        for (i=0; i < Nh; ++i) {
            if (STRING_ELT(haystck, i) == NA_STRING) {
                continue;
            }
            update_str_cache(STRING_ELT(haystck, i), &string_cache, i);
            if (fxd) {
                res = (strstr(string_cache.arr[i], ndl_s) != NULL);
            } else {
                res = (tt == use_wchar) ? wstrrgx(&string_cache.warr[i], &rgx):
                                          strrgx(&string_cache.arr[i], &rgx);
            }
            if (res ^ inv) {
                INTEGER(indices)[nmatch++] = i + 1; // R's idx for current hay
                if (mrule == RETURNFIRST) break;
            }
        }
        if (!fxd) tre_regfree(&rgx);
        if (keep) {
            for (i=nmatch; i < Nh; ++i) INTEGER(indices)[i] = NA_INTEGER;
            nmatch = Nh;
        }
        // rm extra space allocated to match vec
        SETLENGTH(indices, nmatch);
        SET_VECTOR_ELT(output, j, indices);
        UNPROTECT(1); // indices
    }

    UNPROTECT(1); // output
    return output;
}