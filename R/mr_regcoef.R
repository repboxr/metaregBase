# Helper function for regcoef (repdb table)

parmest_output_to_regcoef = function(mr, variant=NULL,  prefix, artid = basename(mr$project_dir),missing.step = c("stop","ignore")[1]) {
  restore.point("parmest_output_to_regcoef")
  co = mr_agg_stata_parmest(mr,file_prefix = prefix, missing.step=missing.step)
  if (!is.null(variant)) co$variant = rep(variant, NROW(co))
  co = ct_to_regcoef(co, "stata", variant=variant, artid=artid)
  co
}

#' regvar is only needed if cterm shall be generated in R
ct_to_regcoef = function(ct, lang="stata", variant=NULL, artid=NULL, regvar=NULL) {
  restore.point("ct_to_regcoef")
  if (NROW(ct)==0) return(NULL)

  if (!is.null(variant) & !has.col(ct, "variant")) {
    ct$variant = variant
  }
  if (!is.null(artid) & !has.col(ct, "artid")) {
    ct$artid = artid
  }

  # Better overwrite cterm since sometimes it has NA values
  if (lang=="stata" & all(has.col(ct, c("var","label")))) {
    ct$cterm = canonical.stata.output.terms(ct$var,ct$label)
  }
  if (lang=="r" & !has.col(ct, "cterm")) {
    if (is.null(regvar)) {
      stop("Computation of cterm for R requires passing regvar.")
    }
    ct$cterm = canonical.r.output.terms(ct$term,regvar,rcmd = "")
  }


  if (!has.col(ct,"shown_term")) {
    if (lang=="stata") {
      ct$shown_term = ct$var
    } else if (lang=="r") {
      ct$shown_term = ct$term
    }
  }

  # Possibly needs some update if non-broom output is used...
  if (lang=="r") {
    new.cols = c("coef","se","t","p","ci_low","ci_up")
    old.cols = c("estimate","std.error","statistic","p.value","conf.low","conf.high")
    use = which(!new.cols %in% colnames(ct))
    if (length(use)>0) {
      ct = rename.col(ct, old.cols[use], new.cols[use])
    }
  }

  if (!"label" %in% colnames(ct)) {
    ct$label = rep("", NROW(ct))
  }

  if (!"p" %in% colnames(ct)) {
    if ("p_val" %in% colnames(ct)) {
      ct = rename.col(ct, "p_val","p")
    } else if ("p.value" %in% colnames(ct)) {
      ct = rename.col(ct, "p.value","p")
    }
  }
  if (!"t" %in% colnames(ct)) {
    if ("t_val" %in% colnames(ct)) {
      ct = rename.col(ct, "t_val","t")
    } else if ("z" %in% colnames(ct)) {
      ct = rename.col(ct, "z","t")
    }
  }

  if (lang == "stata") {
    ct = regcoef_normalize_dropped_coef(ct, lang)
  }
  ct
}


regcoef_normalize_dropped_coef = function(co, lang="stata") {
  restore.point("regcoef_normalize_dropped_coef")
  if (lang=="r") {
    return(co)
  }
  lhs = str.left.of(co$shown_term,".", not.found="")
  co$is_dropped =
    (is.na(co$coef)) |
    (is.na(co$se) & is.true(co$coef==0)) |
    (co$se==0 & co$coef==0 & (is.na(co$t) | has.substr(lhs,"b")))

  co$coef[co$is_dropped] = NA_real_
  co$se[co$is_dropped] = NA_real_
  co
}


coef_diff_summary = function(diff_tab, compare_what=c("all","coef"), problem="") {
  if (NROW(diff_tab)==0) return(NULL)

  if (length(compare_what)>1) {
    res = lapply(compare_what, function(cw) {
      coef_diff_summary(diff_tab, cw, problem=problem)
    }) %>% bind_rows()
    return(res)
  }

  restore.point("coef_diff_summary")


  if (compare_what=="all") {
    sum = diff_tab %>%
      group_by(artid, step) %>%
      summarize(
        variant1 = first(na.omit(variant_1)),
        variant2 = first(na.omit(variant_2)),
        compare_what = "all",
        identical = all(abs_err_coef == 0 & abs_err_se == 0),
        identical_share = mean(is.true(abs_err_coef == 0 & abs_err_se == 0)),
        within_1pc_share = mean(rel_err_coef <=0.01 & rel_err_se <=0.01, na.rm = TRUE),
        within_1pm_share = mean(rel_err_coef <=0.001 & rel_err_se <=0.001, na.rm = TRUE),
        max_rel_diff = max_empty_na(c(rel_err_coef, rel_err_se)),
        max_deviation = max_empty_na(pmin(c(rel_err_coef, rel_err_se),c(abs_err_coef, abs_err_se))),
        ref_level_differ = any(ref_level_differs)
      ) %>%
      ungroup()
  } else if (compare_what=="coef") {
    sum = diff_tab %>%
      group_by(artid, step) %>%
      summarize(
        variant1 = first(na.omit(variant_1)),
        variant2 = first(na.omit(variant_2)),
        compare_what = "coef",
        identical = all(abs_err_coef == 0),
        identical_share = mean(is.true(abs_err_coef == 0)),
        within_1pc_share = mean(rel_err_coef <=0.01, na.rm = TRUE),
        within_1pm_share = mean(rel_err_coef <=0.001, na.rm = TRUE),
        max_rel_diff = max_empty_na(c(rel_err_coef), na.rm=TRUE),
        max_deviation = max_empty_na(pmin(c(rel_err_coef),c(abs_err_coef)) , na.rm=TRUE),
        ref_level_differ = any(ref_level_differs)
      ) %>%
      ungroup()

  } else {
    stop(paste0("compare_what = '", compare_what,"' is not implemented."))
  }

  sum$problem = rep(problem, length.out = NROW(sum))

  sum
}

coef_diff_table = function(co1, co2, check.ref.levels = TRUE) {
  restore.point("regcoef_check_same")

  if (is.null(co1) | is.null(co2)) return(NULL)

  # Match results
  cod = full_join(co1, co2, by=c("cterm","step","artid"), suffix=c("_1","_2"))

  # Ignore coefficients that are missing in both co1 and co2
  cod = cod %>%
    filter(! (is.na(coef_1) & is.na(coef_2)))

  # Should be TRUE whenever co1 and co2 come from different regression commands
  # We try to correct for the fact that they may pick different reference levels
  # when creating the dummy variables
  if (check.ref.levels) {
    cod = cod %>%
      mutate(
        is_ia = has.substr(cterm ,":"),
        is_factor = has.substr(cterm, "="),
        factor_group = stringi::stri_replace_all_regex(paste0(cterm,":"), "=([^\\:]*):",":") %>% str.remove.ends(right=1)
      ) %>%
      group_by(step, factor_group) %>%
      mutate(
        # We will normalize reference levels to those of coef_1
        # Note that rows where both coef_1 and coef_2
        # is NA are removed

        # Reference levels differ if some coef_2 is NA
        ref_level_differs = is_factor & any(is.na(coef_2)),

        # We codute the offset for coef_2
        offset.2 = ifelse(ref_level_differs, -coef_1[first(which(is.na(coef_2)))],0),
        num_diff_ref_coef_2 = sum(is.na(coef_2))
      ) %>%
      ungroup() %>%
      mutate(
        # Replace NA by 0 for coef_2 if ref_level_differs
        coef_2 = ifelse(is.na(coef_2) & ref_level_differs, 0, coef_2),

        # We keep the SE currently as NA as I don't know how
        # to adapt them
        # se.2 = ???

        # Now adapt all coef_2 by offset.2 if ref level differs
        coef_2 = ifelse(ref_level_differs, coef_2 + offset.2, coef_2)
      )

    # Adapt (Intercept) if there are different reference levels
    cod = cod %>%
      group_by(step) %>%
      mutate(
        ref_level_differs = ifelse(cterm=="(Intercept)" & any(ref_level_differs), any(ref_level_differs, na.rm=TRUE), ref_level_differs),
        offset.2.intercept =  ifelse(cterm=="(Intercept)" & any(ref_level_differs), -sum(unique(offset.2), na.rm=TRUE), offset.2),
        coef_2 = ifelse(cterm=="(Intercept)" & any(ref_level_differs), coef_2 + offset.2.intercept, coef_2)
      )
  } else {
    cod$ref_level_differs = rep(FALSE, NROW(cod))
  }

  # Compute absolute and relative differences between coeficients and se
  cod = cod %>%
    mutate(
      abs_err_coef = abs(coef_1-coef_2),
      abs_err_se = abs(se_1-se_2),
      rel_err_coef = abs_err_coef / (0.5*(abs(coef_1)+abs(coef_2))),
      rel_err_se = abs_err_se / (0.5*(abs(se_1)+abs(se_2))),

      rel_within_1pc_coef = rel_err_coef < 0.01,
      rel_within_1pc = rel_err_coef < 0.01 & rel_err_se < 0.01,
      identical_coef = coef_1 == coef_2,
      identical = identical_coef & se_1 == se_2
    )


  # If Stata uses a dummy set like month1 month2 month3 ...
  # we cannot repair different dummy dropping between Stata and R
  # we just add an indicator
  cod = cod %>%
    group_by(step) %>%
    mutate(
      step_refs_differ =
        any(ref_level_differs) |
        any( !is.na(coef_1) & is.na(coef_2) )
    ) %>%
    ungroup()

  cod = cod %>%
    select(artid, step, cterm, identical, identical_coef, everything())

  cod
}

max_empty_na = function(x, na.rm=TRUE) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  if (length(x)==0) return(NA_real_)
  max(x)
}

