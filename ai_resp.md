The label is not getting dropped by `haven` or by the cache.

What is happening is this:

`regcoef_so` / `regcoef_sb` are built from Stata output tables, and `ct_to_regcoef(..., lang="stata")` explicitly uses the Stata `label` column to rename `_I...` terms through `canonical.output.terms.stata.xi()`.

The R branch does not do that. After repair, `mrb_run_r_base()` rebuilds `regvar` from `cmdpart_to_regvar()`. That function only looks at the token `_Ix1_2` and the column class/distinct counts. It never reads `attr(dat[["_Ix1_2"]], "label")`. So `regvar$cterm` stays `_Ix1_2`.

Then `mrb_run_r_reg_step()` translates from that `regvar/regxvar`, so the generated R formula also uses `_Ix1_2`. Finally `cterm_of_r_coefs_fixest()` just mirrors the fixest term name, so `regcoef_rb` also stays `_Ix1_2`.

So the real issue is not I/O. It is that the Stata path canonicalizes `_I...` from labels, while the R/regvar path never does.

There is also a second bug visible in your `regcoef_so` printout: `x1=-1@874...`. That comes from `adapt.stata.prefix.notation()` doing a global `"." -> "@"` replacement. For xi labels with numeric levels, that corrupts decimal points and can later make those cterms look like they have prefixes.

I would fix it in two places:

1. make `adapt.stata.prefix.notation()` only convert actual Stata prefix dots, not decimal points
2. make `cmdpart_to_regvar()` canonicalize `_I...` variables from the dataset variable labels before it builds `regvar` / `regxvar`

Here are the concrete patches.

!MODIFICATION adapt.stata.prefix.notation in mrb_cterms.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_cterms.R"
function_name = "adapt.stata.prefix.notation"
description = "Stop converting decimal points in xi factor levels to @. Only Stata time-series prefix separators should become @."
----------------------------------------------------------------------------------------------------------------------------------

```r
adapt.stata.prefix.notation = function(cterm) {
  cterm = as.character(cterm)

  old_cterm = rep(NA_character_, length(cterm))
  while (isTRUE(any(old_cterm != cterm, na.rm = TRUE))) {
    old_cterm = cterm

    # Only convert dots that separate actual Stata prefixes from the next token,
    # e.g. L2.x -> L2@x, L.D.x -> L@D@x.
    # Do not touch decimal points in factor levels such as x1=-1.874.
    cterm = gsub(
      "(^|#|@)([LlFfDdSsOo][0-9]*)\\.",
      "\\1\\2@",
      cterm,
      perl = TRUE
    )
  }

  cterm
}
```

!END_MODIFICATION adapt.stata.prefix.notation in mrb_cterms.R

!MODIFICATION cmdpart_to_regvar in mrb_reg_tools.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_reg_tools.R"
function_name = "cmdpart_to_regvar"
description = "Canonicalize xi-generated _I variables from dataset labels when rebuilding regvar, so repaired runs and R translations use the same cterms as Stata parcels."
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
#' Create the regvar (vi) table strictly from the expanded cmdpart, opts_df, and se_info
cmdpart_to_regvar = function(cmdpart, dat, opts_df, se_info) {
  restore.point("cmdpart_to_regvar")

  # 1. Collect all terms mapped by role
  term_list = list()

  # Standard variables (dep, exo, endo, instr)
  v_df = cmdpart %>% filter(part == "v")
  if (nrow(v_df) > 0) {
    # Replace tag names with role names (depvar -> dep, others stay same)
    v_df$role = ifelse(v_df$tag == "depvar", "dep", v_df$tag)
    term_list[[1]] = tibble(ia_expr = v_df$content, role = v_df$role, option = "")
  }

  # Weights
  w_df = cmdpart %>% filter(part == "weight_var")
  if (nrow(w_df) > 0) {
    term_list[[2]] = tibble(ia_expr = w_df$content, role = "weight", option = "")
  }

  # Absorb (from reghdfe / areg)
  absorb_opts = opts_df %>% filter(opt %in% c("absorb", "a", "ab", "abs", "abso", "absor"))
  if (nrow(absorb_opts) > 0) {
    abs_vars = strsplit(shorten.spaces(paste0(absorb_opts$opt_arg, collapse = " ")), " ", fixed = TRUE)[[1]]
    term_list[[3]] = tibble(ia_expr = abs_vars, role = "exo", option = "absorb")
  }

  # FE (from xtreg)
  if (any(opts_df$opt == "fe")) {
    # xtreg assumes panelvar is already set via xtset, we'll append it later if needed,
    # or rely on the drf run_obj panelvar injection.
  }

  # Cluster / SE
  if (!is.null(se_info$se_args) && se_info$se_args != "") {
    se_args_parsed = repdb_parse_se_args(se_info$se_args, as_df = TRUE)
    cluster_vars = se_args_parsed$arg_val[startsWith(se_args_parsed$arg_name, "cluster")]
    if (length(cluster_vars) > 0) {
      term_list[[4]] = tibble(ia_expr = cluster_vars, role = "cluster", option = "se")
    }
  }

  vi = bind_rows(term_list) %>% mutate(main_pos = seq_len(n()))

  # 2. Process Interaction Effects and Prefixes
  vi$is_ia = grepl("(\\|)|(#)|(\\*)", vi$ia_expr)
  vi$var_expr = as.list(vi$ia_expr)

  # Unnest interactions
  rows = which(vi$is_ia)
  vi$var_expr[rows] = strsplit(vi$ia_expr[rows], "(##)|(#)|(\\|)|(\\*)")

  vi = vi %>%
    unnest(var_expr) %>%
    group_by(ia_expr) %>%
    mutate(ia_num = n(), ia_pos = seq_len(n())) %>%
    ungroup()

  # Extract Prefix (L1., F., i., c., etc.) - split at LAST dot
  prefix_start = stringi::stri_locate_last_fixed(vi$var_expr, ".")[, 1]
  vi$prefix = ifelse(
    is.na(prefix_start),
    "",
    stringi::stri_sub(vi$var_expr, 1, prefix_start - 1) %>% stringi::stri_replace_all_fixed(".", "")
  )
  vi$var = ifelse(is.na(prefix_start), vi$var_expr, stringi::stri_sub(vi$var_expr, prefix_start + 1))

  # Normalize specific prefixes
  vi = vi %>%
    mutate(prefix = case_when(
      startsWith(tolower(prefix), "ib") ~ paste0("b", substring(prefix, 3)),
      TRUE ~ prefix
    ))

  # 3. Incorporate column stats info
  cols_info = make_cols_small_info(dat)
  vi = vi %>% left_join(cols_info, by = c("var" = "col"))

  # 4. Determine Types and Classes
  vi = vi %>%
    mutate(
      is_factor = class %in% c("character", "factor"),
      fe_type = case_when(
        startsWith(tolower(prefix), "c") ~ "",
        startsWith(tolower(prefix), "i") ~ "i",
        startsWith(tolower(prefix), "b") ~ "b",
        option %in% c("absorb", "fe") ~ option,
        has.substr(ia_expr, "#") ~ "#",
        is_factor ~ class,
        TRUE ~ ""
      ),
      absorbed_fe = option %in% c("absorb", "fe"),
      is_fe = fe_type != "",
      varclass = class,
      class = ifelse(is_fe & !is_factor, "fe", class),
      add_main_effects = is_ia & (has.substr(ia_expr, "##") | has.substr(ia_expr, "*"))
    )

  # 5. Build Canonical Terms
  vi$ia_cterm = stata_expr_to_cterm(vi$ia_expr)
  vi$cterm = stata_expr_to_cterm(vi$var_expr)
  vi$basevar = stata_expr_to_cterm(vi$var)

  # If a variable is xi-generated (_I...) and the cached data still carries the
  # original Stata variable label, use that label to canonicalize the term.
  # This keeps regvar/regxvar/R output aligned with Stata regcoef parcels.
  var_labels = vapply(dat, function(v) {
    lab = attr(v, "label")
    if (is.null(lab) || length(lab) == 0 || is.na(lab[[1]])) {
      return("")
    }
    as.character(lab[[1]])
  }, character(1))

  xi_rows = startsWith(vi$var, "_I")
  if (any(xi_rows)) {
    xi_labels = unname(var_labels[vi$var])
    xi_has_label = xi_rows & !is.na(xi_labels) & stringi::stri_detect_fixed(xi_labels, "==")

    if (any(xi_has_label)) {
      vi$cterm[xi_has_label] = canonical.output.terms.stata.xi(
        terms = vi$var[xi_has_label],
        labels = xi_labels[xi_has_label]
      )
    }
  }

  # Rebuild ia_cterm from the updated component cterms so interactions with xi
  # variables also become canonical.
  vi = vi %>%
    group_by(main_pos) %>%
    mutate(
      ia_cterm = {
        if (dplyr::n() == 1) {
          cterm
        } else {
          rep(
            split_and_sort(
              paste0(cterm, collapse = "#"),
              split = "#",
              k = dplyr::n()
            )[[1]],
            dplyr::n()
          )
        }
      }
    ) %>%
    ungroup()

  # basevar should refer to the underlying source variable, not the raw _I name
  vi$basevar = stringi::stri_replace_first_regex(vi$cterm, "^.*@", "")
  vi$basevar = stringi::stri_replace_first_regex(vi$basevar, "=.*$", "")

  vi$class = ifelse(has.substr(vi$cterm, "="), "dummy", vi$class)

  # 6. Apply interaction types & Reg Types
  vi = vi_add_ia_type(vi)

  vi = vi %>% mutate(
    var_org_type = varclass %>% change_val(c("fe", "character"), "factor"),
    var_reg_type = class %>% change_val(c("fe", "character"), "factor") %>% change_val("logical", "dummy"),
    ia_reg_type = ia_type %>%
      change_val("fe", "factor") %>%
      change_val("fe_numeric", "factor_numeric") %>%
      change_val("fe_logical", "factor_dummy")
  ) %>% mutate(
    var_reg_type = ifelse(role == "cluster", "factor", var_reg_type),
    ia_reg_type = ifelse(role == "cluster", "factor", ia_reg_type)
  )

  # Ensure column order is clean
  vi = vi %>% select(
    ia_expr, var_expr, var, role, prefix, option, class, fe_type, is_fe,
    distinct_num, ia_num, ia_pos, main_pos, ia_cterm, cterm, basevar, everything()
  )

  return(vi)
}
```

!END_MODIFICATION cmdpart_to_regvar in mrb_reg_tools.R

With those two changes, the flow becomes consistent:

* Stata parcels keep using xi labels, but now without corrupting decimal points
* repaired `regvar` turns `_Ix1_2` into `x1=-1.874...`
* `regxvar` is built from that canonical cterm
* the generated R regression uses the canonical dummy name, not `_Ix1_2`
* `regcoef_rb` then naturally matches the Stata-side cterm naming

After patching, I would rerun at least:

`mrb = mrb_run_r_base(mrb, just_pids = 10)`
`mrb = mrb_run_r_reg(mrb, just_pids = 10)`
`mrb = mrb_make_regcheck_parcel(mrb, just_pids = 10)`

Expected result for runid 10:

* `regvar$cterm` should be `x1=-1.874031985005462` instead of `_Ix1_2`
* `regcoef_sb`, `regcoef_so`, and `regcoef_rb` should all use the same cterm
* the stray `@` inside the numeric level should disappear

The one thing I would not do is add a generic try-wrapper around this. This is a deterministic naming bug, and it is better to fail loudly if a future `_I...` label has a format we do not parse.
