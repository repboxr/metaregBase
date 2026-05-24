An excellent analysis of the issues. There are two primary problems addressed here:

!MODIFICATION cmdpart_to_regvar in metaregBase/R/mrb_reg_tools.R
scope = "function"
file = "metaregBase/R/mrb_reg_tools.R"
function_name = "cmdpart_to_regvar"
description = "Fix fe_type logic to not coerce interacting variables to factors based on # presence."
---
```r
cmdpart_to_regvar = function(cmdpart, dat, opts_df, se_info) {
  restore.point("cmdpart_to_regvar")

  # 1. Collect all terms mapped by role
  term_list = list()

  # Standard variables (dep, exo, endo, instr)
  v_df = cmdpart %>% dplyr::filter(part == "v")
  if (nrow(v_df) > 0) {
    # Replace tag names with role names (depvar -> dep, others stay same)
    v_df$role = ifelse(v_df$tag == "depvar", "dep", v_df$tag)
    term_list[[1]] = dplyr::tibble(ia_expr = v_df$content, role = v_df$role, option = "")
  }

  # Weights
  w_df = cmdpart %>% dplyr::filter(part == "weight_var")
  if (nrow(w_df) > 0) {
    term_list[[2]] = dplyr::tibble(ia_expr = w_df$content, role = "weight", option = "")
  }

  # Absorb (from reghdfe / areg)
  absorb_opts = opts_df %>% dplyr::filter(opt %in% c("absorb", "a", "ab", "abs", "abso", "absor"))
  if (nrow(absorb_opts) > 0) {
    abs_vars = strsplit(shorten.spaces(paste0(absorb_opts$opt_arg, collapse = " ")), " ", fixed = TRUE)[[1]]
    term_list[[3]] = dplyr::tibble(ia_expr = abs_vars, role = "exo", option = "absorb")
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
      term_list[[4]] = dplyr::tibble(ia_expr = cluster_vars, role = "cluster", option = "se")
    }
  }

  vi = dplyr::bind_rows(term_list) %>% dplyr::mutate(main_pos = seq_len(dplyr::n()))

  # 2. Process Interaction Effects and Prefixes
  vi$is_ia = grepl("(\\|)|(#)|(\\*)", vi$ia_expr)
  vi$var_expr = as.list(vi$ia_expr)

  # Unnest interactions
  rows = which(vi$is_ia)
  vi$var_expr[rows] = strsplit(vi$ia_expr[rows], "(##)|(#)|(\\|)|(\\*)")

  vi = vi %>%
    tidyr::unnest(var_expr) %>%
    dplyr::group_by(ia_expr) %>%
    dplyr::mutate(ia_num = dplyr::n(), ia_pos = seq_len(dplyr::n())) %>%
    dplyr::ungroup()

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
    dplyr::mutate(prefix = dplyr::case_when(
      startsWith(tolower(prefix), "ib") ~ paste0("b", substring(prefix, 3)),
      TRUE ~ prefix
    ))

  # 3. Incorporate column stats info
  cols_info = make_cols_small_info(dat)
  vi = vi %>% dplyr::left_join(cols_info, by = c("var" = "col"))

  # 4. Determine Types and Classes
  vi = vi %>%
    dplyr::mutate(
      is_factor = class %in% c("character", "factor"),
      fe_type = dplyr::case_when(
        startsWith(tolower(prefix), "c") ~ "",
        startsWith(tolower(prefix), "i") ~ "i",
        startsWith(tolower(prefix), "b") ~ "b",
        option %in% c("absorb", "fe") ~ option,
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
    dplyr::group_by(main_pos) %>%
    dplyr::mutate(
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
    dplyr::ungroup()

  # basevar should refer to the underlying source variable, not the raw _I name
  vi$basevar = stringi::stri_replace_first_regex(vi$cterm, "^.*@", "")
  vi$basevar = stringi::stri_replace_first_regex(vi$basevar, "=.*$", "")

  vi$class = ifelse(has.substr(vi$cterm, "="), "dummy", vi$class)

  # 6. Apply interaction types & Reg Types
  vi = vi_add_ia_type(vi)

  vi = vi %>% dplyr::mutate(
    var_org_type = varclass %>% change_val(c("fe", "character"), "factor"),
    var_reg_type = class %>% change_val(c("fe", "character"), "factor") %>% change_val("logical", "dummy"),
    ia_reg_type = ia_type %>%
      change_val("fe", "factor") %>%
      change_val("fe_numeric", "factor_numeric") %>%
      change_val("fe_logical", "factor_dummy")
  ) %>% dplyr::mutate(
    var_reg_type = ifelse(role == "cluster", "factor", var_reg_type),
    ia_reg_type = ifelse(role == "cluster", "factor", ia_reg_type)
  )

  # Ensure column order is clean
  vi = vi %>% dplyr::select(
    ia_expr, var_expr, var, role, prefix, option, class, fe_type, is_fe,
    distinct_num, ia_num, ia_pos, main_pos, ia_cterm, cterm, basevar, dplyr::everything()
  )

  return(vi)
}
```
!END_MODIFICATION cmdpart_to_regvar in metaregBase/R/mrb_reg_tools.R
