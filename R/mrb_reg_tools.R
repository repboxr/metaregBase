# ==============================================================================
# PART 1: Rescued & Adapted Variable Expansion (Globbing, Ranges, Abbreviations)
# ==============================================================================

#' Expand Stata patterns (*, -, abbreviations) into actual column names
#' @param pattern Character vector of variable patterns (e.g., "x1-x5", "i.year*")
#' @param cols Character vector of available columns in the dataset
#' @param unlist Logical, whether to unlist the result
#' @param uses_xi Logical, whether the command is prefixed with `xi:`
expand_stata_var_patterns = function(pattern, cols, unlist=TRUE, uses_xi=FALSE) {
  restore.point("expand_stata_var_patterns")

  # Helper to expand time series operators with ranges
  # e.g. L(0/3).x1 -> x1 L1.x1 L2.x1 L3.x1
  pattern = expand_stata_ts_ranges(pattern)

  if (uses_xi) {
    if (!is.null(pattern)) {
      pattern = stringi::stri_replace_all_fixed(pattern, "|","#")
      pattern = stringi::stri_replace_all_regex(pattern, "(([\\.][a-zA-Z0-9_]+))(\\*)","$1##")
    }
  }

  # Split interaction terms
  ia_rows = which(has.substr(pattern,"#"))
  if (length(ia_rows)>0) {
    has_double = has.substr(pattern[ia_rows],"##")
    sep = ifelse(has_double,"##","#")
    for (i in seq_along(ia_rows)) {
      row = ia_rows[i]
      parts = strsplit(pattern[row],sep[i],fixed=TRUE)[[1]]
      parts = expand_stata_var_patterns(parts,cols=cols, unlist=TRUE, uses_xi=uses_xi)
      pattern[row] = paste0(parts, collapse=sep)
    }
    not_ia_rows = setdiff(seq_along(pattern),ia_rows)
    if (length(not_ia_rows)>0) {
      pattern[not_ia_rows] = expand_stata_var_patterns(pattern[not_ia_rows], cols=cols, uses_xi=uses_xi,unlist=FALSE)
    }
    if (unlist) return(unlist(pattern))
    return(pattern)
  }

  star_rows = which(has.substr(pattern,"*") | has.substr(pattern, "?"))
  minus_rows = which(has.substr(pattern,"-"))
  normal_rows = setdiff(seq_along(pattern), c(star_rows, minus_rows))

  # Split at the LAST dot to cleanly separate all Stata prefixes from the base variable
  last_dot = stringi::stri_locate_last_fixed(pattern, ".")[, 1]
  pattern_rhs = ifelse(is.na(last_dot), pattern, stringi::stri_sub(pattern, last_dot + 1))
  pattern_lhs = ifelse(is.na(last_dot), "", stringi::stri_sub(pattern, 1, last_dot))

  # Old code
  #pattern_rhs = str.right.of(pattern,".")
  #pattern_lhs = str.left.of(pattern,".",not.found = rep("", length(pattern)))
  #pattern_lhs = ifelse(pattern_lhs == "","", paste0(pattern_lhs, "."))



  # Abbreviation Matching
  no_match_rows = normal_rows[which(!(pattern_rhs[normal_rows] %in% cols))]
  if (length(no_match_rows)>0) {
    for (row in no_match_rows) {
      mcols = which(startsWith(cols, pattern_rhs[row]))
      if (length(mcols)>1) {
        cat("\nThe regression variable ", pattern_rhs[row], " matches multiple variables.\n")
        pattern[row] = paste0(pattern_lhs[row] ,cols[mcols[1]])
      } else if (length(mcols)==0) {
        msg = paste0("The regression variable ", pattern_rhs[row], " could not be matched with any variable in the data set.")
        repbox_problem(msg, "regvar_no_match", fail_action = "msg")
      } else {
        pattern[row] = paste0(pattern_lhs[row],cols[mcols[1]])
      }
    }
  }

  if (length(star_rows)+length(minus_rows)==0) return(pattern)
  vars = as.list(pattern)

  # Replace var* patterns
  rows = star_rows
  if (uses_xi) {
    has_dot_star = pattern_lhs[rows]!="" & has.substr(pattern[rows],"*")
  }
  rx = glob2rx(pattern_rhs[rows])
  for (i in seq_along(rows)) {
    r = rows[i]
    mvars = cols[grepl(rx[i],cols)]
    vars[[r]] = paste0(pattern_lhs[r],mvars)
    if (uses_xi && has_dot_star[i]) {
      vars[[r]] = pattern[rows[i]]
    }
  }

  # Replace var1-var5 patterns
  rows = minus_rows
  for (i in seq_along(rows)) {
    r = rows[i]
    pat = pattern_rhs[r]
    from_var = str.left.of(pat,"-") %>% trimws()
    to_var = str.right.of(pat,"-") %>% trimws()
    range = sort(which(cols %in% c(from_var, to_var)))
    if (length(range) >= 2) {
      vars[[r]] = paste0(pattern_lhs[r],cols[range[1]:range[2]])
    } else {
      vars[[r]] = pattern[r] # Fallback
    }
  }

  if (unlist) return(unlist(vars))
  vars
}


#' Helper to expand time series operators with ranges, e.g. L(0/3).x1 -> x1 L1.x1 L2.x1 L3.x1
# FILE: mrb_reg_tools.R
# Replace the existing expand_stata_ts_ranges function:

#' Helper to expand time series operators with ranges, e.g. L(0/3).x1 -> x1 L1.x1 L2.x1 L3.x1

expand_stata_ts_ranges = function(patterns) {
  n = length(patterns)
  if (n == 0) return(patterns)

  detect_rx = "^[A-Za-z.]+\\([0-9]+[/-][0-9]+\\)\\."
  cand = stringi::stri_detect_regex(patterns, detect_rx)

  if (!any(cand)) return(patterns)

  match_rx = "^([A-Za-z.]+)\\(([0-9]+)[/-]([0-9]+)\\)\\.(.*)$"
  cand_idx = which(cand)
  mat = stringi::stri_match_first_regex(patterns[cand_idx], match_rx)

  out = as.list(patterns)
  out[cand_idx] = lapply(seq_along(cand_idx), function(i) {
    if (is.na(mat[i, 1])) {
      return(patterns[cand_idx[i]])
    }

    prefix = mat[i, 2]
    start_num = as.integer(mat[i, 3])
    end_num = as.integer(mat[i, 4])
    var = mat[i, 5]

    if (is.na(start_num) || is.na(end_num) || start_num > end_num) {
      return(patterns[cand_idx[i]])
    }

    nums = start_num:end_num
    expanded = paste0(prefix, nums, ".", var)

    zero = nums == 0L
    if (any(zero)) {
      base_prefix = stringi::stri_replace_last_regex(prefix, "[LlFfDdSsOo]$", "")
      if (tolower(base_prefix) %in% c("c", "i", "co", "o")) {
        base_prefix = paste0(base_prefix, ".")
      }
      expanded[zero] = paste0(base_prefix, var)
    }

    expanded
  })

  unlist(out, use.names = FALSE)
}

# expand_stata_ts_ranges = function(patterns) {
#   if (length(patterns) == 0) return(patterns)
#
#   res = unlist(lapply(patterns, function(p) {
#     # Match start/end ranges like L(0/3).x1 or c.L(0-3).x1
#     match_range = stringi::stri_match_first_regex(p, "^([a-zA-Z\\.]+)\\(([0-9]+)[/\\-]([0-9]+)\\)\\.(.*)$")
#
#     if (!is.na(match_range[1,1])) {
#       prefix = match_range[1,2]
#       start_num = as.integer(match_range[1,3])
#       end_num = as.integer(match_range[1,4])
#       var = match_range[1,5]
#
#       if (start_num <= end_num) {
#         nums = start_num:end_num
#         expanded = sapply(nums, function(n) {
#           if (n == 0) {
#             # For 0, remove the trailing TS operator (e.g. "L", "F", "D") from the prefix
#             base_prefix = stringi::stri_replace_last_regex(prefix, "[LlFfDdSsOo]$", "")
#
#             # Reattach dot if the prefix is exclusively an isolated indicator
#             if (tolower(base_prefix) %in% c("c", "i", "co", "o")) {
#                base_prefix = paste0(base_prefix, ".")
#             }
#             paste0(base_prefix, var)
#           } else {
#             paste0(prefix, n, ".", var)
#           }
#         })
#         return(expanded)
#       }
#     }
#     return(p) # Return unchanged if no match
#   }))
#   return(res)
# }


# ==============================================================================
# PART 2: Semantic Data-Driven Expansion of `cmdpart`
# ==============================================================================

#' Expand variable parts in cmdpart using the dataset columns
#' @param cmdpart The parsed command part dataframe
#' @param data_cols Character vector of column names in the dataset
cmdpart_expand_vars = function(cmdpart, data_cols) {
  restore.point("cmdpart_expand_vars")

  # Check if `xi:` prefix is used
  uses_xi = any(cmdpart$part == "pre" & cmdpart$tag == "xi")

  # Separate variable parts and non-variable parts
  v_rows = which(cmdpart$part == "v")
  if (length(v_rows) == 0) return(cmdpart)

  v_df = cmdpart[v_rows, ]
  non_v_df = cmdpart[-v_rows, ]

  expanded_list = lapply(seq_len(nrow(v_df)), function(i) {
    row_data = v_df[i, ]
    expanded_content = expand_stata_var_patterns(row_data$content, data_cols, unlist=TRUE, uses_xi=uses_xi)
    expanded_content = expanded_content[expanded_content!=""]

    # If the pattern expanded into multiple columns, replicate the row
    if (length(expanded_content) > 1) {
      new_rows = row_data[rep(1, length(expanded_content)), ]
      new_rows$content = expanded_content
      return(new_rows)
    } else if (length(expanded_content) == 1) {
      row_data$content = expanded_content
      return(row_data)
    } else {
      repbox_problem(paste0("Variable expansion for Stata term ",  row_data$content, " had no match in data columns."), type="empty_term_expand",fail_action = "msg")
      return(row_data[0, ])
    }
  })

  expanded_v_df = bind_rows(expanded_list)

  # Re-calculate the counter correctly for each tag within the variable part
  expanded_v_df = expanded_v_df %>%
    group_by(runid, parent, part, tag) %>%
    mutate(counter = seq_len(n())) %>%
    ungroup()

  # Rebind and sort safely
  res = bind_rows(non_v_df, expanded_v_df) %>%
    arrange(runid, parent, part, counter)

  # If some

  return(res)
}


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

#' Add the panel fixed effect implied by xtreg, fe to regvar
#'
#' Stata's xtreg, fe absorbs the panel variable declared by xtset.
#' In metaregBase this variable should already be available in xtvar$panelvar
#' or in the reg parcel. Legacy xtreg syntax may also specify it via i() or iis().
#' We deliberately do not infer the panel variable from the cluster variable,
#' because Stata does not do that.
mrb_add_xtreg_fe_regvar = function(regvar, reg, opts_df, xtvar = NULL, dat = NULL) {
  restore.point("mrb_add_xtreg_fe_regvar")

  if (is.null(reg) || NROW(reg) == 0) {
    return(regvar)
  }

  cmd = as.character(reg$cmd[1])
  if (!identical(cmd, "xtreg")) {
    return(regvar)
  }

  if (is.null(opts_df) || NROW(opts_df) == 0 || !any(opts_df$opt == "fe")) {
    return(regvar)
  }

  nonempty_chr = function(x) {
    x = as.character(x)
    x = x[!is.na(x) & nzchar(trimws(x))]
    x
  }

  panelvar = character(0)

  if (!is.null(xtvar) && "panelvar" %in% names(xtvar)) {
    panelvar = nonempty_chr(xtvar$panelvar)[1]
  }

  if (length(panelvar) == 0 || is.na(panelvar)) {
    if ("panelvar" %in% names(reg)) {
      panelvar = nonempty_chr(reg$panelvar)[1]
    }
  }

  if (length(panelvar) == 0 || is.na(panelvar)) {
    panel_rows = opts_df$opt %in% c("i", "iis")
    if (any(panel_rows)) {
      panelvar = nonempty_chr(opts_df$opt_arg[panel_rows])[1]
    }
  }

  if (length(panelvar) == 0 || is.na(panelvar) || !nzchar(panelvar)) {
    msg = paste0(
      "xtreg, fe was found but no panel variable is available from xtvar, ",
      "reg$panelvar, or legacy i()/iis() options. Cannot add the fixed effect."
    )
    repbox_problem(type = "xtreg_panelvar_missing", msg = msg, fail_action = "warn")
    return(regvar)
  }

  panel_cterm = stata_expr_to_cterm(panelvar)

  already_has_fe = any(
    regvar$role == "exo" &
      isTRUE_VEC(regvar$absorbed_fe) &
      regvar$cterm == panel_cterm
  )

  if (isTRUE(already_has_fe)) {
    return(regvar)
  }

  if (!is.null(dat) && panelvar %in% names(dat)) {
    distinct_num = dplyr::n_distinct(dat[[panelvar]], na.rm = TRUE)
    varclass = repbox_col_class(dat[[panelvar]], distinct_num = distinct_num)
  } else {
    distinct_num = NA_integer_
    varclass = NA_character_
  }

  main_pos = suppressWarnings(max(regvar$main_pos, na.rm = TRUE))
  if (!is.finite(main_pos)) {
    main_pos = 0L
  }

  new_row = regvar[1, , drop = FALSE]

  for (col in names(new_row)) {
    if (is.logical(new_row[[col]])) {
      new_row[[col]] = FALSE
    } else if (is.integer(new_row[[col]])) {
      new_row[[col]] = NA_integer_
    } else if (is.numeric(new_row[[col]])) {
      new_row[[col]] = NA_real_
    } else if (is.list(new_row[[col]])) {
      new_row[[col]] = list(NULL)
    } else {
      new_row[[col]] = NA_character_
    }
  }

  vals = list(
    ia_expr = panelvar,
    var_expr = panelvar,
    var = panelvar,
    role = "exo",
    prefix = "",
    option = "xtreg_fe",
    class = "fe",
    fe_type = "xtreg_fe",
    is_fe = TRUE,
    distinct_num = as.integer(distinct_num),
    ia_num = 1L,
    ia_pos = 1L,
    main_pos = as.integer(main_pos + 1L),
    ia_cterm = panel_cterm,
    cterm = panel_cterm,
    basevar = panel_cterm,
    is_ia = FALSE,
    absorbed_fe = TRUE,
    is_factor = TRUE,
    add_main_effects = FALSE,
    varclass = varclass,
    ia_distinct_num = as.numeric(distinct_num),
    ia_type = "fe",
    var_org_type = ifelse(is.na(varclass), "factor", varclass),
    var_reg_type = "factor",
    ia_reg_type = "factor"
  )

  for (nm in intersect(names(vals), names(new_row))) {
    new_row[[nm]] = vals[[nm]]
  }

  dplyr::bind_rows(regvar, new_row)
}

isTRUE_VEC = function(x) {
  x[is.na(x)] = FALSE
  as.logical(x)
}



vi_add_ia_type = function(vi) {
  vi %>%
    group_by(is_ia, ia_cterm, role, ia_num, main_pos) %>%
    arrange(desc(is_fe), desc(class=="dummy")) %>%
    mutate(
      ia_distinct_num = prod(distinct_num, na.rm=TRUE),
      ia_type = case_when(
        all(!is_fe & class=="dummy") ~ "dummies",
        all(!is_fe) ~ "numeric",
        all(is_fe | class=="dummy") ~ "fe",
        ia_num == 2 & class[1] == "dummy" & (!is_fe[2] & class[2] != "dummy") ~ "dummy_numeric",
        ia_num == 2 & is_fe[1] & !is_fe[2] ~ "fe_numeric",
        TRUE ~ "unknown"
      )
    ) %>%
    ungroup()
}

