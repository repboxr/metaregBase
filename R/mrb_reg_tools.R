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

  pattern_rhs = str.right.of(pattern,".")
  pattern_lhs = str.left.of(pattern,".",not.found = rep("", length(pattern)))
  pattern_lhs = ifelse(pattern_lhs == "","", paste0(pattern_lhs, "."))

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

    # If the pattern expanded into multiple columns, replicate the row
    if (length(expanded_content) > 1) {
      new_rows = row_data[rep(1, length(expanded_content)), ]
      new_rows$content = expanded_content
      return(new_rows)
    } else {
      row_data$content = expanded_content
      return(row_data)
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

  return(res)
}


# ==============================================================================
# PART 3: Bridging `cmdpart` to `regvar` (Replacing `mrb_vi_from_stata_reg`)
# ==============================================================================

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
    abs_vars = strsplit(shorten.spaces(absorb_opts$opt_arg[1]), " ", fixed = TRUE)[[1]]
    term_list[[3]] = tibble(ia_expr = abs_vars, role = "exo", option = "absorb")
  }

  # FE (from xtreg)
  if (any(opts_df$opt == "fe")) {
    # xtreg assumes panelvar is already set via xtset, we'll append it later if needed,
    # or rely on the drf run_obj panelvar injection.
  }

  # Cluster / SE
  if (!is.null(se_info$se_args) && se_info$se_args != "") {
    se_args_parsed = repdb_parse_se_args(se_info$se_args, as_df=TRUE)
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
  vi$prefix = ifelse(is.na(prefix_start), "", stringi::stri_sub(vi$var_expr, 1, prefix_start - 1) %>% stringi::stri_replace_all_fixed(".", ""))
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
        prefix %in% c("i", "I") ~ "i",
        startsWith(prefix, "b") ~ "b",
        has.substr(ia_expr, "#") & !startsWith(prefix, "c") ~ "#",
        option %in% c("absorb", "fe") ~ option,
        is_factor ~ class,
        TRUE ~ ""
      ),
      absorbed_fe = fe_type %in% c("absorb", "fe"),
      is_fe = fe_type != "",
      varclass = class,
      class = ifelse(is_fe & !is_factor, "fe", class),
      add_main_effects = is_ia & (has.substr(ia_expr, "##") | has.substr(ia_expr, "*"))
    )

  # 5. Build Canonical Terms
  vi$ia_cterm = stata_expr_to_cterm(vi$ia_expr)
  vi$cterm = stata_expr_to_cterm(vi$var_expr)
  vi$basevar = stata_expr_to_cterm(vi$var)

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
  vi = vi %>% select(ia_expr, var_expr, var, role, prefix, option, class, fe_type, is_fe, distinct_num, ia_num, ia_pos, main_pos, ia_cterm, cterm, basevar, everything())

  return(vi)
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

