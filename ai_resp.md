```toml
!MODIFICATION t_merge.R
scope = "file"
file = "/home/rstudio/aicoder/stata2r/R/t_merge.R"
description = "Fix scmd_merge to correctly default to full_join, handle common non-by columns correctly without dropping them from using_data beforehand, avoid unconditional as.numeric() on merge keys, and prevent if_else type mismatch errors for common columns."
---
```r
# FILE: R/t_merge.R

# 1. Parsing Phase
s2r_p_merge = function(rest_of_cmd) {
  restore.point("s2r_p_merge")
  match = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*([1m]:[1m])\\s+(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")

  if (is.na(match[1,1])) {
    match_old = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
    if (is.na(match_old[1,1])) return(list(merge_type = NA_character_))
    return(list(merge_type = "1:1", varlist = stringi::stri_trim_both(match_old[1,2]), file = stringi::stri_trim_both(match_old[1,3]), options = stringi::stri_trim_both(match_old[1,4])))
  }

  list(merge_type = match[1,2], varlist = stringi::stri_trim_both(match[1,3]), file = stringi::stri_trim_both(match[1,4]), options = stringi::stri_trim_both(match[1,5]))
}

# 2. Code Generation Phase
t_merge = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_merge")
  parsed = s2r_p_merge(rest_of_cmd)
  if (is.na(parsed$merge_type)) return(paste0("# Failed to parse merge: ", rest_of_cmd))

  file_r_expr = resolve_stata_filename(parsed$file, cmd_df, line_num, default_base_dir_var = "working_dir")

  has_nogenerate = fast_coalesce(stringi::stri_detect_regex(parsed$options, "\\bno(?:generate|gen)\\b"), FALSE)
  keep_opt = NA_character_
  if (!is.na(parsed$options)) {
    k_match = stringi::stri_match_first_regex(parsed$options, "\\bkeep\\s*\\(([^)]+)\\)")
    if (!is.na(k_match[1,1])) keep_opt = stringi::stri_trim_both(k_match[1,2])
  }

  args = c("data = data", paste0("merge_type = ", quote_for_r_literal(parsed$merge_type)),
           paste0("varlist = ", quote_for_r_literal(parsed$varlist)), paste0("file_path = ", file_r_expr),
           paste0("keep_opt = ", quote_for_r_literal(keep_opt)), paste0("has_nogenerate = ", has_nogenerate))

  r_code = paste0("data = scmd_merge(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nif (isTRUE(stata2r_env$has_original_order_idx)) { data = dplyr::mutate(data, stata2r_original_order_idx = dplyr::row_number()) }")

  return(r_code)
}

# 3. Runtime Execution Phase
scmd_merge = function(data, merge_type, varlist, file_path, keep_opt = NA_character_, has_nogenerate = FALSE) {
  restore.point("scmd_merge")
  merge_keys = expand_varlist(varlist, names(data))

  using_data = haven::read_dta(file_path)
  data = sfun_normalize_string_nas(sfun_strip_stata_attributes(data))
  using_data = sfun_normalize_string_nas(sfun_strip_stata_attributes(using_data))

  # Safely align types of merge keys
  for (k in merge_keys) {
    if (is.character(data[[k]]) || is.character(using_data[[k]])) {
      data[[k]] = as.character(data[[k]])
      using_data[[k]] = as.character(using_data[[k]])
    } else if (is.numeric(data[[k]]) || is.numeric(using_data[[k]])) {
      data[[k]] = as.numeric(data[[k]])
      using_data[[k]] = as.numeric(using_data[[k]])
    }
  }

  if (merge_type == "1:1") {
    if (any(duplicated(data[, merge_keys, drop=FALSE]))) stop("Merge 1:1 failed: Duplicate keys in master.")
    if (any(duplicated(using_data[, merge_keys, drop=FALSE]))) stop("Merge 1:1 failed: Duplicate keys in using.")
  }

  # In Stata, merge defaults to keeping all (full_join)
  join_func = dplyr::full_join
  if (!is.na(keep_opt) && keep_opt != "") {
    if (grepl("\\bmatch\\b", keep_opt) && !grepl("\\bmaster\\b", keep_opt) && !grepl("\\busing\\b", keep_opt)) {
      join_func = dplyr::inner_join
    } else if (grepl("\\bmaster\\b", keep_opt) && !grepl("\\busing\\b", keep_opt) && !grepl("\\bmatch\\b", keep_opt)) {
      join_func = dplyr::left_join
    } else if (grepl("\\busing\\b", keep_opt) && !grepl("\\bmaster\\b", keep_opt) && !grepl("\\bmatch\\b", keep_opt)) {
      join_func = dplyr::right_join
    } else if (grepl("\\bmatch\\b", keep_opt) && grepl("\\bmaster\\b", keep_opt) && !grepl("\\busing\\b", keep_opt)) {
      join_func = dplyr::left_join
    } else if (grepl("\\bmatch\\b", keep_opt) && !grepl("\\bmaster\\b", keep_opt) && grepl("\\busing\\b", keep_opt)) {
      join_func = dplyr::right_join
    }
  }

  common_not_by = setdiff(intersect(names(data), names(using_data)), merge_keys)

  # Track origins for Stata's _merge indicator and to resolve common columns correctly
  data$.stata_in_master = 1L
  using_data$.stata_in_using = 1L

  # Prevent Cartesian product on NA == NA by dropping NA keys before merge, if they exist?
  # Actually, Stata merge DOES NOT match missing values with missing values if they are keys!
  # Stata says: "Missing values in merge variables are treated like any other value". So they DO match.
  # So we let NA match NA (dplyr does this by default if we use full_join or we can just let it be).
  # Wait, dplyr >= 1.1.0 allows joining on NA. `na_matches = "na"` is default.

  data = join_func(data, using_data, by = merge_keys, suffix = c("", ".stata_using_suffix"))
  data = sfun_normalize_string_nas(data)

  # Resolve columns common to both datasets (not merge keys)
  # Stata default: keep master's value. If observation is using-only, keep using's value.
  for (col in common_not_by) {
    using_col = paste0(col, ".stata_using_suffix")
    if (using_col %in% names(data)) {
      # Align types to prevent if_else error
      if (is.character(data[[col]]) || is.character(data[[using_col]])) {
        data[[col]] = as.character(data[[col]])
        data[[using_col]] = as.character(data[[using_col]])
      } else if (is.numeric(data[[col]]) || is.numeric(data[[using_col]])) {
        data[[col]] = as.numeric(data[[col]])
        data[[using_col]] = as.numeric(data[[using_col]])
      }

      is_using_only = is.na(data$.stata_in_master) & !is.na(data$.stata_in_using)
      data[[col]] = dplyr::if_else(is_using_only, data[[using_col]], data[[col]])
      data[[using_col]] = NULL
    }
  }

  if (!has_nogenerate) {
    data$`_merge` = dplyr::case_when(
      !is.na(data$.stata_in_master) & is.na(data$.stata_in_using) ~ 1L,
      is.na(data$.stata_in_master) & !is.na(data$.stata_in_using) ~ 2L,
      !is.na(data$.stata_in_master) & !is.na(data$.stata_in_using) ~ 3L,
      TRUE ~ NA_integer_
    )
  }
  
  # Clean up tracking cols
  data$.stata_in_master = NULL
  data$.stata_in_using = NULL

  return(data)
}
```
!END_MODIFICATION t_merge.R

```toml
!MODIFICATION t_replace.R
scope = "file"
file = "/home/rstudio/aicoder/stata2r/R/t_replace.R"
description = "Fix dplyr::if_else type mismatch errors in scmd_replace by explicitly casting the false branch (var_actual) to match the true branch (.val)."
---
```r
# FILE: R/t_replace.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_replace = function(rest_of_cmd) {
  restore.point("s2r_p_replace")
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = if (!is.na(explicit_type_match[1,1])) explicit_type_match[1,2] else NA_character_

  rest_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")

  parts = stringi::stri_split_fixed(rest_no_type, "=", n=2)[[1]]
  if (length(parts) < 2) {
    return(list(
      declared_type = declared_type_str,
      var_to_replace = NA_character_,
      stata_expr = NA_character_,
      if_cond = NA_character_,
      in_str = NA_character_
    ))
  }

  var_to_replace = stringi::stri_trim_both(parts[1])
  right_side = stringi::stri_trim_both(parts[2])

  parsed = s2r_parse_if_in(right_side)

  list(
    declared_type = declared_type_str,
    var_to_replace = var_to_replace,
    stata_expr = parsed$base_str,
    if_cond = parsed$if_str,
    in_str = parsed$in_str
  )
}

# 2. Code Generation Phase: Emit R code
t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_replace")
  parsed = s2r_p_replace(rest_of_cmd)
  if (is.na(parsed$var_to_replace)) return(paste0("# Failed to parse replace command: ", rest_of_cmd))

  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  r_expr = translate_stata_expression_with_r_values(parsed$stata_expr, line_num, cmd_df, current_context)
  if (is.na(r_expr)) r_expr = "NA_real_"

  r_if_cond = NA_character_
  if (!is.na(parsed$if_cond) && parsed$if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_cond, line_num, cmd_df, list(is_by_group = FALSE))
  }

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (current_context$is_by_group) {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  is_string = sfun_is_stata_expression_string_typed(parsed$stata_expr)
  force_integer = FALSE
  if (!is.na(parsed$declared_type)) {
    is_string = stringi::stri_startswith_fixed(parsed$declared_type, "str")
    force_integer = parsed$declared_type %in% c("byte", "int", "long")
  }

  args = c("data = data", paste0("var_to_replace = ", quote_for_r_literal(parsed$var_to_replace)), paste0("r_expr_str = ", quote_for_r_literal(r_expr)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse="','"), "')"))
  args = c(args, paste0("is_string = ", is_string), paste0("force_integer = ", force_integer))

  return(paste0("data = scmd_replace(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_replace = function(data, var_to_replace, r_expr_str, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0), is_string = FALSE, force_integer = FALSE) {
  restore.point("scmd_replace")

  var_actual = expand_varlist(var_to_replace, names(data))[1]
  r_expr_str = resolve_abbrevs_in_expr(r_expr_str, names(data))
  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  # If the target is character, we safely flag it as string to prevent NA coercion
  target_is_char = is.character(data[[var_actual]])
  is_string = is_string || target_is_char

  mask_expr = ".stata_temp_mask"
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask_expr = paste0("(.stata_temp_mask & fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0)")
  }

  if (r_expr_str == "NA_real_") {
    if (is_string) {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", '', as.character(`", var_actual, "`))")
    } else {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", NA_real_, as.numeric(`", var_actual, "`))")
    }
  } else {
    if (is_string) {
        expr_body = paste0("{ .val <- as.character(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, as.character(`", var_actual, "`)) }")
    } else if (force_integer) {
        expr_body = paste0("{ .val <- as.integer(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, as.integer(`", var_actual, "`)) }")
    } else {
        expr_body = paste0("{ .val <- ", r_expr_str, "; if(is.character(.val)) { dplyr::if_else(", mask_expr, ", .val, as.character(`", var_actual, "`)) } else { .val <- as.numeric(.val); dplyr::if_else(", mask_expr, ", .val, as.numeric(`", var_actual, "`)) } }")
    }
  }

  # Compute in-range mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }
  data$.stata_temp_mask = in_mask

  pipe_el = c("data")
  group_vars = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars, collapse="','"), "')))"))
  pipe_el = c(pipe_el, paste0("dplyr::mutate(`", var_actual, "` = ", expr_body, ")"))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  data$.stata_temp_mask = NULL

  return(data)
}
```
!END_MODIFICATION t_replace.R
