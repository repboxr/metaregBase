mrb_test_first_non_missing_value = function(df, cols, default = NA) {
  restore.point("mrb_test_first_non_missing_value")

  cols = cols[cols %in% names(df)]
  if (length(cols) == 0) {
    return(default)
  }

  for (col in cols) {
    val = df[[col]][[1]]
    if (length(val) == 0) {
      next
    }
    if (is.character(val)) {
      if (!is.na(val) && nzchar(val)) {
        return(val)
      }
    } else if (!all(is.na(val))) {
      return(val)
    }
  }

  default
}


mrb_test_line_range_text = function(start, end = start) {
  restore.point("mrb_test_line_range_text")

  start = suppressWarnings(as.integer(start))
  end = suppressWarnings(as.integer(end))

  if (is.na(start) && is.na(end)) {
    return(NA_character_)
  }
  if (is.na(start)) {
    start = end
  }
  if (is.na(end)) {
    end = start
  }
  if (end < start) {
    end = start
  }
  if (start == end) {
    return(as.character(start))
  }
  paste0(start, "-", end)
}


mrb_test_get_regsource_row = function(runid, parcels = list(), drf = NULL) {
  restore.point("mrb_test_get_regsource_row")

  if (!is.null(parcels$regsource) && NROW(parcels$regsource) > 0) {
    src = parcels$regsource[parcels$regsource$runid == runid, , drop = FALSE]
    if (NROW(src) > 0) {
      return(src[1, , drop = FALSE])
    }
  }

  if (is.null(drf) || is.null(drf$run_df) || NROW(drf$run_df) == 0) {
    return(tibble())
  }

  run_df = drf$run_df[drf$run_df$runid == runid, , drop = FALSE]
  if (NROW(run_df) == 0) {
    return(tibble())
  }

  script_path = mrb_test_first_non_missing_value(
    run_df,
    c("file_path", "found_path", "root_file_path"),
    default = NA_character_
  )
  run_line = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(run_df, c("line"), default = NA_integer_)
  ))
  org_line = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(run_df, c("orgline"), default = NA_integer_)
  ))
  cmdline = mrb_test_first_non_missing_value(run_df, c("cmdline"), default = NA_character_)

  tibble(
    runid = runid,
    script_path = as.character(script_path),
    script_file = ifelse(is.na(script_path), NA_character_, basename(script_path)),
    code_line_start = ifelse(is.na(org_line), run_line, org_line),
    code_line_end = ifelse(is.na(org_line), run_line, org_line),
    line = run_line,
    cmdline = as.character(cmdline),
    text = NA_character_
  )
}


mrb_test_source_snippet_text = function(src_row) {
  restore.point("mrb_test_source_snippet_text")

  if (NROW(src_row) == 0) {
    return("")
  }
  if (!("text" %in% names(src_row))) {
    return("")
  }

  text = src_row$text[[1]]
  if (length(text) == 0 || is.na(text) || !nzchar(text)) {
    return("")
  }

  start = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src_row, c("code_line_start", "line"), default = NA_integer_)
  ))
  end = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src_row, c("code_line_end"), default = start)
  ))

  if (is.na(start) || is.na(end)) {
    return("")
  }

  lines = stringi::stri_split_lines1(as.character(text))
  if (length(lines) == 0) {
    return("")
  }

  start = max(1L, start)
  end = min(length(lines), end)
  if (end < start) {
    end = start
  }

  idx = seq.int(start, end)
  snippet = paste0(sprintf("%4d ", idx), lines[idx], collapse = "\n")
  snippet
}


mrb_test_source_text = function(runid, parcels = list(), drf = NULL) {
  restore.point("mrb_test_source_text")

  src = mrb_test_get_regsource_row(runid, parcels = parcels, drf = drf)
  if (NROW(src) == 0) {
    return("")
  }

  script_path = as.character(
    mrb_test_first_non_missing_value(
      src,
      c("script_path", "file_path", "found_path", "root_file_path"),
      default = NA_character_
    )
  )
  code_start = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src, c("code_line_start", "orgline", "line"), default = NA_integer_)
  ))
  code_end = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src, c("code_line_end"), default = code_start)
  ))
  run_line = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src, c("line"), default = NA_integer_)
  ))
  cmdline = as.character(
    mrb_test_first_non_missing_value(src, c("cmdline"), default = NA_character_)
  )
  snippet = mrb_test_source_snippet_text(src)

  has_info =
    (!is.na(script_path) && nzchar(script_path)) ||
    !is.na(code_start) ||
    (!is.na(run_line) && run_line > 0) ||
    (!is.na(cmdline) && nzchar(cmdline)) ||
    nzchar(snippet)

  if (!has_info) {
    return("")
  }

  out = c("### Original Stata Source", "")

  if (!is.na(script_path) && nzchar(script_path)) {
    out = c(out, paste0("- File: `", script_path, "`"))
  }

  line_range = mrb_test_line_range_text(code_start, code_end)
  if (!is.na(line_range)) {
    out = c(out, paste0("- Original line", ifelse(grepl("-", line_range, fixed = TRUE), "s", ""), ": ", line_range))
  }

  if (!is.na(run_line) && (is.na(code_start) || run_line != code_start)) {
    out = c(out, paste0("- Parsed run line: ", run_line))
  }

  if (!nzchar(snippet) && !is.na(cmdline) && nzchar(cmdline)) {
    out = c(out, paste0("- Command: `", cmdline, "`"))
  }

  if (nzchar(snippet)) {
    out = c(out, "", "```stata", snippet, "```")
  }

  paste0(out, collapse = "\n")
}
