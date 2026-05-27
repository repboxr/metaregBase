# For a given runid=pid
# create a text that shows the complete path including Stata data modification steps,
# the filter code, and the translated R regression steps, formatted clearly.

example = function() {

}

mrb_runid_test_files = function(project_dir, runid, parcels = list(), drf = repboxDRF::drf_load(project_dir, parcels), outdir = paste0(project_dir, "/run/runid_", runid)) {

  if (!dir.exists(outdir)) dir.create(outdir)
  r_code = mrb_test_code_path(project_dir, runid, parcels, drf)
  r_code = paste0('project_dir = "', project_dir, '"\n', r_code)
  file = paste0(outdir, "/test_runid_", runid, ".R")
  writeLines(r_code, file)
  invisible()
}


mrb_test_reg_data_prep_code = function(project_dir, runid, parcels = list()) {
  restore.point("mrb_test_reg_data_prep_code")

  need = c("reg", "regvar", "regxvar")
  missing = need[!need %in% names(parcels)]

  load_call = if (length(missing) == 0) {
    'parcels = parcels'
  } else {
    paste0(
      'parcels = repboxDB::repdb_load_parcels(project_dir, c(',
      paste0('"', missing, '"', collapse = ", "),
      '), parcels = parcels)'
    )
  }

  lines = c(
    paste0("runid = ", runid),
    "if (!exists(\"parcels\")) parcels = list()",
    load_call,
    "drf = repboxDRF::drf_load(project_dir, parcels = parcels)",
    "reg = parcels$reg[parcels$reg$runid == runid, , drop = FALSE]",
    "regvar = parcels$regvar[parcels$regvar$runid == runid, , drop = FALSE]",
    "regxvar = if (!is.null(parcels$regxvar)) parcels$regxvar[parcels$regxvar$runid == runid, , drop = FALSE] else tibble::tibble()",
    "",
    "# dat is the regression-ready data, including the DRF path, filtering,",
    "# generated cterm columns, and regxvar columns",
    "dat = metaregBase:::mrb_get_regression_data(runid = runid, drf = drf, reg = reg, regvar = regvar, regxvar = regxvar)"
  )

  paste0(lines, collapse = "\n")
}


mrb_test_code_path = function(project_dir, runid, parcels, drf, opts = mrb_test_opts()) {
  restore.point("mrb_test_code_path")

  path_df = drf$path_df %>% filter(pid == !!runid, runid <= !!runid) %>% arrange(runid)

  if (NROW(path_df) == 0) {
    return(paste0("# No path found in drf$path_df for pid ", runid))
  }

  run_df = drf$run_df %>% filter(runid %in% path_df$runid) %>% arrange(runid)

  txt_lines = c()

  for (i in seq_len(NROW(run_df))) {
    r_id = run_df$runid[i]
    stata_cmd = run_df$cmdline[i]

    # Format the original Stata command neatly as an R comment
    stata_cmd_lines = strsplit(stata_cmd, "\n")[[1]]
    stata_cmd_comment = paste0("# Stata: ", paste0(stata_cmd_lines, collapse = "\n#        "))

    if (r_id == runid) {
      # This is the final analysis target / regression command.

      # Explicit dependency load logic and filter translation from drf_get_data()
      pid_load_code = repboxDRF:::drf_get_dependency_load_code(r_id, drf)
      filter_code = repboxDRF::drf_get_filter_code(r_id, drf, parcels = parcels)

      final_step_drf_code = c(pid_load_code, filter_code)
      final_step_drf_code = final_step_drf_code[!is.na(final_step_drf_code) & nzchar(final_step_drf_code)]

      # Also add the direct regression-ready data construction so the block is runnable as-is.
      data_prep_code = mrb_test_reg_data_prep_code(project_dir, r_id, parcels)

      reg_code = mrb_test_reg_r_code(project_dir, r_id, parcels, add_function = FALSE)

      rcode_parts = c(
        if (length(final_step_drf_code) > 0) final_step_drf_code else NULL,
        if (length(final_step_drf_code) > 0) "" else NULL,
        data_prep_code,
        "",
        reg_code
      )
      rcode_str = paste0(rcode_parts, collapse = "\n")
      if (!nzchar(rcode_str) || all(is.na(rcode_str))) {
        rcode_str = "# No R translation found/needed"
      }

      txt_lines = c(txt_lines, stata_cmd_comment, rcode_str, "")

    } else {
      # Modification or data loading step preceding the target.
      rcode = run_df$rcode[i]

      # If this is the FIRST runid in the path, and it has a cache, inject the cache load code
      if (i == 1 && isTRUE(run_df$has_file_cache[i])) {
        drf_rel_path = paste0("cached_dta/", basename(run_df$drf_cache_file[i]))
        rcode = paste0(
          'data = drf_load_data(project_dir, "', drf_rel_path, '")\n',
          'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
          'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
        )
      }

      if (is.null(rcode) || is.na(rcode) || !nzchar(rcode)) {
        rcode = "# No R translation found/needed"
      }

      txt_lines = c(txt_lines, stata_cmd_comment, rcode, "")
    }
  }

  paste0(txt_lines, collapse = "\n")
}
