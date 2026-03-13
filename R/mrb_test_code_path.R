# For a given runid=pid
# create a text that shows the complete path including Stata data modification steps,
# the filter code, and the translated R regression steps, formatted clearly.

mrb_test_code_path = function(project_dir, runid, parcels, drf, opts=mrb_test_opts()) {
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

       # 1. Obtain data filter code (for `if` and `in` syntax)
       filter_code = drf_get_filter_code(r_id, drf, parcels = parcels)

       # 2. Obtain translated regression code
       reg_code = mrb_test_reg_r_code(project_dir, r_id, parcels)

       rcode = c(filter_code, reg_code)
       rcode_str = paste0(rcode, collapse = "\n")
       if (!nzchar(rcode_str) || all(is.na(rcode_str))) {
         rcode_str = "# No R translation found/needed"
       }

       txt_lines = c(txt_lines, stata_cmd_comment, rcode_str, "")

    } else {
       # Modification or data loading step preceding the target.
       rcode = run_df$rcode[i]
       if (is.null(rcode) || is.na(rcode) || !nzchar(rcode)) {
         rcode = "# No R translation found/needed"
       }

       txt_lines = c(txt_lines, stata_cmd_comment, rcode, "")
    }
  }

  paste0(txt_lines, collapse = "\n")
}
