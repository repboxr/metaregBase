example = function() {
  mrb_print_parcels(project_dir, runid=7)
}

mrb_print_parcels = function(project_dir=mrb$project_dir,parcel_names = c("regcoef","regcoef_so","regcoef_rb","regvar","regxvar", "regcheck"),  runid=NULL, mrb=NULL, parcels=mrb$parcels, outfile = file.path(project_dir, "run/parcel_out.txt")) {
  restore.point("mrb_print_parcels")
  parcels = repdb_load_parcels(project_dir, parcel_names, parcels)

  str = ""
  if (!is.null(runid)) {
    str = paste0(str,"All parcel contents for runid=",paste0(runid, collapse=", "))
  }
  for (p in parcel_names) {
    str = paste0(str,"\n\nParcel ", p,":\n\n")
    parcel = parcels[[p]]
    if (is.null(parcel)) next
    if (!is.null(runid)) parcel = parcel[parcel$runid==runid,]
    str = paste0(str, paste0(capture.output(print(parcel, width=1000)), collapse="\n"))

  }
  writeLines(str, outfile)
  cat(str)
  invisible()
}


mrb_test_data_preview_text = function(runid, drf, parcels, n = 5, opts=mrb_test_opts()) {
  restore.point("mrb_test_data_preview_text")

  if (!opts$show_org_data & !opts$show_reg_data) return("")

  # Get the path for this runid to find the first step (the data load state)
  path_df = drf$path_df %>% filter(pid == !!runid, runid <= !!runid) %>% arrange(runid)
  if (NROW(path_df) == 0) return("")

  first_runid = path_df$runid[1]

  format_df_sample = function(df, title, put_reg_cols_first = TRUE) {
    restore.point("format_df_sample")
    if (inherits(df, "try-error") || is.null(df)) return(paste0("Could not load ", title, "."))
    if (NROW(df) == 0) return(paste0(title, " is empty."))

    # Put regression columns first
    if (put_reg_cols_first && NROW(regvar) > 0) {
      cols = unique(c(regvar$basevar, regvar$cterm, regxvar$cterm))
      cols = intersect(cols, colnames(df))
      df = df[, union(cols, names(df))]
    }

    # Using tibble prints nicely across terminal widths truncating extra cols safely
    df_tibble = tibble::as_tibble(df)


    make_txt = function(df) {
      w = getOption("width")
      options(width=opts$data_width)
      txt = paste0(capture.output(print(df,n = Inf, width=opts$data_width)), collapse = "\n")
      options(width=w)
      txt
    }


    out_head = out_tail = ""

    if (opts$data_head_rows + opts$data_tail_rows >= NROW(df_tibble)) {
      txt = paste0("##", title, " (complete)\n```\n", make_txt(df_tibble),"\n```")
    } else if (opts$data_head_rows>0 & opts$data_tail_rows > 0) {
      txt = paste0("##", title, " (head & tail)\n```\n",
        make_txt(head(df_tibble, opts$data_head_rows)), "\n...", NROW(df_tibble)-opts$data_head_rows-opts$data_tail_rows, " rows ommited ...\n",make_txt(tail(df_tibble, opts$data_tail_rows)),
        "\n```")
    } else if (opts$data_head_rows>0) {
      txt = paste0("##", title, " (head)\n```\n", make_txt(head(df_tibble, opts$data_head_rows)),"\n```")
    } else if (opts$data_tail_rows>0) {
      txt = paste0("##", title, " (tail)\n```\n", make_txt(tail(df_tibble, opts$data_tail_rows)),"\n```")
    } else {
      txt = "opts$data_head_rows=0 and opts$data_tail_rows=0, so no rows shown"
    }

    txt
  }
  if (opts$data_add_org_row) {
    org_row_opt = repboxDRF::drf_set_add_org_row(TRUE)
  }

  res = ""
  if (opts$show_reg_data) {
    # Fetch Regression Data
    regvar = parcel_for_runid(parcels$regvar, runid)
    regxvar = if (!is.null(parcels$regxvar)) parcel_for_runid(parcels$regxvar, runid) else tibble()
    reg = parcel_for_runid(parcels$reg, runid)
    dat = try(mrb_get_regression_data(runid, drf, reg=reg, regvar=regvar, regxvar=regxvar), silent = TRUE)
    res = c(res, format_df_sample(dat, "## Regression Data (fully prepared)", put_reg_cols_first = TRUE), "")
  }
  if (opts$show_pre_reg_data) {
    dat = try(repboxDRF::drf_get_data(runid=runid, drf = drf, before=TRUE,filtered = TRUE), silent = TRUE)
    res = c(res, format_df_sample(dat, "## Data before regression command (filtered)", put_reg_cols_first = TRUE), "")
  }

  if (opts$show_org_data) {
    # 1. Fetch Original Data
    dat = try(repboxDRF::drf_get_data(first_runid, drf = drf, before=FALSE), silent = TRUE)
    res = c(res, format_df_sample(dat, "## Original Data (First Step)"), "")
  }

  if (opts$data_add_org_row) {
    repboxDRF::drf_set_add_org_row(org_row_opt)
  }
  paste0(res, collapse = "\n")
}















