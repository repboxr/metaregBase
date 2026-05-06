#' Assemble the 'regcheck' parcel checking cross-language replication success
#'
#' Evaluates the success of regression outputs and maps any mismatches
#' to a standardized `regcheck` parcel.
mrb_make_regcheck_parcel = function(
  mrb,
  save = TRUE,
  just_pids = NULL,
  repair_code = "",
  max_rel_diff_tol = 1e-6,
  max_deviation_tol = 1e-8,
  rb_max_rel_diff_tol = 0.01,
  rb_max_deviation_tol = 1e-6
) {
  restore.point("mrb_make_regcheck_parcel")

  mrb$parcels = parcels = repboxDB::repdb_load_parcels(
    mrb$project_dir,
    c("reg", "reg_rb", "regcoef", "regcoef_so", "regcoef_rb"),
    mrb$parcels
  )

  pids = unique(c(
    if (!is.null(parcels$reg)) parcels$reg$runid else integer(),
    if (!is.null(parcels$reg_rb)) parcels$reg_rb$runid else integer(),
    if (!is.null(parcels$regcoef_so)) parcels$regcoef_so$runid else integer(),
    if (!is.null(mrb$drf$pids)) mrb$drf$pids else integer()
  ))

  if (length(pids) == 0) return(mrb)

  if (!is.null(just_pids)) {
    pids = intersect(pids, just_pids)
  }

  pid = pids[1]

  res_li = lapply(pids, function(pid) {
    so_did_run = !is.null(parcels$regcoef_so) && pid %in% parcels$regcoef_so$runid
    sb_did_run = !is.null(parcels$reg) && pid %in% parcels$reg$runid

    run_cmd = ""
    if (!is.null(parcels$reg) && "cmd" %in% names(parcels$reg) && pid %in% parcels$reg$runid) {
      reg_row = parcels$reg[parcels$reg$runid == pid, , drop = FALSE][1, ]
      run_cmd = as.character(reg_row$cmd[1])
      if (is.na(run_cmd)) run_cmd = ""
    }

    rb_did_run = FALSE
    error_msg = ""
    if (!is.null(parcels$reg_rb) && pid %in% parcels$reg_rb$runid) {
      rb_row = parcels$reg_rb[parcels$reg_rb$runid == pid, , drop = FALSE][1, ]
      rb_did_run = !isTRUE(rb_row$error_in_r)

      if ("error_msg" %in% names(rb_row) && !is.na(rb_row$error_msg[1])) {
        error_msg = as.character(rb_row$error_msg[1])
      }
    }

    sb_so_identical = NA
    sb_so_coef_same = NA
    sb_so_coef_max_dev = NA_real_
    sb_so_coef_max_rel = NA_real_
    sb_so_se_same = NA
    sb_so_se_max_dev = NA_real_
    sb_so_se_max_rel = NA_real_

    rb_sb_coef_same = NA
    rb_sb_coef_max_dev = NA_real_
    rb_sb_coef_max_rel = NA_real_
    rb_sb_se_same = NA
    rb_sb_se_max_dev = NA_real_
    rb_sb_se_max_rel = NA_real_

    problem = ""
    comment = ""

    if (sb_did_run && so_did_run) {
      co_sb = parcels$regcoef[parcels$regcoef$runid == pid, , drop = FALSE]
      co_so = parcels$regcoef_so[parcels$regcoef_so$runid == pid, , drop = FALSE]

      diff_so = coef_diff_table(co_sb, co_so, cmd = run_cmd)
      ev_so = mrb_regcheck_diff_eval(
        diff_so,
        max_rel_diff_tol = max_rel_diff_tol,
        max_deviation_tol = max_deviation_tol
      )

      sb_so_identical = ev_so$all_same
      sb_so_coef_same = ev_so$coef_same

      # The regcheck parcel spec defines sb_so_coef_max_dev as the
      # maximum relative coefficient deviation between sb and so.
      sb_so_coef_max_dev = ev_so$coef_max_rel
      sb_so_coef_max_rel = ev_so$coef_max_rel

      sb_so_se_same = ev_so$se_same
      sb_so_se_max_dev = ev_so$se_max_dev
      sb_so_se_max_rel = ev_so$se_max_rel
    }

    if (sb_did_run && rb_did_run) {
      co_sb = parcels$regcoef[parcels$regcoef$runid == pid, , drop = FALSE]
      co_rb = parcels$regcoef_rb[parcels$regcoef_rb$runid == pid, , drop = FALSE]

      diff_rb = coef_diff_table(co_sb, co_rb, cmd = run_cmd)
      ev_rb = mrb_regcheck_diff_eval(
        diff_rb,
        max_rel_diff_tol = rb_max_rel_diff_tol,
        max_deviation_tol = rb_max_deviation_tol
      )

      rb_sb_coef_same = ev_rb$coef_same
      rb_sb_coef_max_dev = ev_rb$coef_max_dev
      rb_sb_coef_max_rel = ev_rb$coef_max_rel
      rb_sb_se_same = ev_rb$se_same
      rb_sb_se_max_dev = ev_rb$se_max_dev
      rb_sb_se_max_rel = ev_rb$se_max_rel
    }

    if (!rb_did_run & !sb_did_run & !so_did_run) {
      problem = "All reproductions failed: so, sb and rb"
    } else if (!sb_did_run & !rb_did_run) {
      problem = "metaregBase reproductions failed: sb and rb"
    } else if (!rb_did_run) {
      problem = paste0("R replication rb failed: ", error_msg)
    } else if (!sb_did_run) {
      problem = "Stata base sb replication failed, but rb did run."
    } else if (!so_did_run) {
      problem = "Original Stata reproduction results missing."
    } else if (isTRUE(!rb_sb_coef_same)) {
      problem = "R and Stata base coefficients differ by > tolerance."
    } else if (isTRUE(!sb_so_identical)) {
      problem = "Stata base differs from Stata original."
    }

    reg_ok = isTRUE(so_did_run) &&
      isTRUE(sb_did_run) &&
      isTRUE(rb_did_run) &&
      isTRUE(sb_so_identical) &&
      isTRUE(rb_sb_coef_same) &&
      isTRUE(rb_sb_se_same)

    dplyr::tibble(
      runid = as.integer(pid),
      cmd = run_cmd,
      reg_ok = reg_ok,
      so_did_run = so_did_run,
      sb_did_run = sb_did_run,
      rb_did_run = rb_did_run,

      sb_so_identical = sb_so_identical,
      sb_so_coef_same = sb_so_coef_same,
      sb_so_coef_max_dev = sb_so_coef_max_dev,
      sb_so_coef_max_rel = sb_so_coef_max_rel,
      sb_so_se_same = sb_so_se_same,
      sb_so_se_max_dev = sb_so_se_max_dev,
      sb_so_se_max_rel = sb_so_se_max_rel,

      rb_sb_coef_same = rb_sb_coef_same,
      rb_sb_coef_max_dev = rb_sb_coef_max_dev,
      rb_sb_coef_max_rel = rb_sb_coef_max_rel,
      rb_sb_se_same = rb_sb_se_same,
      rb_sb_se_max_dev = rb_sb_se_max_dev,
      rb_sb_se_max_rel = rb_sb_se_max_rel,

      repair_code = repair_code,
      problem = problem,
      comment = comment
    )
  })

  regcheck = dplyr::bind_rows(res_li)

  regcheck$so_raw_did_run = regcheck$so_did_run
  if (!is.null(mrb$regtab_so) && "runid" %in% names(mrb$regtab_so)) {
    regcheck$so_raw_did_run = regcheck$so_raw_did_run | regcheck$runid %in% mrb$regtab_so$runid
  }

  regcheck$sb_raw_did_run = regcheck$sb_did_run
  if (!is.null(mrb$stata_ct_sb) && "runid" %in% names(mrb$stata_ct_sb)) {
    regcheck$sb_raw_did_run = regcheck$sb_raw_did_run | regcheck$runid %in% mrb$stata_ct_sb$runid
  }

  if (!is.null(just_pids)) {
    parcels = repboxDB::repdb_load_parcels(mrb$project_dir, "regcheck", parcels)
    old_regcheck = parcels$regcheck %>% anti_join(regcheck, by = "runid")
    regcheck = bind_rows(regcheck, old_regcheck) %>% arrange(runid)
  }

  if (save) {
    repboxDB::repdb_save_parcels(
      list(regcheck = regcheck),
      file.path(mrb$project_dir, "repdb"),
      check = FALSE
    )
  }

  mrb$parcels$regcheck = regcheck
  return(mrb)
}

mrb_regcheck_numeric_same = function(
  abs_err,
  rel_err,
  missing_one = FALSE,
  max_rel_diff_tol = 1e-8,
  max_deviation_tol = 1e-10
) {
  restore.point("mrb_regcheck_numeric_same")

  if (length(abs_err) == 0) {
    return(FALSE)
  }

  missing_one[is.na(missing_one)] = FALSE

  bad_missing = isTRUE(any(missing_one))
  if (bad_missing) {
    return(FALSE)
  }

  same = is.na(abs_err) |
    abs_err <= max_deviation_tol |
    (!is.na(rel_err) & rel_err <= max_rel_diff_tol)

  isTRUE(all(same))
}


mrb_regcheck_diff_eval = function(
  diff_tab,
  max_rel_diff_tol = 1e-8,
  max_deviation_tol = 1e-10
) {
  restore.point("mrb_regcheck_diff_eval")

  if (is.null(diff_tab) || NROW(diff_tab) == 0) {
    return(list(
      coef_same = FALSE,
      se_same = FALSE,
      all_same = FALSE,
      coef_max_dev = NA_real_,
      coef_max_rel = NA_real_,
      se_max_dev = NA_real_,
      se_max_rel = NA_real_
    ))
  }

  coef_missing_one = xor(is.na(diff_tab$coef_1), is.na(diff_tab$coef_2))
  se_missing_one = xor(is.na(diff_tab$se_1), is.na(diff_tab$se_2))

  coef_same = mrb_regcheck_numeric_same(
    abs_err = diff_tab$abs_err_coef,
    rel_err = diff_tab$rel_err_coef,
    missing_one = coef_missing_one,
    max_rel_diff_tol = max_rel_diff_tol,
    max_deviation_tol = max_deviation_tol
  )

  se_same = mrb_regcheck_numeric_same(
    abs_err = diff_tab$abs_err_se,
    rel_err = diff_tab$rel_err_se,
    missing_one = se_missing_one,
    max_rel_diff_tol = max_rel_diff_tol,
    max_deviation_tol = max_deviation_tol
  )

  list(
    coef_same = coef_same,
    se_same = se_same,
    all_same = isTRUE(coef_same) && isTRUE(se_same),
    coef_max_dev = max_empty_na(diff_tab$abs_err_coef, na.rm = TRUE),
    coef_max_rel = max_empty_na(diff_tab$rel_err_coef, na.rm = TRUE),
    se_max_dev = max_empty_na(diff_tab$abs_err_se, na.rm = TRUE),
    se_max_rel = max_empty_na(diff_tab$rel_err_se, na.rm = TRUE)
  )
}
