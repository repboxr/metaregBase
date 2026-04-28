# FILE: /home/rstudio/repbox/metaregBase/R/mrb_regcheck.R

#' Assemble the 'regcheck' parcel checking cross-language replication success
#'
#' Evaluates the success of regression outputs and maps any mismatches
#' to a standardized `regcheck` parcel.
mrb_make_regcheck_parcel = function(mrb, save = TRUE, just_pids=NULL, repair_code="") {
  restore.point("mrb_make_regcheck_parcel")

  mrb$parcels = parcels = repboxDB::repdb_load_parcels(mrb$project_dir, c("reg", "reg_rb", "regcoef", "regcoef_so", "regcoef_rb"), mrb$parcels)

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

  res_li = lapply(pids, function(pid) {
    # Boolean checks of run existence
    so_did_run = !is.null(parcels$regcoef_so) && pid %in% parcels$regcoef_so$runid
    sb_did_run = !is.null(parcels$reg) && pid %in% parcels$reg$runid

    rb_did_run = FALSE
    error_msg = ""
    if (!is.null(parcels$reg_rb) && pid %in% parcels$reg_rb$runid) {
      row = parcels$reg_rb[parcels$reg_rb$runid == pid, , drop = FALSE][1,]
      rb_did_run = !isTRUE(row$error_in_r)
      error_msg = if (!is.na(row$error_msg)) row$error_msg else ""
    }

    # Default parameters to return
    sb_so_identical = NA
    rb_sb_coef_same = NA
    rb_sb_coef_max_dev = NA_real_
    rb_sb_se_same = NA
    rb_sb_se_max_dev = NA_real_
    problem = ""
    comment = ""

    # Assess discrepancies via Diff summaries
    if (sb_did_run && so_did_run) {
      co_sb = parcels$regcoef[parcels$regcoef$runid == pid, , drop = FALSE]
      co_so = parcels$regcoef_so[parcels$regcoef_so$runid == pid, , drop = FALSE]
      diff_so = coef_diff_table(co_sb, co_so)
      if (!is.null(diff_so) && NROW(diff_so) > 0) {
        sb_so_identical = all(diff_so$identical, na.rm = TRUE)
      } else {
        sb_so_identical = FALSE
      }
    }

    if (sb_did_run && rb_did_run) {
      co_sb = parcels$regcoef[parcels$regcoef$runid == pid, , drop = FALSE]
      co_rb = parcels$regcoef_rb[parcels$regcoef_rb$runid == pid, , drop = FALSE]
      diff_rb = coef_diff_table(co_sb, co_rb)
      if (!is.null(diff_rb) && NROW(diff_rb) > 0) {
        rb_sb_coef_max_dev = max_empty_na(diff_rb$rel_err_coef, na.rm = TRUE)
        rb_sb_se_max_dev = max_empty_na(diff_rb$rel_err_se, na.rm = TRUE)
        rb_sb_coef_same = isTRUE(rb_sb_coef_max_dev <= 0.01)
        rb_sb_se_same = isTRUE(rb_sb_se_max_dev <= 0.01)
      } else {
        rb_sb_coef_same = FALSE
        rb_sb_se_same = FALSE
      }
    }

    # Evaluate any detected problem strings
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
      problem = "R and Stata base coefficients differ by > 1%."
    } else if (isTRUE(!sb_so_identical)) {
      problem = "Stata base differs from Stata original."
    }

    reg_ok = isTRUE(so_did_run) && isTRUE(sb_did_run) && isTRUE(rb_did_run) &&
             isTRUE(sb_so_identical) && isTRUE(rb_sb_coef_same) && isTRUE(rb_sb_se_same)

    dplyr::tibble(
      runid = as.integer(pid),
      reg_ok = reg_ok,
      so_did_run = so_did_run,
      sb_did_run = sb_did_run,
      rb_did_run = rb_did_run,
      sb_so_identical = sb_so_identical,
      rb_sb_coef_same = rb_sb_coef_same,
      rb_sb_coef_max_dev = rb_sb_coef_max_dev,
      rb_sb_se_same = rb_sb_se_same,
      rb_sb_se_max_dev = rb_sb_se_max_dev,
      repair_code = repair_code,
      problem = problem,
      comment = comment
    )
  })

  regcheck = dplyr::bind_rows(res_li)

  regcheck$so_raw_did_run = regcheck$so_did_run | regcheck$runid %in% mrb$regtab_so$runid
  regcheck$sb_raw_did_run = regcheck$sb_did_run | regcheck$runid %in% mrb$stata_ct_sb$runid



  if (!is.null(just_pids)) {
    parcels = repboxDB::repdb_load_parcels(mrb$project_dir, "regcheck", parcels)
    old_regcheck = parcels$regcheck %>% anti_join(regcheck, by="runid")
    regcheck = bind_rows(regcheck, old_regcheck) %>% arrange(runid)
  }

  if (save) {
    repboxDB::repdb_save_parcels(list(regcheck = regcheck), file.path(mrb$project_dir, "repdb"), check = FALSE)
  }

  mrb$parcels$regcheck = regcheck
  return(mrb)
}
