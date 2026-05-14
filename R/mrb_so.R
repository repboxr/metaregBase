# Original Stata reproduction parcels
#
# These parcels are generated from the original repbox Stata reproduction
# results, not from the metaregBase sb/rb reconstruction pipeline.
#
# For now we only create regcoef_so. We deliberately do not create reg or
# reg_so rows, because the reg parcel represents standardized metaregBase
# regression metadata and much of that information may be unavailable when
# only the original Stata coefficient table exists.

mrb_make_so_parcels = function(
  mrb,
  save = TRUE,
  just_pids = NULL,
  regtab_file = file.path(mrb$project_dir, "repbox/stata/regtab.Rds"),
  variant = "so"
) {
  restore.point("mrb_make_so_parcels")

  if (is.null(mrb$artid)) {
    mrb$artid = basename(mrb$project_dir)
  }

  if (!file.exists(regtab_file)) {
    mrb$regtab_so = NULL
    mrb$regcoef_so = tibble::tibble()

    if (save) {
      repboxDB::repdb_save_parcels(
        list(regcoef_so = mrb$regcoef_so),
        file.path(mrb$project_dir, "repdb"),
        check = TRUE
      )
    }

    mrb$parcels$regcoef_so = mrb$regcoef_so
    return(mrb)
  }

  regtab_so = readRDS(regtab_file)

  if (!is.null(just_pids) && NROW(regtab_so) > 0 && "runid" %in% names(regtab_so)) {
    regtab_so = regtab_so[regtab_so$runid %in% just_pids, , drop = FALSE]
  }

  regcoef_so = mrb_regtab_so_to_regcoef_so(
    regtab_so = regtab_so,
    artid = mrb$artid,
    variant = variant
  )

  if (!is.null(just_pids)) {
    mrb$parcels = repboxDB::repdb_load_parcels(
      mrb$project_dir,
      "regcoef_so",
      parcels = mrb$parcels
    )

    old_regcoef_so = mrb$parcels$regcoef_so
    if (!is.null(old_regcoef_so) && NROW(old_regcoef_so) > 0) {
      old_regcoef_so = old_regcoef_so[!old_regcoef_so$runid %in% just_pids, , drop = FALSE]
      regcoef_so = dplyr::bind_rows(old_regcoef_so, regcoef_so)
    }
  }

  if (NROW(regcoef_so) > 0) {
    regcoef_so = regcoef_so %>%
      dplyr::arrange(runid, variant, eq, cterm)
  }

  mrb$regtab_so = regtab_so
  mrb$regcoef_so = regcoef_so
  mrb$parcels$regcoef_so = regcoef_so

  if (save) {
    repboxDB::repdb_save_parcels(
      list(regcoef_so = regcoef_so),
      file.path(mrb$project_dir, "repdb"),
      check = TRUE
    )
  }

  mrb
}


mrb_regtab_so_to_regcoef_so = function(regtab_so, artid = NULL, variant = "so") {
  restore.point("mrb_regtab_so_to_regcoef_so")

  if (is.null(regtab_so) || NROW(regtab_so) == 0) {
    return(tibble::tibble())
  }

  if (!"runid" %in% names(regtab_so)) {
    stop("Cannot create regcoef_so because regtab_so has no runid column.")
  }

  if (!"ct" %in% names(regtab_so)) {
    stop("Cannot create regcoef_so because regtab_so has no ct column.")
  }

  has_ct = !vapply(regtab_so$ct, is.null, logical(1))
  if (!any(has_ct)) {
    return(tibble::tibble())
  }

  rows = which(has_ct)

  li = lapply(rows, function(row) {
    ct = regtab_so$ct[[row]]

    if (is.null(ct) || NROW(ct) == 0) {
      return(tibble::tibble())
    }

    ct = tibble::as_tibble(ct)

    if (!"var" %in% names(ct)) {
      stop(paste0("Cannot create regcoef_so for runid ", regtab_so$runid[[row]], " because ct has no var column."))
    }

    if (!"label" %in% names(ct)) {
      ct$label = ""
    }

    ct$runid = as.integer(regtab_so$runid[[row]])

    res = ct_to_regcoef(
      ct = ct,
      lang = "stata",
      variant = variant,
      artid = artid
    )

    if (is.null(res)) {
      return(tibble::tibble())
    }

    res
  })

  dplyr::bind_rows(li)
}
