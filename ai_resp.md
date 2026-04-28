Yes. I would make this a small “split-at-the-edge” refactor: keep `ct_to_regcoef()` unchanged, but immediately split its output into parcel-named data frames by `variant`.

Main idea:

`variant == "sb"` -> `regcoef`
`variant == "sb_mfx"` -> `regcoef_sb_mfx`
`variant == "rb"` -> `regcoef_rb`
`variant == "rb_mfx"` -> `regcoef_rb_mfx`

Here are the proposed changes.

!MODIFICATION regcoef variant helpers
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_regcoef.R"
insert_after_fun = "ct_to_regcoef"
description = "Add helpers that map coefficient variants to parcel names and split a regcoef table into variant-specific parcels."
----------------------------------------------------------------------------------------------------------------------------------

```r

regcoef_variant_parcel_name = function(
  variant,
  base_variant = "sb",
  base_parcel = "regcoef",
  root_parcel = "regcoef"
) {
  variant = as.character(variant)
  variant[is.na(variant) | variant == ""] = base_variant

  variant = stringi::stri_replace_all_regex(variant, "[^A-Za-z0-9_]+", "_")

  ifelse(
    variant == base_variant,
    base_parcel,
    paste0(root_parcel, "_", variant)
  )
}


regcoef_split_variant_parcels = function(
  co,
  base_variant = "sb",
  base_parcel = "regcoef",
  root_parcel = "regcoef"
) {
  if (is.null(co) || NROW(co) == 0) {
    return(list())
  }

  if (!"variant" %in% names(co)) {
    co$variant = base_variant
  }

  co$variant = as.character(co$variant)
  co$variant[is.na(co$variant) | co$variant == ""] = base_variant

  parcel_name = regcoef_variant_parcel_name(
    co$variant,
    base_variant = base_variant,
    base_parcel = base_parcel,
    root_parcel = root_parcel
  )

  row_split = split(seq_len(NROW(co)), parcel_name)

  res = lapply(row_split, function(rows) {
    co[rows, , drop = FALSE]
  })

  ord = unique(c(base_parcel, sort(names(res))))
  res[intersect(ord, names(res))]
}
```

!END_MODIFICATION regcoef variant helpers

!MODIFICATION mrb_run_r_base_step regcoef split
scope = "lines"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_base.R"
description = "Replace the regcoef creation block so only variant sb is stored in regcoef and other variants are stored in variant-specific parcels."
-----------------------------------------------------------------------------------------------------------------------------------------------------

```r
  # A. REGCOEF (Parsed Stata Coefficients from metaregBase runs)
  if (!is.null(stata_ct) && nrow(stata_ct) > 0) {
    # Split all coefficient variants into separate parcels.
    # The only naming outlier is sb, which remains stored in regcoef.
    co_all = ct_to_regcoef(stata_ct, artid = mrb$artid)
    co_parcels = regcoef_split_variant_parcels(
      co_all,
      base_variant = "sb",
      base_parcel = "regcoef"
    )
    step_parcels[names(co_parcels)] = co_parcels

    regcoef_main = if (!is.null(step_parcels$regcoef)) {
      regcoef_keep_default_eq(step_parcels$regcoef)
    } else {
      tibble()
    }
  } else {
    step_parcels$regcoef = tibble()
    regcoef_main = tibble()
  }
```

!END_MODIFICATION mrb_run_r_base_step regcoef split

!MODIFICATION mrb_make_r_base_parcels dynamic regcoef variants
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_base.R"
function_name = "mrb_make_r_base_parcels"
description = "Save dynamically generated regcoef_* variant parcels, while keeping known parcels checked normally."
-------------------------------------------------------------------------------------------------------------------

```r
# The step parcels are generated in mrb_r
mrb_make_r_base_parcels = function(mrb, save=TRUE, is_partial_run = isTRUE(mrb$is_partial_run)) {
  restore.point("mrb_make_r_base_parcels")

  all_step_parcels = mrb$all_step_parcels
  if (is.null(all_step_parcels)) {
    cat("\nmrb_save_step_parcels: mrb$all_step_parcels were not yet generated. Make sure mrb_run_r_base is called beforehand.\n")
    return(mrb)
  }

  step_fields = unique(unlist(lapply(all_step_parcels, names), use.names = FALSE))
  extra_regcoef_fields = grep("^regcoef_", step_fields, value = TRUE)
  extra_regcoef_fields = setdiff(
    extra_regcoef_fields,
    c("regcoef_so", "regcoef_rb", "regcoef_diff")
  )
  extra_regcoef_fields = sort(extra_regcoef_fields)

  if (is_partial_run) {
    mrb$parcels = repdb_load_parcels(
      mrb$project_dir,
      c(
        "reg", "regcoef", "regcoef_so", "regvar", "regxvar",
        "colstat_numeric", "colstat_dummy", "colstat_factor",
        "colinfo", "regscalar", "regstring",
        extra_regcoef_fields
      ),
      mrb$parcels
    )
  }

  parcels = list()

  combine_steps = function(field) {
    res_list = lapply(all_step_parcels, function(x) x[[field]])
    res_list = res_list[!sapply(res_list, is.null)]

    if (length(res_list) == 0) {
      new_data = tibble()
    } else {
      new_data = bind_rows(res_list)
    }

    if (isTRUE(is_partial_run) && !is.null(mrb$parcels[[field]])) {
      old_data = mrb$parcels[[field]]
      if (NROW(old_data) > 0 && NROW(new_data) > 0) {
        old_kept = old_data[!old_data$runid %in% mrb$partial_pids, , drop = FALSE]
        new_data = bind_rows(old_kept, new_data)
      } else if (NROW(old_data) > 0 && NROW(new_data) == 0) {
        new_data = old_data
      }
    }

    new_data
  }

  # reg
  parcels$reg = combine_steps("reg")

  # Coefs & Variables
  parcels$regcoef = combine_steps("regcoef")
  parcels$regcoef_so = combine_steps("regcoef_so")

  for (field in extra_regcoef_fields) {
    parcels[[field]] = combine_steps(field)
  }

  parcels$regvar = combine_steps("regvar")
  parcels$regxvar = combine_steps("regxvar")

  # Column Stats
  parcels$colstat_numeric = combine_steps("colstat_numeric")
  parcels$colstat_dummy = combine_steps("colstat_dummy")
  parcels$colstat_factor = combine_steps("colstat_factor")

  parcels$colinfo = combine_steps("colinfo")

  # Scalars & Macros
  parcels$regscalar = combine_steps("regscalar")
  parcels$regstring = combine_steps("regstring")

  # regsource parcel is just a combination of existing parcels
  mrb$parcels = repdb_load_parcels(mrb$project_dir, c("stata_file", "stata_cmd"), parcels = mrb$parcels)
  run_df = mrb$drf$run_df

  regsource = parcels$reg %>%
    select(runid) %>%
    left_join(run_df %>% select(runid, file_path, line), by="runid") %>%
    left_join(mrb$parcels$stata_cmd %>% select(file_path, line, code_line_start=orgline_start, code_line_end = orgline_end), by = c("file_path", "line")) %>%
    left_join(mrb$parcels$stata_file, by="file_path") %>%
    rename(script_path = file_path, script_name = file_name,script_type = file_type) %>%
    mutate(script_file = basename(script_path))

  parcels$regsource = regsource

  if (save) {
    repdb_dir = file.path(mrb$project_dir, "repdb")

    static_parcels = parcels[setdiff(names(parcels), extra_regcoef_fields)]
    repboxDB::repdb_save_parcels(static_parcels, repdb_dir, check = TRUE)

    # Dynamic variant parcels use the regcoef schema but have dynamic names,
    # so they are saved without table-name based checking.
    if (length(extra_regcoef_fields) > 0) {
      extra_parcels = parcels[extra_regcoef_fields]
      repboxDB::repdb_save_parcels(extra_parcels, repdb_dir, check = FALSE)
    }
  }

  mrb$parcels[names(parcels)] = parcels
  return(mrb)
}
```

!END_MODIFICATION mrb_make_r_base_parcels dynamic regcoef variants

!MODIFICATION mrb_run_r_reg_step rb variant split
scope = "lines"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
description = "When R ever creates multiple coefficient variants, split them into regcoef_rb and regcoef_<variant> parcels."
----------------------------------------------------------------------------------------------------------------------------

```r
    co_df = ct_to_regcoef(ct, lang="r", variant="rb", artid=artid, default_eq=default_eq)
    co_df$runid = runid

    co_parcels = regcoef_split_variant_parcels(
      co_df,
      base_variant = "rb",
      base_parcel = "regcoef_rb"
    )
    step_parcels[names(co_parcels)] = co_parcels
```

!END_MODIFICATION mrb_run_r_reg_step rb variant split

!MODIFICATION mrb_make_r_reg_parcels dynamic rb variants
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
function_name = "mrb_make_r_reg_parcels"
description = "Save dynamically generated R-side regcoef_* variant parcels, e.g. regcoef_rb_mfx, if they ever appear."
----------------------------------------------------------------------------------------------------------------------

```r
# The step parcels are generated in mrb_r
mrb_make_r_reg_parcels = function(mrb, save=TRUE,is_partial_run=mrb$is_partial_run) {
  restore.point("mrb_make_r_reg_parcels")

  all_step_parcels = mrb$all_step_parcels
  if (is.null(all_step_parcels)) {
    cat("\nAll step parcels were not generated in mrb.\n")
    return(mrb)
  }

  step_fields = unique(unlist(lapply(all_step_parcels, names), use.names = FALSE))
  extra_regcoef_fields = grep("^regcoef_", step_fields, value = TRUE)
  extra_regcoef_fields = setdiff(
    extra_regcoef_fields,
    c("regcoef_rb", "regcoef_so", "regcoef_diff")
  )
  extra_regcoef_fields = sort(extra_regcoef_fields)

  if (is_partial_run) {
    mrb$parcels = repdb_load_parcels(
      mrb$project_dir,
      c(
        "reg_rb", "regcoef_rb", "regcoef_diff",
        "regscalar_rb", "regstring_rb",
        extra_regcoef_fields
      )
    )
  }

  parcels = list()

  combine_steps = function(field, check_table = field) {
    res_list = lapply(all_step_parcels, function(x) x[[field]])
    res_list = res_list[!sapply(res_list, is.null)]

    if (length(res_list) == 0) {
      new_data = tibble()
    } else {
      new_data = bind_rows(res_list)
    }

    if (isTRUE(is_partial_run) && !is.null(mrb$parcels[[field]])) {
      old_data = mrb$parcels[[field]]
      if (NROW(old_data) > 0 && NROW(new_data) > 0) {
        old_kept = old_data[!old_data$runid %in% mrb$partial_pids, , drop = FALSE]
        new_data = bind_rows(old_kept, new_data)
      } else if (NROW(old_data) > 0 && NROW(new_data) == 0) {
        new_data = old_data
      }
    }

    if (NROW(new_data) > 0) {
      repdb_check_data(new_data, table=check_table)
    }

    new_data
  }

  parcels$reg_rb = combine_steps("reg_rb")
  parcels$regcoef_rb = combine_steps("regcoef_rb")
  parcels$regcoef_diff = combine_steps("regcoef_diff")
  parcels$regscalar_rb = combine_steps("regscalar_rb")
  parcels$regstring_rb = combine_steps("regstring_rb")

  for (field in extra_regcoef_fields) {
    parcels[[field]] = combine_steps(field, check_table = "regcoef")
  }

  if (save) {
    repdb_dir = file.path(mrb$project_dir, "repdb")

    static_parcels = parcels[setdiff(names(parcels), extra_regcoef_fields)]
    repboxDB::repdb_save_parcels(static_parcels, repdb_dir, check = TRUE)

    if (length(extra_regcoef_fields) > 0) {
      extra_parcels = parcels[extra_regcoef_fields]
      repboxDB::repdb_save_parcels(extra_parcels, repdb_dir, check = FALSE)
    }
  }

  mrb$parcels[names(parcels)] = parcels
  return(mrb)
}
```

!END_MODIFICATION mrb_make_r_reg_parcels dynamic rb variants

The key behavioral change is in `mrb_run_r_base_step`: `step_parcels$regcoef` now only receives `variant == "sb"`. Any additional Stata variant from `stata_ct`, such as `sb_mfx`, is still preserved, but under its own parcel name like `regcoef_sb_mfx`.

The dynamic save logic keeps normal `repdb` checking for known parcels and uses `check = FALSE` only for dynamic coefficient parcel names, because their schema is really `regcoef` but their table names are not known in advance.
