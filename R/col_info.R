# Store information about all columns in data set (not only those used in the regression)

get_var_labels = function(dat) {
  #var.labels <- attr(org_dat,"var.labels")
  #if (!is.null(var.labels)) {
  #  stop("Need to deal with variable labels!")
  #}
  labs= rep("", NCOL(dat))
  names(labs) = names(dat)

  labels = unlist(lapply(dat, function(v) attr(v, "label")))
  labs[names(labels)] = labels
  labs

}

repbox_col_class = function(v, distinct_num = n_distinct(v,na.rm = TRUE)) {
  restore.point("repbox_col_class")
  if (is.numeric(v)) {

    if (length(distinct_num)>0 & length(distinct_num)<=2) {
      uni = unique(v)
      if (isTRUE(all(uni %in% c(0,1), na.rm=TRUE))) {
        return("dummy")
      }
    }
    # need to add try since integer conversion sometimes fails
    # with error
    if (isTRUE((try(all(v==suppressWarnings(as_integer(v)),na.rm=TRUE), silent=TRUE)))) return("integer")
    return("numeric")
  }
  atomic_class(v)
}




repbox_compute_step_col_info = function(step, project_dir,dat, org_dat, reg) {
  restore.point("repbox_store_all_col_info")
  #stop()
  cols = names(org_dat)
  # Only use rows used in regression
  dat = dat[cols]

  var_labels <-get_var_labels(org_dat)

  colinfo_li = lapply(cols, function(col) {
    restore.point("jhsjkfhkshdfjh")
    val = dat[[col]]
    org_val = org_dat[[col]]

    val_tab = c(sort(table(val), decreasing = TRUE),NA,NA)[1:2]
    top = c(names(val_tab),NA_character_,NA_character_)
    distinct_num_org = n_distinct(org_val, na.rm = TRUE)
    distinct_num = n_distinct(val, na.rm = TRUE)


    res = list(
      step = step,
      var = col,
      label = var_labels[col], # TO DO: DEAL WITH LABELS
      col_type_org = atomic_class(org_val),
      col_type = repbox_col_class(val,distinct_num),
      var_group = "",
      dummy_set = "", # TO DO
      md5_org = digest::digest(org_val),
      nobs_org = NROW(org_dat),
      nobs = NROW(dat),
      na_num_org = sum(is.na(org_val)),
      na_num = sum(is.na(val)),
      distinct_num_org = distinct_num_org,
      distinct_num = distinct_num,
      mean = NA_real_, sd = NA_real_, min = NA_real_, max=NA_real_,

      top1_val = top[1],
      top1_num = val_tab[top[1]],

      top2_val = top[2],
      top2_num = val_tab[top[2]]
    )

    if (is.numeric(val) & distinct_num >0) {
      res$mean = mean(val, na.rm=TRUE)
      res$sd = sd(val, na.rm=TRUE)
      res$min = min(val, na.rm=TRUE)
      res$max = max(val, na.rm=TRUE)
    }
    #as_tibble(res)
    res
  })
  df = bind_rows(colinfo_li)


  # Col groups
  col_group = stri_match_first_regex(cols, "^[a-zA-Z][a-zA-Z_]*(?=[0-9]+$)")[,1] %>% na.val("")
  # Also consider xi generated variables like _Iyear_2009
  col_group = stri_match_first_regex(cols, "^[a-zA-Z_]+(?=[0-9]+$)")[,1] %>% na.val("")
  dupl_col_group = unique(col_group[duplicated(col_group)])
  col_group[!col_group %in% dupl_col_group] = ""
  df$var_group = col_group

  dummy_var_groups = df %>%
    filter(var_group != "") %>%
    group_by(var_group) %>%
    summarize(keep = all(col_type=="dummy")) %>%
    filter(keep)

  for (group in dummy_var_groups$var_group) {
    gcols = which(df$var_group==group)
    mat = as.matrix(org_dat[gcols])
    rsum = rowSums(mat)
    if (all(rsum==1, na.rm=TRUE) & !all(is.na(rsum))) {
      df$dummy_set[gcols] = group
    }
  }
  df

}

# Update for vargroup and dummy_set definitions that can be applied on
# given colinfo
recompute_colinfo_groups = function(colinfo) {
  restore.point("recompute_colinfo_groups")
  df = colinfo
  cols = colinfo$var
  # Col groups
  #col_group = stri_match_first_regex(cols, "^[a-zA-Z][a-zA-Z_]*(?=[0-9]+$)")[,1] %>% na.val("")
  # Also consider xi generated variables like _Iyear_2009
  col_group = stri_match_first_regex(cols, "^[a-zA-Z_]+(?=[0-9]+$)")[,1] %>% na.val("")
  dupl_col_group = unique(col_group[duplicated(col_group)])
  col_group[!col_group %in% dupl_col_group] = ""
  df$var_group = col_group

  df$dummy_set = case_when(
    df$dummy_set != "" ~ df$dummy_set,
    startsWith(df$var_group, "_I") ~ df$var_group,
    TRUE ~ ""
  )
  df
}
