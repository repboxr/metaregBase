make_cols_small_info = function(df, cols = colnames(df)) {
  restore.point("make_cols_small_info")
  distinct_num = sapply(cols, function(col) n_distinct(df[[col]]))
  class = sapply(seq_along(cols), function(j) {
    #restore.point("shkjfhkdhfkkd")
    v = df[[ cols[j] ]]
    if (is.numeric(v)) {
      if (length(distinct_num[j])>0 & length(distinct_num[j])<=2) {
        uni = unique(v)
        if (all(uni %in% c(0,1))) {
          return("dummy")
        }
      }
      # need to add try since integer conversion sometimes fails
      # with error
      if (isTRUE(try(all(v==suppressWarnings(as_integer(v)),na.rm=TRUE), silent=TRUE))) return("integer")
      return("numeric")
    }
    return(class(v)[[1]])
  })

  tibble(col = cols, class=class, distinct_num = distinct_num)

}

# Make column statistics as mainly stored later in colstat_num and colstat_factor
# Make column statistics as mainly stored later in colstat_num and colstat_factor
make_cols_info = function(df, cols=colnames(df)) {
  restore.point("make_cols_info")
  df = df[,cols, drop=FALSE]
  distinct_num = sapply(df, n_distinct, na.rm=TRUE)
  distinct_vals = vector("list", NCOL(df))
  use = distinct_num <= 5
  distinct_vals[use] = lapply(df[use], function(v) unique(na.omit(v)))

  na_num = sapply(df, function(v) sum(is.na(v)))
  na_share = na_num / NROW(df)

  class = sapply(seq_along(cols), function(j) {
    v = df[[j]]
    if (is.numeric(v)) {
      if (distinct_num[[j]] == 2 | distinct_num[[j]]==1) {
        if (length(setdiff(distinct_vals[[j]], 0:1))==0)
          return("dummy")
      }
      if (isTRUE(try(all(v==suppressWarnings(as_integer(v)),na.rm=TRUE), silent=TRUE))) return("integer")
      return("numeric")
    }
    return(class(v))[1]
  })
  is_numeric = class %in% c("numeric","dummy","integer","Date","POSIXct","POSIXt")

  mean = sd = min = max = rep(NA_real_, NCOL(df))

  min.na.fun = function(vals) {
    if (all(is.na(vals))) return(NA_real_)
    if (inherits(vals, c("Date", "POSIXt", "difftime"))) vals = as.numeric(vals)
    min(vals, na.rm=TRUE)
  }

  max.na.fun = function(vals) {
    if (all(is.na(vals))) return(NA_real_)
    if (inherits(vals, c("Date", "POSIXt", "difftime"))) vals = as.numeric(vals)
    max(vals, na.rm=TRUE)
  }

  safe_mean = function(vals) {
    if (inherits(vals, c("Date", "POSIXt", "difftime"))) vals = as.numeric(vals)
    mean(vals, na.rm=TRUE)
  }

  safe_sd = function(vals) {
    if (inherits(vals, c("Date", "POSIXt", "difftime"))) vals = as.numeric(vals)
    sd(vals, na.rm=TRUE)
  }

  if (any(is_numeric)) {
    mean[is_numeric] = sapply(df[is_numeric], safe_mean)
    sd[is_numeric] = sapply(df[is_numeric], safe_sd)
    min[is_numeric] = sapply(df[is_numeric], min.na.fun)
    max[is_numeric] = sapply(df[is_numeric], max.na.fun)
  }

  quantiles = matrix(NA_real_, nrow=length(cols), ncol=5)
  colnames(quantiles) = c("q10","q25","median","q75","q90")

  # Quantiles don't work natively on Date objects without type=1 or converting
  rows = which(class %in% c("numeric","dummy","integer","Date","POSIXct","POSIXt"))
  for (i in rows) {
    vals = df[[i]]
    if (inherits(vals, c("Date", "POSIXt", "difftime"))) vals = as.numeric(vals)
    qu = quantile(vals, c(0.1,0.25,0.5,0.75,0.9), na.rm = TRUE)
    quantiles[i,] = qu
  }

  bind_cols(tibble(col = cols, class, distinct_num, distinct_vals, obs = NROW(df)-na_num, na_num, na_share, min, max, mean, sd), as_tibble(quantiles))
}

get_natural_reg_type = function(v) {
  if (inherits(v, c("Date", "POSIXt"))) {
    return("numeric")
  } else if (is.numeric(v)) {
    uni = unique(v)
    if (length(uni)>0 & length(uni)<=2) {
      if (all(uni %in% c(0,1))) {
        return("dummy")
      }
    }
    return("numeric")
  } else if (is.logical(v)) {
    return("dummy")
  } else {
    return("factor")
  }
}

# Make column statistics as mainly stored later in colstat_numeric,
# colstat_factor and colstat_dummy
# Make column statistics as mainly stored later in colstat_numeric,
# colstat_factor, colstat_dummy, and colstat_datetime
# Make column statistics as mainly stored later in colstat_numeric,
# colstat_factor, colstat_dummy, and colstat_datetime
make_colstats = function(cols,df, reg_df=df, reg_types = NULL) {
  restore.point("make_colstats")

  numeric_li = dummy_li = factor_li = datetime_li = vector("list", length(cols))
  numeric_count = dummy_count = factor_count = datetime_count = 0

  for (col in cols) {
    rt = reg_types[[2]][reg_types[[1]] == col]
    #reg_types[names(reg_types)==col]
    if (length(rt)==0) {
      rt = get_natural_reg_type(df[[col]])
    }

    # Inspect actual object. If Date/Datetime, route to colstat_datetime
    # and strip 'numeric' to prevent numeric quantile operations from crashing.
    is_date = inherits(df[[col]], c("Date", "POSIXt"))
    if (is_date) {
      datetime_count = datetime_count + 1
      datetime_li[[datetime_count]] = colstat_datetime(col, df, reg_df)
      rt = setdiff(rt, "numeric")
    }

    if ("numeric" %in% rt) {
      numeric_count = numeric_count + 1
      numeric_li[[numeric_count]] = colstat_numeric(col, df, reg_df)
    }
    if ("dummy" %in% rt) {
      dummy_count = dummy_count + 1
      dummy_li[[dummy_count]] = colstat_dummy(col, df, reg_df)
    }
    if ("factor" %in% rt) {
      factor_count = factor_count + 1
      factor_li[[factor_count]] = colstat_factor(col, df, reg_df)
    }
  }
  list(
    colstat_numeric = bind_rows(numeric_li[seq_len(numeric_count)]),
    colstat_dummy = bind_rows(dummy_li[seq_len(dummy_count)]),
    colstat_factor = bind_rows(factor_li[seq_len(factor_count)]),
    colstat_datetime = bind_rows(datetime_li[seq_len(datetime_count)])
  )
}



# Make column statistics as mainly stored later in colstat_num and colstat_factor
# Make column statistics as mainly stored later in colstat_num and colstat_factor
colstat_numeric = function(col,df, reg_df=df, val = df[[col]], reg_val = reg_df[[col]]) {
  restore.point("colstat_numeric")

  orig_class = class(val)[1]
  v = reg_val

  if (inherits(v, c("Date", "POSIXt", "difftime"))) v = as.numeric(v)
  if (inherits(val, c("Date", "POSIXt", "difftime"))) val = as.numeric(val)

  qu = quantile(v,c(0.1,0.25,0.75,0.9),na.rm = TRUE)
  tibble(
    col=col,
    class = orig_class,
    na_num = sum(is.na(val)),
    na_share = na_num / length(val),

    mean_org = mean(val, na.rm=TRUE),
    sd_org = sd(val, na.rm=TRUE),

    mean = mean(v, na.rm=TRUE),
    sd = sd(v, na.rm=TRUE),
    min = suppressWarnings(min(v, na.rm=TRUE)),
    max = suppressWarnings(max(v, na.rm=TRUE)),
    median = median(v, na.rm=TRUE),

    q10 = qu[1],
    q25 = qu[2],
    q75 = qu[3],
    q90 = qu[4]
  )
}


colstat_factor = function(col,df, reg_df=df, val = df[[col]], reg_val = reg_df[[col]]) {
  restore.point("colstat_factor")

  val_tab = sort(table(val), decreasing = TRUE)
  reg_val_tab = sort(table(reg_val), decreasing = TRUE)[1:3]
  top = names(reg_val_tab)[1:3]
  tibble(
    col=col,
    class = class(val)[1],
    na_num = sum(is.na(val)),
    na_share = na_num / length(val),

    distinct_num = n_distinct(reg_val),
    distinct_num_org = n_distinct(val, na.rm=TRUE),

    top1_val = top[1],
    top1_num = reg_val_tab[top[1]],
    top1_num_org = val_tab[top[1]],

    top2_val = top[2],
    top2_num = reg_val_tab[top[2]],
    top2_num_org = val_tab[top[2]],

    top3_val = top[3],
    top3_num = reg_val_tab[top[3]],
    top3_num_org = val_tab[top[3]]
  )

}

# Make column statistics as mainly stored later in colstat_num and colstat_factor
colstat_dummy = function(col,df, reg_df=df, val = as_integer(df[[col]]), reg_val = as_integer(reg_df[[col]])) {
  restore.point("colstat_numeric")

  v = reg_val
  tibble(
    col=col,
    class = class(val)[1],
    na_num = sum(is.na(val)),
    na_share = na_num / length(val),

    ones_num = sum(v == 1, na.rm=TRUE),
    ones_num_org = sum(val == 1, na.rm=TRUE),

    mean_org = mean(val, na.rm=TRUE),
    mean = mean(v, na.rm=TRUE),
    sd = sd(v, na.rm=TRUE),
  )
}

# Make column statistics specifically for Date and POSIXt objects
colstat_datetime = function(col, df, reg_df = df, val = df[[col]], reg_val = reg_df[[col]]) {
  restore.point("colstat_datetime")

  out_class = if (inherits(val, "Date")) "date" else "datetime"

  min_val = suppressWarnings(min(reg_val, na.rm = TRUE))
  max_val = suppressWarnings(max(reg_val, na.rm = TRUE))

  na_num = sum(is.na(val))

  tibble(
    col = col,
    class = out_class,
    na_num = na_num,
    na_share = na_num / length(val),
    min = if (is.infinite(min_val)) NA_character_ else as.character(min_val),
    max = if (is.infinite(max_val)) NA_character_ else as.character(max_val),
    distinct_num_org = n_distinct(val, na.rm = TRUE),
    distinct_num = n_distinct(reg_val, na.rm = TRUE)
  )
}
