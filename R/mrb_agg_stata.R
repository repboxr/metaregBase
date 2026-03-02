
# TO DO: Transform to parcels
mrb_agg_stata_reg_out = function(mrb) {
  restore.point("mrb_agg_stata_reg_out")
  mrb$stata_ct = stata_ct = mrb_agg_stata_regcoef(mrb)
  mrb$stata_ct = mrb_agg_add_dprobit_coef(mrb,mrb$stata_ct)
  mrb$stata_reg_scalars = mrb_agg_stata_reg_scalars(mrb)
  mrb$stata_reg_macros = mrb_agg_stata_reg_macros(mrb)

  unique(mrb$stata_ct$runid)
}

mrb_agg_stata_regcoef = function(mrb, file_prefix="reg_", dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mrb_agg_stata_regcoef")
  glob = paste0(file_prefix, "*",".dta")
  run_df = mrb$drf$run_df
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(mrb)

  file = files[1]
  old.cols = c("parm","label","estimate","stderr","dof", "z","p","min95","max95")
  new.cols = c("var","label", "coef","se","dof", "t","p","ci_low","ci_up")

  li = lapply(files, function(file) {
    restore.point("kahkdhskdhk")
    df = haven::read_dta(file)
    df = rename.cols(df, old.cols, new.cols)
    df = df[,intersect(new.cols, colnames(df))]
    base = basename(file)
    if (!is.null(df)) {
      has.variant = has.substr(base,"__")
      if (has.variant) {
        runid = as_integer(str.between(base, file_prefix, "__"))
        variant = str.between(base, "__", ".dta")
      } else {
        runid = as_integer(str.between(base, file_prefix, ".dta"))
        variant = ""
      }
      df$runid = rep(runid, NROW(runid))
      df$variant = rep(variant, NROW(runid))
      df$cmd = run_df$cmd[runid]
    }
    return(df)
  })
  res = bind_rows(li)
  res
}


# Aggregates regression statistics stored with svret
mrb_agg_stata_reg_scalars = function(mrb, file_prefix="regscalar_", dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mr_agg_stata_reg_scalars")
  glob = paste0(file_prefix, "*",".txt")
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  file = first(files)
  li = lapply(files, function(file) {

    df = read_var_equal_val_file(file,as.numeric = TRUE)
    base = basename(file)
    if (!is.null(df)) {
      runid = as_integer(str.between(base, file_prefix, "__"))
      variant = str.between(base, "__", ".txt")
      df$runid = rep(runid, NROW(runid))
      df$variant = rep(variant, NROW(runid))
    }
    return(df)
  })
  res = bind_rows(li)
  res
}

# Aggregates regression statistics stored with svret
mrb_agg_stata_reg_macros = function(mrb, file_prefix="regmacro_", dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mr_agg_stata_reg_macros")
  glob = paste0(file_prefix, "*",".txt")
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  file = first(files)
  li = lapply(files, function(file) {
    df = read_var_equal_val_file(file,as.numeric = FALSE)
    base = basename(file)
    if (!is.null(df)) {
      runid = as_integer(str.between(base, file_prefix, "__"))
      variant = str.between(base, "__", ".txt")
      df$runid = rep(runid, NROW(runid))
      df$variant = rep(variant, NROW(runid))

    }
    return(df)
  })
  res = bind_rows(li)
  res
}



# Extract marginal effects for dprobit commands
# Unlike margins, dprobit (and mfx) treats dummy variables automatically as
# discrete, i.e. we have special code.
mrb_agg_add_dprobit_coef = function(mrb, stata_ct, dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mr_agg_add_dprobit_coef")
  glob = paste0("dprobit_", "*",".csv")
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(stata_ct)

  #runid.df = mr$runid.df
  #new.cols = c("var","label", "coef","se","dof", "t","p","ci_low","ci_up")

  df = lapply(files, function(file) {
    df = read.csv(file)
    base = basename(file)
    if (!is.null(df)) {
      runid = as_integer(str.between(base, "dprobit_", ".csv"))
      df$runid = rep(runid, NROW(runid))
    }
    return(df)
  }) %>% bind_rows()
  df$t = df$coef / df$se
  df$ci_low = df$ci_up = NA_real_
  df$variant = rep("sb_mfx", NROW(runid))
  df$cmd = "dprobit"

  # p-value is the same as for the original coefficient
  df = left_join(df, stata_ct %>% filter(variant=="sb") %>% select(runid, var,p,label), by=c("runid","var"))



  return(bind_rows(stata_ct,df))
}


read_var_equal_val_file = function(file, as.numeric=FALSE, wide = FALSE) {
  restore.point("read_var_equal_file")
  txt = readLines(file)
  pos = stringi::stri_locate_first_fixed(txt,"=")[,1]

  var = stringi::stri_sub(txt,1,pos-1)
  val = stringi::stri_sub(txt,pos+1)
  if (as.numeric) {
    val = suppressWarnings(as.numeric(val))
  }

  if (wide) {
    li = as.list(val)
    names(li) = var
    res = as_tibble(li)
  } else {
    res = tibble(
      var = var,
      val = val
    )
  }

  res
}
