# Some helper functions to compute regression results for different standard errors

tidy_sandwich = function(reg, se_type, ..., use.summary=FALSE, vcov.fun = NULL) {
  restore.point("tidy_sandwich")
  if (use.summary) {
    reg =summary(reg)
  }
  if (se_type == "default") {
    coef.df = broom::tidy(reg, conf.int=TRUE)
    return(bind_cols(se_type=se_type, coef.df))
  }
  if (!is.null(vcov.fun)) {
    vcov = vcoc.fun(reg)
  } else if (startsWith(se_type,"HC")) {
    vcov = vcovHC(reg, type=se_type)
  } else if (startsWith(se_type, "HAC")) {
    vcov = vcovHAC(reg, ...)
  }
  library(lmtest)
  coef.df = broom::tidy(coeftest(reg, vcov. = vcov), conf.int=TRUE)
  bind_cols(se_type=se_type, coef.df)
}


