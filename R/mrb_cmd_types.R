stata_cmds_with_margin = function() {
  #cmds = readLines("~/repbox/repboxReg/inst/misc/reg_margin_cmds.txt")
  #cat(paste0('"',sort(cmds),'"', collapse=", "))


  c("asclogit", "asmprobit", "asroprobit", "binreg", "biprobit", "blogit", "bprobit", "clogit",  "etregress", "exlogistic", "glogit", "gprobit", "heckman", "heckoprobit", "heckprobit", "hetprobit", "ivpoisson", "ivprobit", "ivtobit", "logistic", "logit", "mlogit", "mprobit", "nlogit", "obprobit", "ologit", "probit", "rologit", "scobit", "slogit",
    #"tobit","dprobit"
    "xtlogit", "xtprobit")
}

stata_cmds_with_exp_coef = function() {
  c("nbreg", "poisson", "gnbreg","etpoisson", "tnbreg","tpoisson", "zinb", "zip", "expoisson")
}
