# Store information about expanded interaction terms and factors
# This will allow to rebuild regressions in a way that drops exactly
# the same factor levels than the original regression run


example = function() {
  library(repboxReg)
  project_dir = "/home/rstudio/repbox/projects_reg/aer_105_5_55"
  step = 5

  dat=haven::read_dta("/home/rstudio/repbox/projects_reg/aer_105_5_55/metareg/dap/stata/cache/step_1.dta")

  dap = get.project.dap(project_dir)
  mr = dap
  internal_reg = mr_get_reg_info(mr, step,dat)
  org_dat = dat
  dat = mr_adapt_data_for_reg(project_dir, step, internal_reg, dat)

  regvar = load_parcel(project_dir, "base","regvar")$regvar %>% filter(step==5)
  regcoef = load_parcel(project_dir, "base","regcoef")$regcoef %>% filter(step==5)

  regxvar = make_regxvar(regvar, dat, regcoef)
  regxvar
}


make_regxvar = function(regvar,dat,  regcoef=NULL) {
  restore.point("make_regxvar")
  regvar = regvar[regvar$role %in% c("exo","endo","instr") & !regvar$absorbed_fe,]

  rows = regvar$var_reg_type == "factor"
  factor_vars = unique(regvar$cterm[rows])

  factor_levels = lapply(factor_vars, function(var) {
    unique(dat[[var]]) %>% as.character()
  })
  names(factor_levels) = factor_vars

  ia_cterms = unique(regvar$ia_cterm)


  res_li = lapply(ia_cterms, function(ia_term) {
    rows = which(regvar$ia_cterm == ia_term)
    if (length(rows)==1) {
      res = make_regxvar_ia1(regvar[rows,],factor_levels)
    } else if (length(rows)==2) {
      res = make_regxvar_ia2(regvar[rows,],factor_levels)
    } else if (length(rows)==3) {
      res = make_regxvar_ia3(regvar[rows,],factor_levels)
    } else {
      stop(paste0("We can currently deal with at most tripple interaction terms, but the regression uses ", length(rows),"-fold interaction terms. Who specifies such regressions?"))
    }
    res
  })

  regcoef = filter(regcoef, !is.na(coef))
  # For some commands like mlogit we have duplicated entries
  regcoef = regcoef[!duplicated(regcoef[,c("step","cterm")]), ]

  regxvar = tibble(artid=first(regvar$artid), step=first(regvar$step), ia_cterm=ia_cterms, cterm = res_li) %>%
    unnest(cterm) %>%
    left_join(regvar %>% select(ia_cterm, role), by="ia_cterm") %>%
    unique() %>%
    left_join(select(regcoef,step, cterm, org_coef=coef), by=c("cterm","step")) %>%
    mutate(in_regcoef = !is.na(org_coef))

  regxvar

}

make_regxvar_ia1 = function(regvar,level_li) {
  restore.point("make_regxvar_ia1")
  if (regvar$var_reg_type != "factor") return(regvar$cterm)

  levels = level_li[[regvar$cterm]]
  cterms = paste0(regvar$cterm, "=", levels)
  cterms
}


make_regxvar_ia2 = function(rv, level_li) {
  restore.point("make_regxvar_ia2")

  vars1 = make_regxvar_ia1(rv[1,], level_li)
  vars2 = make_regxvar_ia1(rv[2,], level_li)

  grid = expand.grid(var1=vars1, var2=vars2,stringsAsFactors = FALSE) %>%
    mutate(var12 = paste0(var1,"#", var2))

  unique(c(vars1,vars2, grid$var12))
}

make_regxvar_ia3 = function(rv, level_li) {
  restore.point("make_regxvar_ia3")

  vars12 = make_regxvar_ia2(rv[1:2,], level_li)
  #vars13 = make_regxvar_ia2(rv[c(1,3),], level_li)
  #vars23 = make_regxvar_ia2(rv[2:3,], level_li)
  vars3 = make_regxvar_ia1(rv[3,], level_li)

  grid = expand.grid(var12=vars12, var3=vars3,stringsAsFactors = FALSE) %>%
    mutate(var123 = paste0(var12,"#", var3))

  unique(c(vars12,vars3, grid$var123))
}

# Add the expanded columns specified in regxvar to dat
# if a column already exists, we won't overwrite it.
make_regxvar_cols = function(dat, regxvar) {
  restore.point("make_regxvar_cols")
  # Don't overwrite existing columns
  # This also should guarantee that every element of all_cterm
  # is either a factor or an interaction term
  all_cterms = setdiff(regxvar$cterm, names(dat))

  num_ia = stringi::stri_count_fixed(all_cterms,"#")+1

  # Cols without interaction effect
  cterms = all_cterms[num_ia==1]
  vars = str.left.of(cterms, "=")
  vals = str.right.of(cterms, "=")
  for (i in seq_along(cterms)) {
    # E.g. cols with o@ prefix can be missings
    if (!has.col(dat,vars[i])) next
    col_val = as.vector(dat[[ vars[i] ]])
    dat[[ cterms[i] ]] = 1L*(col_val == as(vals[i], atomic_class(col_val)))
  }

  # Cols with pair interaction effect
  # We assume that the single terms are in dat (e.g. from previous loop)
  cterms = all_cterms[num_ia==2]
  if (length(cterms)>0) {
    vars1 = str.left.of(cterms, "#")
    vars2 = str.right.of(cterms, "#")
    for (i in seq_along(cterms)) {
      dat[[ cterms[i] ]] = dat[[vars1[i] ]]*dat[[vars2[i]]]
    }
  }

  # Cols with tripple interaction effect
  cterms = all_cterms[num_ia==3]
  if (length(cterms)>0) {
    vars1 = str.left.of(cterms, "#")
    str = str.right.of(cterms, "#")

    vars2 = str.left.of(str, "#")
    vars3 = str.right.of(str, "#")
    for (i in seq_along(cterms)) {
      dat[[ cterms[i] ]] = dat[[vars1[i]]]*dat[[vars2[i]]]*dat[[vars3[i]]]
    }
  }
  dat
}

