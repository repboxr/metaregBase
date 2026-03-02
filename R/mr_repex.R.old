# Make short reproducable examples
# Main goal: help debugging problems in mr runs

# Put the following functions back to repboxReg

example = function() {
  library(repboxReg)
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  core = load_parcel(project_dir,"s2r","s2r_core")

  parcels = load_parcels(project_dir, c("base","s2r"),just_glob = c("detail","core","regcoef","source","regvar"), ignore_glob = "org_")

  s2r_repex(project_dir, step=51, parcels=parcels)
  navigate_to_repex(project_dir)
}


stata_code_reg_data_preparation = function(project_dir, step, dap=get.project.dap(project_dir, make.if.missing=FALSE), add_reg_code=FALSE) {
  restore.point("stata_code_step_data_preparation")
  astep = step
  code = paste0("// Stata code to replicate analysis in step ", step,"\n\n")
  code = "\n\nset more off\n"
  path = dap$path.df[dap$path.df$astep == astep,]

  if (!astep %in% dap$path.df$astep) {
    stop("The chosen step ", astep, " for the repex is not an analysis step.")
  }

  # 1. Get data file
  step = path$step[1]
  file = mr_get_cache_file(project_dir, step)
  code = paste0(code,'\nuse "', file,'", clear\n')

  # 2. code for all other stepsdata modification steps
  steps = path$step[-1]
  if (!add_reg_code) {
    steps = setdiff(steps, astep)
  }

  if (length(steps)>0) {
    code = paste0(code,"\n", paste0(dap$step.df$stata_code[steps], collapse = "\n"))
  }
  code
}

r_code_reg_data_preparation = function(project_dir, step, dap=get.project.dap(project_dir, make.if.missing=FALSE)) {
  restore.point("r_code_step_data_preparation")
  astep = step
  path = dap$path.df[dap$path.df$astep == astep,]

  step.df = dap$step.df

  # 1. Load cached data file
  step = path$step[1]
  file = mr_get_cache_file(project_dir, step)
  code = paste0(
'\nproject.dir = "', project_dir,'"\n',
'step = ', astep,'\n'
)
  code = paste0(code,'\ndat=haven::read_dta("', file,'")')

  # 2. Translate data modification steps from Stata to R

  msteps = path$step[c(-1, -length(path$step))]

  for (mstep in msteps) {
    row = which(step.df$step == mstep)
    rcode = stata.to.r(sep.lines(step.df$stata_code[row]))
    code = paste0(code,"\n",paste0(rcode, collapse="\n"))
  }

  # 3. Load dap and adapt dat for reg

  code = paste0(code,'

# Adapt data set for regression.
# We use the internal functions that are called by mr_run

dap = get.project.dap(project_dir)
mr = dap
internal_reg = mr_get_reg_info(mr, step,dat)
org_dat = dat
dat = mr_adapt_data_for_reg(project_dir, step, internal_reg, dat)

'
  )

  code
}


filter_parcels_by_step = function(parcels, step) {
  sparcels = lapply(parcels, filter_parcel_by_step, step=step)
  sparcels
}


filter_parcel_by_step = function(parcel, step) {
  restore.point("filter_parcel_by_step")
  .step = step
  sparcel = lapply(parcel, function(df) {
    if (!has.col(df,"step")) return(df)
    filter(df, step %in% .step)
  })
  sparcel
}


filter_tabs_by_step = function(tabs, step) {
  restore.point("filter_tabs_by_step")
  .step = step
  stabs = lapply(tabs, function(df) {
    if (!has.col(df,"step")) return(df)
    filter(df, step %in% .step)
  })
  stabs
}

parcels_to_tables = function(parcels, return_tab_df = FALSE) {
  restore.point("parcels_to_table")
  parcel_name = first(names(parcels))
  tab_df = lapply(names(parcels), function(parcel_name) {
    .parcel = parcels[[parcel_name]]
    tibble(parcel = parcel_name, table = names(.parcel), df = .parcel)
  }) %>% bind_rows()

  agg_df = tab_df %>%
    group_by(table) %>%
    summarize(
      df = list(bind_rows(df))
    )

  tab_li = agg_df$df
  names(tab_li) = agg_df$table
  tab_li
}


load_parcels = function(project_dir, metaid, just_glob=NULL, ignore_glob="*_header.Rds", parcels=list()) {
  restore.point("load_parcels")
  parcel.dirs = file.path(project_dir,"metareg", metaid, "repdb")
  parcel.files = list.files(parcel.dirs, glob2rx("*.Rds"), full.names = TRUE)

  bases = basename(parcel.files)
  keep = rep(TRUE, length(parcel.files))

  if (length(just_glob)>0) {
    rx = sapply(just_glob, glob2rx) %>% str.remove.ends(1,1)
    rx = paste0("(",rx,")", collapse="|")
    keep = keep & grepl(rx,bases)
  }
  for (gl in ignore_glob) {
    rx = glob2rx(gl,trim.head=TRUE) %>% str.remove.ends(1,1)
    keep = keep & !grepl(rx,bases)
  }

  parcel.files = parcel.files[keep]
  names = tools::file_path_sans_ext(basename(parcel.files))
  old_parcels = parcels
  if (length(old_parcels)>0) {
    has_parcel = names %in% names(parcels)
    parcel.files = parcel.files[has_parcel]
    if (length(parcel.files)==0) return(old_parcels)
  }
  parcels = lapply(parcel.files, function(file) {
    readRDS(file)
  })
  names(parcels) = names
  c(old_parcels, parcels)
}

load_parcel = function(project_dir, metaid, parcel) {
  parcel.file=file.path(project_dir,"metareg", metaid,"repdb", paste0(parcel,".Rds"))
  if (!file.exists(parcel.file)) return(NULL)
  readRDS(parcel.file)
}



navigate_to_repex = function(project_dir, step=NULL, repex.dir = file.path(project_dir,"metareg","s2r",paste0("repex/", step))) {
  restore.point("navigate_to_repex")
  rstudioapi::filesPaneNavigate(repex.dir)

}


nice_coef_diff = function(co1, co2,ct=NULL, labs=NULL) {
  diff = coef_diff_table(co1, co2)
  diff = diff %>%
    mutate(
      err = pmax(
        pmin(rel_err_coef, abs_err_coef),
        pmin(rel_err_se, abs_err_se)
      ),
      err_coef = pmin(rel_err_coef, abs_err_coef)
    ) %>%
    select(cterm,err, err_coef, coef_1, coef_2, se_1, se_2) %>%
    arrange(desc(err))
  if (!is.null(labs)) {
    colnames(diff)[4:5] = paste0("coef_",labs)
    colnames(diff)[6:7] = paste0("se_",labs)
  }

  diff
}

show_regcoefs_beside = function(...) {
  co_li = list(...)

}

make_repex_project_code = function(project_dir, repex.dir) {
  restore.point("make_repex_project_code")
  new.project_dir = file.path(dirname(project_dir),"_repex")

  first_lines = paste0(
'project_dir = "', new.project_dir,'"
repex.dir = "', repex.dir,'"')

  main_code = readLines(system.file("code_tpl/make_repex_project.R",package = "repboxReg")) %>% merge.lines()

  paste0(first_lines,"\n", main_code)

}
