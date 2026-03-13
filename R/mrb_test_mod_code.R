
# For a given runid=pid
# create a text that shows the Stata data modification steps
# as well as the translated R data modification steps
# if add_filer_code also add the R code used to implement the filter
# we use the same translation as used to generate the R code repboxDRF

#' @param direct_stata_path if TRUE generated the Stata code for a direct Stata path
#' if FALSE replicate the code as for mrb_full_stata_script, at least up to the relevant regression. Later code can be removed.
#' @param line_numbers If TRUE append line numbers to
mrb_test_mod_code = function(project_dir, runid, parcels=list(),add_state=TRUE, add_r = TRUE,add_filter_code = TRUE, direct_stata_path=FALSE, line_numbers=TRUE, drf=drf_load(project_dir, parcels=parcels)) {



  return(txt)

}
