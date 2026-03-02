# Static code analysis of regression commands
# Note that the static code can still contain Stata macros
# and thus cannot be as reliably analysed as the command line
# we get from our dynamic analysis in which macros have been
# replaced.

example = function() {
  project_dir = "~/repbox/projects_static/aejapp_10_1_1"
  parcels = repboxDB::repdb_load_parcels(project_dir, "reg_stata_cmd")
  reg_cmd = parcels$reg_stata_cmd$reg_stata_cmd
  reg_cmd$sregnum = 1:NROW(reg_cmd)
  reg_df = static_reg_cmd_to_reg_df(reg_cmd)
}

static_reg_cmd_to_reg_df = function(reg_cmd) {
  restore.point("static_reg_cmd_to_reg_df")
  if (NROW(reg_cmd)==0) return(NULL)
  reg_cmd$sregnum = seq_len(NROW(reg_cmd))
  cmdpart = cmdparts_of_stata_reg(reg_cmd$cmdline)
  cmdpart$artid = reg_cmd$artid[cmdpart$str_row]
  cmdpart$step  = cmdpart$sregnum = cmdpart$str_row
  opts_df = cmdpart_to_opts_df(cmdpart)

  unique(cmdpart$part)
  unique(cmdpart$tag)

  cols = union(c("artid", "sregnum"), setdiff(colnames(reg_cmd), c("num_runs","num_err_runs","is_reg")))
  reg_cmd = reg_cmd[,cols]


  i = 1
  reg_df = bind_rows(lapply(seq_len(NROW(reg_cmd)), function(i) {
    #restore.point("hhsfkhf")
    reg=reg_cmd[i,]
    opts = opts_df[opts_df$step==i,]
    reg$opts_have_macro = isTRUE(any( has.substr(opts$opt_arg,"$") | has.substr(opts$opt_arg,"`")))
    se_info = try(se_stata_to_repdb(reg_cmd$cmd[i], opts),silent = TRUE)

    if (is(se_info,"try-error")) {
      cat(paste0("\nCould not parse se info:\n   ", reg$cmdline,"\n"))
      reg[c("se_category","se_type","se_args")] = rep(NA_character_,3)
    } else {
      reg[c("se_category","se_type","se_args")] = se_info[c("se_category","se_type","se_args")]
    }
    reg
  }))

  cmd_sum = cmdpart %>%
    group_by(artid, sregnum) %>%
    summarize(
      has_weights = "weight_str" %in% part,
      has_absorb = "absorb" %in% tag,
      specifies_se = "se" %in% tag,
      has_if = "if_str" %in% part,
      has_in = "in_str" %in% part,
      has_opt = "opt_str" %in% part
    )

  reg_df = reg_df %>% left_join_overwrite(cmd_sum, by = c("artid","sregnum"))
  reg_df$uses_macro = has.substr(reg_df$cmdline,"`") & has.substr(reg_df$cmdline,"$")

  reg_df

}
