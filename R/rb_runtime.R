example = function() {
  project_dir = "~/repbox/projects/aejapp_5_3_6"
}

rb_get_steps_runtimes = function(project_dir) {
  step_files = list.files(file.path(project_dir,"steps"), glob2rx("*.Rds"), full.names = TRUE)
  bases = basename(step_files)

  artid = basename(project_dir)
  is_start = has.substr(bases, ".start.")
  type = str.left.of(bases,".")
  time = file.mtime(step_files)

  start_df = data.frame(type = type[is_start], time_start= time[is_start])
  end_df = data.frame(type = type[!is_start], time_end=time[!is_start])

  df = full_join(start_df, end_df, by="type")
  df$sec = as.numeric(df$time_end)-as.numeric(df$time_start)
  df$minutes = df$sec/60


  df = bind_cols(data.frame(artid = artid), df)


  df
}
