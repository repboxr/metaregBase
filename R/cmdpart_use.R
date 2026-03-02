# Tools for working with command part representations
# or wrappers that use them under the hood.

cmdpart_parse_stata_opt_str = function(str) {
  restore.point("cmdpart_of_stata_opt_str")

  # str = c(
  #   "vce (robust)  opt2(arg2 = fun( funarg )) noarg",
  #   "absorb(x) robust"
  # )


  # We have examples like absorb(var)r
  # we need to transform it to absorb(var) r
  str = stri_replace_all_regex(str,"\\)(?=[a-zA-Z])",") ")


  all_opt_str = all_opt = all_opt_arg = vector("list",NROW(str))


  i = 221
  for (i in seq_along(str)) {
    s = str[i]
    if (trimws(s)=="") next

    # 1. Find 1st level braces and replace with ph
    brace_pos = locate_1st_level_braces(s,open="(",close=")")

    if (NROW(brace_pos)==0) {
      s = shorten.spaces(s) %>% trimws()
      all_opt[[i]] = strsplit(s, " ", fixed=TRUE)[[1]]
      all_opt_str[[i]] = paste0("{{opt", seq_along(all_opt[[i]]),"}}")
      all_opt_arg[[i]] = rep(NA, length(all_opt[[i]]))
      next
    }

    # 1. Replace 1st level braces with placeholders
    brace_content = substring(s, brace_pos[,1], brace_pos[,2])
    ph = paste0("#~(", seq_len(NROW(brace_pos)),"~#")
    s = str.replace.at.pos(s, brace_pos, ph)

    # 2. Remove all ws before "#~()~#" and shorten ws
    s = gsub("[ \t]+#~\\(","#~(", s)
    s = shorten.spaces(s) %>% trimws()

    # 3. Split by " "
    opt_str = strsplit(s," ", fixed=TRUE)[[1]]

    # 4. locate argument placeholders
    arg_start_pos = stringi::stri_locate_first_fixed(opt_str, "#~(")[,1]
    has_arg = !is.na(arg_start_pos)

    opt_arg = rep(NA, length(opt_str))
    opt_arg[has_arg] = str.remove.ends(brace_content,1,1)

    opt = ifelse(has_arg,substring(opt_str,1, arg_start_pos-1), opt_str)

    all_opt_str[[i]] = opt_str
    all_opt[[i]] = opt
    all_opt_arg[[i]] = opt_arg

    all_opt_str[[i]] = ifelse(has_arg,
      paste0("{{opt", seq_along(opt),"}}({{opt_arg",seq_along(opt),"}})"),
      paste0("{{opt", seq_along(opt),"}}")
    )

  }
  list(
    opt_str = all_opt_str,
    opt = all_opt,
    opt_arg = all_opt_arg
  )
}

cmdpart_get_placeholders = function(cp_df, prefix="{{", postfix="}}") {
  ifelse(is.true(cp_df$counter > 0),
    paste0(prefix, cp_df$part, cp_df$counter, postfix),
    paste0(prefix, cp_df$part, postfix)
  )
}

cmdpart_create_cmdline = function(cp_df) {

  main_row = which(cp_df$part == "main")
  str = cp_df$content[main_row]
  ph = cmdpart_get_placeholders(cp_df)

  # while(TRUE) {
  #   str_n = stri_replace_all_fixed(str, ph, cp_df$content, vectorize_all = FALSE)
  #   if (str_n == str) break
  #   str = str_n
  # }
  stri_replace_all_fixed(str, ph, cp_df$content, vectorize_all = FALSE)

}

cmdpart_find_parent_rows = function(df, rows=seq_along(NROW(df)), remove.na=FALSE) {
  if ("str_row" %in% colnames(df)) {
    part.key = paste0(df$str_row,"|", df$part ,"|",df$counter)
    parent.key = paste0(df$str_row,"|", df$parent,"|",df$counter)
  } else if ("artid" %in% colnames(df)) {
    part.key = paste0(df$artid,"|", df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$artid,"|", df$step, "|", df$parent,"|",df$counter)
  } else {
    part.key = paste0(df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$step, "|", df$parent,"|",df$counter)
  }
  match(parent.key[rows], part.key)
}

cmdpart_find_child_rows = function(df, rows=seq_along(NROW(df)), child_part) {
  if ("str_row" %in% colnames(df)) {
    part.key = paste0(df$str_row,"|", df$part,"|",df$counter)
    parent.key = paste0(df$str_row,"|", df$parent,"|",df$counter)
  } else if ("artid" %in% colnames(df)) {
    part.key = paste0(df$artid,"|", df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$artid,"|", df$step, "|", df$parent,"|",df$counter)
  } else {
    part.key = paste0(df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$step, "|", df$parent,"|",df$counter)
  }
  parent.key[df$part != child_part] = NA

  match(part.key[rows], parent.key)

}


example = function() {
  cmdpart = readRDS("C:/libraries/repbox/projects_reg/testsupp/repdb/cmdpart.Rds")$cmdpart
  cmdpart_to_opts_df(cmdpart)
}

cmdpart_to_opts_df = function(cmdpart) {
  restore.point("cmdpart_to_opts_df")
  opt_df = filter(cmdpart, part=="opt") %>% select(artid,step, counter, opt=content, tag=tag)
  opt_arg = filter(cmdpart, part=="opt_arg") %>% select(artid, step, counter, opt_arg=content)
  left_join(opt_df, opt_arg, by=c("artid","step","counter"))
}



locate_1st_level_braces = function(txt, open="(", close = ")") {
  #restore.point("locate_1st_level_braces")
  if (length(txt)==0) return(NULL)
  res = try(str.blocks.pos(txt, open, close), silent=TRUE)
  if (is(res,"try-error")) {
    cat("\nNon-matching braces in reg options:\n  ", txt)
    return(NULL)
  }
  res$outer[res$levels==1,,drop=FALSE]
}


