# Split Stata regression commands into parts that will be stored in cmdpart

# This code adapts much of the code from stata_reg_info.R

cp_init = function(str, org_rows=NULL, str_part = "main") {
  cp = list(
    str=str,
    org_rows = org_rows,
    n = 0,
    start = rep(1, length(str)),
    df = cp_add_empty_df(df = NULL, size=max(length(str)*20))
  )
  if (!is.null(str_part)) {
    cp = cp_add_part_in_df(cp, seq_along(str),str_part,str,tag="", parent="")
  }

  cp
}

cp_add_empty_df = function(df = NULL,min_size=NROW(df), size=max(min_size,NROW(df))) {
  new_df = tibble(
    str_row = rep(NA, size),
    parent = rep("", size),
    part = rep("", size),
    content = rep("", size),
    tag = rep("",size),
    counter = rep(0, size)
    #ph = rep("", size)
  )
  if (is.null(df)) return(new_df)
  bind_rows(df, new_df)
}

cp_add_part_in_df = function(cp, str_rows, part, content, tag=NA, parent="main", counter=rep(0, length(part))) {
  restore.point("cp_add_part_in_df")
  n_add = max(length(str_rows), length(part), length(content))
  if (n_add == 0) return(cp)
  if (cp$n + n_add >= NROW(cp$df)) {
    cp$df = cp_add_empty_df(cp$df, n_add*2)
  }
  inds = (cp$n+1):(cp$n+n_add)

  cp$df$parent[inds] = parent
  cp$df$str_row[inds] = str_rows
  cp$df$part[inds] = part
  cp$df$content[inds] = content
  cp$df$tag[inds] = tag
  cp$df$counter[inds] = counter
  cp$n = cp$n + n_add
  cp
}

# Add extracted parts to cp
cp_add = function(cp, str_rows, start, end, part, tag=NA, use_counter=FALSE,ignore.right.ws=FALSE, parent="main") {
  restore.point("cp_add")
  n_add = length(str_rows)
  if (n_add==0) return(cp)
  if (cp$n + n_add >= NROW(cp$df)) {
    cp$df = cp_add_empty_df(cp$df, n_add*2)
  }

  inds = (cp$n+1):(cp$n+n_add)
  if (use_counter) {
    prev_count_df = cp$df[cp$df$part == part & cp$df$str_row %in% unique(str_rows),] %>%
      group_by(str_row) %>%
      summarize(
        count = n()
      )
    count_df = tibble(str_row = str_rows) %>%
      left_join(prev_count_df, by = "str_row")
    count_df$count[is.na(count_df$count)] = 0
    count_df$count = count_df$count+1


    counter = count_df$count[match(str_rows, count_df$str_row)]
    cp$df$counter[inds] = counter

    ph = paste0("{{", part, counter,"}}")
  } else {
    counter = rep(0, length(str_rows))
    ph = paste0("{{", part,"}}")
  }

  sstr = substring(cp$str[str_rows], start, end)
  if (ignore.right.ws) {
    len_ws = nchar(sstr) - nchar(trimws(sstr,"right"))
    end = end - len_ws
    sstr = substring(cp$str[str_rows], start, end)
  }


  content = sstr
  cp$df$parent[inds] = parent
  cp$df$str_row[inds] = str_rows
  cp$df$part[inds] = part
  cp$df$content[inds] = content
  cp$df$tag[inds] = tag
  cp$df$counter[inds] = counter
  #cp$df$ph[inds] = ph

  # Use stringi:stri_sub_replace since substring <- requires replacement
  # to be of same than original string
  cp$str[str_rows] = stringi::stri_sub_replace(cp$str[str_rows], from=start, to=end, replacement=ph)

  cp$added_inds = inds
  cp$start[str_rows] = cp$start[str_rows]+nchar(ph)
  cp$n = cp$n + n_add

  cp
}

cp_add_starts_with = function(cp, patterns, part, tag, use_counter = FALSE, ignore.right.ws = TRUE) {
  restore.point("set_cmdpart_starts_with")
  cp$did_change = FALSE
  unused = rep(TRUE, length(cp$str))
  start = cp$start
  sstr = substring(cp$str, cp$start)

  s = patterns[1]
  for (s in patterns) {
    str_rows = which(startsWith(sstr, s) & unused)
    if (length(str_rows)>0) {
      if (ignore.right.ws) {
        s = trimws(s, "right")
      }
      len = nchar(s)

      end = cp$start[str_rows]+len-1
      cp = cp_add(cp, str_rows = str_rows, start=start[str_rows], end=end, part=part, tag=tag, use_counter=use_counter)
      unused[str_rows] = FALSE
      cp$did_change = TRUE
    }
  }
  cp
}



find_ws_around = function(txt) {
  txt2 = trimws(txt,"left")
  ws_left = nchar(txt) - nchar(txt2)
  txt3 = trimws(txt2,"right")
  ws_right = nchar(txt2) - nchar(txt3)
  list(ws_left = ws_left, ws_right=ws_right, txt = txt3)
}

# TO DO: Allow to ignore white spaces on the left, but keep them in str
cp_add_left_of = function(cp, left_of, part, tag, use_counter = FALSE, include_split=FALSE, ignore.right.ws = TRUE, fixed=TRUE) {
  restore.point("cp_add_left_of")
  start = cp$start
  sstr = substring(cp$str, cp$start)

  left = left_of(sstr, left_of, fixed=fixed, not.found = rep(NA, length(sstr)))
  str_rows = which(!is.na(left))

  if (length(str_rows)==0) {
    cp$did_change = FALSE
    return(cp)
  }
  start = start[str_rows]
  left = left[str_rows]

  if (ignore.right.ws) {
    left = trimws(left, "right")
  }

  if (include_split) {
    left = paste0(left, left_of)
  }

  len = nchar(left)
  end = start+len-1
  cp = cp_add(cp, str_rows = str_rows, start=start, end=end, part=part, tag=tag, use_counter=use_counter)
  cp$did_change = TRUE

  cp
}

cp_jump_ws = function(cp) {
  sstr = substring(cp$str, cp$start)

  left_ws = nchar(sstr) - nchar(trimws(sstr,"left"))
  cp$start = cp$start + left_ws
  cp
}

example = function() {
  cmdlines = c(
    "ivregress 2sls y1 x1 (y2 y3 = z1 z2), vce(robust)",
    "capt: xi: regress y i.i1##c.d1 [aw=x] if a == 5 in 3, vce(hc2)",
    #"capture quietly xi: regress y i.i1##c.d1, vce(hc2)",
    "capt: regress y i.i1##c.d1 if a==5 [aw=z], vce (robust)  opt2(arg2 = fun( funarg )) noarg"
  )
  options(warn=2)
  df = cmdparts_of_stata_reg(cmdlines)
}




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


