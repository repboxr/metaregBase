# Split Stata commands into parts that have a special cmdpart structure

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

