expand_stata_abbr_one_val = function(val, abbr.li) {
  for (i in seq_along(abbr.li)) {
    if (val %in% abbr.li[[i]]) return(abbr.li[[i]][1])
  }
  val
}

in_stata_abbr = function(val, abbr.li) {
  all.abbr = unlist(abbr.li, recursive = FALSE, use.names=TRUE)
  val %in% all.abbr
}
