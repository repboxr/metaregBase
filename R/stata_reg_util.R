example.i = function() {
  dat = data.frame(a=sample(1:2,10,replace=TRUE),x = 1:10)
  dat = dat %>%
    group_by(a) %>%
    mutate(y = x[rev(i(dat))])

  dat
}

make.is.in.sample = function(reg, dat) {
  restore.point("make.is.in.sample")
  .fitted = broom::augment(reg, newdata=dat)$.fitted
  ifelse(is.na(.fitted),0L,1L)
}

keep.cols =function(dat, cols) {
  #restore.point("keep.cols")
  # Update: We now expand Stata column patterns like "x*"
  #         and also account for abbreviations
  ecols = expand.stata.var.patterns(cols, names(dat))
  dat[,intersect(ecols, names(dat))]
}

remove.cols = function(dat, cols) {
  # Updated: We now expand Stata column patterns like "x*"
  #          and also account for abbreviations
  ecols = expand.stata.var.patterns(cols, names(dat))
  dat[,setdiff(names(dat),ecols)]
}

# i() will replace Stata's _n equivalent
# Difficulty: should work with grouped tibbles
# but also inside mutate_rows
i = function(dat=NULL) {
  restore.point("i")
  # for grouped tibbles
  res = try(seq_len(n()), silent = TRUE)
  if (!is(res,"try-error")) return(res)
  return(seq_len(NROW(dat)))
}

N = function(dat=NULL) {
  restore.point("N")
  # for grouped tibbles
  res = try(n(), silent = TRUE)
  if (!is(res,"try-error")) return(res)

  res = try(.N, silent = TRUE)


  # Inside mutate_rows
  if (is(res,"try-error")) {
    res = NROW(dat)
  }
  res
}


rows = function(r) {
  n = n()
  r[r < 1] = NA_integer_
  r[r > n()] = NA_integer_
  r
}
