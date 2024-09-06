
`%fin%` <- function(x, table) {
  stopifnot(require(fastmatch))
  fastmatch::fmatch(x, table, nomatch = 0L) > 0L
}
