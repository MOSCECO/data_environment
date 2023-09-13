mini <- function(x, na.rm = FALSE) {
  y <- x[is.finite(x) & length(x) > 0]
  out <- max(y, na.rm = na.rm)
  if(is.infinite(out)) { return(NA) } else { return(out) }
}
