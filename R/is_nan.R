#' is.nan
#' Function analog to is.na for Nan values
#'
#' @param x variable for diagnostic
#' @return
#'
#' @export
#'
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
