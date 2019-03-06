#' Caculate LOESS value from a dataframe.
#'
#' This function caculates LOESS (locally weighted regression) by given a dataframe.
#'
#' LOESS (locally weighted regression) is a technique which can smooth a series. It
#' needs a explaining variable as exp, a response variable as resp, a parameter span.
#' By using loess() function in stats package, this function can caculate LOESS value
#' for a series from a dataframe.
#'
#' @seealso \code{\link[stats]{loess}}
#'
#' @param df The dataFrame contains series to be loess.
#' @param sp Span parameter of loess() function.
#' @export
#'
handle_loess_fordf <- function(df,sp){
  ndays <- nrow(df)
  timestamps <- 1:ncol(df)
  for(i in 1:ndays){
    loess_result <- stats::loess(df[i,]~timestamps,span=sp)
    df[i,] <- loess_result$fitted
  }
  return(df)
}
