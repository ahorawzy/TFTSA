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
#' @param exp The sequence column number of explaining variable.
#' @param resp The sequence column number of response variable.
#' @param sp Span parameter of loess() function.
#' @export

handle_loess <- function(df,exp,resp,sp){
  result <- rep(NA,nrow(df))
  result <- stats::loess(df[,resp]~df[,exp],span=sp)
  return(result$fitted)
}
