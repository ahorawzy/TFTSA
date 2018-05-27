handle_loess <- function(df,exp,resp,sp){
  result <- rep(NA,nrow(df))
  result <- loess(df[,resp]~df[,exp],span=sp)
  return(result$fitted)
}
