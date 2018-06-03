#' Evaluate the forecast result.
#'
#' This function evaluates the forecast result in several evaluation criterions.
#'
#' @param rv real value of traffic flow, a row from a dataframe;
#' @param pv forecast value, can be the return value of flow_knn function,
#'           a row from a dataframe;
#' @param method can be "mse", "rmse", "mae" or "all"
#' @export
flow_evaluate <- function(rv,pv,method="all"){
  n <- length(rv)
  z <- rv-pv
  mse <- 1/n*sum(z^2)
  rmse <- sqrt(1/n*sum(z^2))
  mae <- 1/n*sum(abs(z))
  imse <- 1/n*sum(ifelse(z>0,1.5,0.5)*z^2)

  if(method == "all"){
    result <- data.frame(mse,rmse,mae,imse)
  } else if(method == "mse"){
    result <- mse
  } else if(method == "rmse"){
    result <- rmse
  } else if(method == "mae"){
    result <- mae
  }
  return(result)
}
