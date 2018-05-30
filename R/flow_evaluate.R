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
  mse <- 1/n*sum((rv-pv)**2)
  rmse <- sqrt(1/n*sum((rv-pv)**2))
  mae <- 1/n*sum(abs(rv-pv))

  if(method == "all"){
    result <- data.frame(mse,rmse,mae)
  } else if(method == "mse"){
    result <- mse
  } else if(method == "rmse"){
    result <- rmse
  } else if(method == "mae"){
    result <- mae
  }
  return(result)
}
