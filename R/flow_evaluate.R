#' Evaluate the forecast result.
#'
#' This function evaluates the forecast result in several evaluation criterions.
#'
#' @param rv real value of traffic flow, a row from a dataframe;
#' @param pv forecast value, can be the return value of flow_knn function,
#'           a row from a dataframe;
#' @param method can be "mse", "rmse" or "mae"
#' @export
flow_evaluate <- function(rv,pv,method="mse"){
  n <- length(rv)
  if(method=="mse"){
    result <- 1/n*sum((rv-pv)**2)
  } else if(method=="rmse"){
    result <- sqrt(1/n*sum((rv-pv)**2))
  } else if(method=="mae"){
    result <- 1/n*sum(abs(rv-pv))
  }
  return(result)
}
