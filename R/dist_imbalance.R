#бо Imbalance distance caculation
#'
#' This functions help function knn_flow() use imbalance distance in caculating similarity
#' among traffic flows.
#'
#' Imbalance distance means that values of flow in flow base is treated differently, which
#' depends on whether it's higher than object flow or otherwise. Because the punishment of
#' forecast should be different whether the forecast tends to be higher or smaller. It's more
#' tolerable that make higher forecast than lower.
#'
#' @param obj The object flow (fragment) to be forecasted.
#' @param base The flow base (fragment) used by KNN method.
#' @return The distance between object flow and every flow in flow base as a dataframe with
#' the same order of flow in flow base.
#' @export

dist_imbalance <- function(obj,base){
  n <- nrow(base)
  s <- matrix(NA,n,1)
  for (i in 1:nrow(base)) {
    if(sum(base[i,(obj[1,]>base[i,])])>0){
      s[i,1] <- sqrt(sum((base[i,(obj[1,]>base[i,])]-obj[1,(obj[1,]>base[i,])])**2))
    } else{
      s[i,1] = 0
    }
  }
  rownames(s) <- rownames(base)
  return(s)
}
