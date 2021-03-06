#' Make prediction using KNN method.
#'
#' This function makes flexible prediction of traffic flow
#' using K-Nearest Neighbours method.
#'
#' @section KNN method:
#' K-Nearest Neighbours method is a non-parameter method. It can be used
#' to make classifcation and prediction. This function uses KNN method for
#' traffic flow prediction.
#'
#' @section Object flow and Flow Database:
#' This function needs two components: object flow and flow database.
#' The object flow is the flow to be predicted, which's used to refresh
#' the flow as time goes on and make criterion of prediction. The flow
#' database is used to select k nearest neighbours to make prediction of
#' object flow in a given time window.
#'
#' @section Dynamics:
#' Unlike most KNN predicting functions which are static, this function is
#' designed to be dynamic. It means that the process will not stop when a
#' prediction of time window in the future is made. As time goes on, it will
#' use the new real flow data to refresh the object flow, which sometimes is
#' critical to the determination of similarity between flows. In other words,
#' this function is rolling.
#'
#' @section Similarity between flows:
#' How to define similarity between flows is critical in KNN method. The function
#' in this version is designed to use Euclidean distance, which has been testified
#' to be useful in the context of traffic flow similarity defination.
#'
#' @section Flexibility:
#' The flexibility of this function is designed as follows.
#' \enumerate{
#'     \item When to start prediction is arbitrary. Before start point, the
#'           prediction of flow equals to real flow value.
#'     \item The number of nearest neighbour is arbitrary. But it must be
#'           lower than the number of flows in flow database.
#'     \item The time window in the past is arbitrary. It's used to determine
#'           the similarity between two flows. It's advised to be times of 12.
#'     \item The time window in the future is arbitrary. It's used to determine
#'           how long the prediction will be. It's advised to be times of 12.
#' }
#'
#' @seealso \code{\link[stats]{dist}}
#'
#' @param obj The object flow to be predicted, a row from a dataframe.
#' @param base The flow database to select nearest neighbours.
#' @param start Start point to make prediction.
#' @param k The number of nearest neighbours.
#' @param lag_duration The time window to determine the similarity between flows.
#' @param fore_duration The time window to make prediction.
#' @param cat If cat=1, it will print the detail information about neighbours each time.
#' @param save_detail Default is FALSE. If it's set to a string filename with path,
#' selected neighbours of each time will be saved in a .csv file in the given path.
#' @param imbalance Default is FALSE. If it's set to be True, it will use dist_imbalance()
#' to caculate distance.
#' @export
#'
flow_knn <- function(obj,base,start,k,lag_duration,fore_duration,
                     cat=FALSE,save_detail=FALSE,imbalance=FALSE){

  ld = lag_duration
  fd = fore_duration
  st = start

  foreflow = obj
  foreflow[,] <- 0

  detail <- data.frame(matrix(NA,ncol(obj),3))
  names(detail) <- c("start","to","neighbours")

  foreflow[,1:st] = obj[,1:st]

  fl = st

  flowall = rbind(obj,base)

  while(fl<(ncol(obj)-1)){
    obj = as.matrix(obj)
    base = as.matrix(base)

    fwin = fl - ld
    bwin = fl + fd - 1

    if(imbalance == FALSE){
      knnames = names(sort(as.matrix(dist(flowall[,fwin:fl-1]))[,1]))[2:(2+k-1)]
    }else if(imbalance == TRUE){
      obj <- as.data.frame(obj)
      d <- dist_imbalance(obj[,fwin:fl-1],base[,fwin:fl-1])
      knnames = names(d[order(d),][1:k])
    }else{
      stop("imbalance must be logical")
    }

    if (cat==TRUE) {
      cat("predicting window is from",fl,"to",bwin,"and near neighbour are",knnames,"\n")
    }
    if (is.character(save_detail)){
      detail[fl,1] <- fl
      detail[fl,2] <- bwin
      detail[fl,3] <- paste(knnames,collapse = "   ")
    }

    kn = base[knnames,fl:bwin]
    foreflow[,fl:bwin] = colMeans(kn)
    fl = bwin+1
  }
  if (is.character(save_detail)){
    detail <- na.omit(detail)
    write.csv(detail,file=save_detail,row.names = F)
  }
  return(foreflow)
}
