#' Caculate enhanced Euclidean distance
#'
#' This function caculates enhanced Euclidean distance for a given matrix.
#'
#' @param series_matrix The matrix contains several rows to be caculated distance.
#' @param r1 The weight parameter of the absolute distance between two series.
#' @param r2 The weight parameter of the stability of the difference of two series. r1 add r2 must be 1.
#' @return The distance matrix of every pair of rows in series_matrix. The class is \code{\link[stats]{dist}}.
#' @examples
#' enhanced_euclidean(matrix(runif(100,0,100),20,50))
#'
#' @export

enhanced_euclidean <- function(series_matrix, r1, r2){
  if(r1+r2 != 1){
    stop("r1 add r2 must equal 1.")
  }
  num_row <- nrow(series_matrix)
  result_matrix <- matrix(rep(0, num_row ** 2), num_row, num_row)
  for(i in 1:num_row){
    for(j in 1:num_row){
      if(i > j){
        z <- series_matrix[i,] - series_matrix[j,]
        result_matrix[i,j] <- r1 * sqrt(sum(z**2)) + r2 * sqrt(sum((z-mean(as.matrix(z)))**2))
      }
    }
  }
  colnames(result_matrix) <- rownames(series_matrix)
  rownames(result_matrix) <- rownames(series_matrix)
  return(as.dist(result_matrix))
}
