dist_euclidean <- function(df){
  len <- nrow(df)
  result <- matrix(NA,nrow = len,ncol = len)
  for (j in 1:(len-1)) {
    for (i in (j+1):len) {
      result[i,j] <- sqrt(sum((df[i,]-df[j,])**2))
    }
  }
  rownames(result) <- rownames(df)
  colnames(result) <- rownames(df)
  result <- as.dist(result)
  return(result)
}
