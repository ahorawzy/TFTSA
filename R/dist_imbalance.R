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
