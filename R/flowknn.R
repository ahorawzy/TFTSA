#' @export
flowknn <- function(obj,base,start,k,lag_duration,fore_duration){

  ld = lag_duration
  fd = fore_duration
  st = start

  foreflow = obj
  foreflow[,] <- 0

  foreflow[,1:st] = obj[,1:st]

  fl = st

  obj = as.matrix(obj)
  base = as.matrix(base)

  flowall = rbind(obj,base)

  while(fl<(ncol(obj)-1)){

    fwin = fl - ld
    bwin = fl + fd - 1

    knnames = names(sort(as.matrix(dist(flowall[,fwin:fl-1]))[,1]))[2:(2+k-1)]
    cat("predicting window is from",fl,"to",bwin,"and near neighbour are",knnames,"\n")

    kn = base[knnames,fl:bwin]
    foreflow[,fl:bwin] = colMeans(kn)

    fl = bwin+1
  }
  return(foreflow)
}
