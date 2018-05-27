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
    cat("预测区间为",fl,"到",bwin,"所选近邻为",knnames,"\n")

    kn = base[knnames,fl:bwin]
    foreflow[,fl:bwin] = colMeans(kn)

    fl = bwin+1
  }
  return(foreflow)
}
