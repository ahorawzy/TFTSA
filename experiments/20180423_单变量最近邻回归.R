#  单变量的回归
#  NNBR  knn近邻回归与预测


#  这里是基于单变量的时间序列
setwd('D:/MyDriversMatlab/Mfiles13')
rm(list=ls())
nnbrRegress<-function (xData , lags , disType)
{
  #  lags是滞后的阶数P
  #  outNum是样本外预测的个数
  #  disType是计算距离的类别
  if (!is.matrix(xData))
  {
    stop('error input the Current Data')
  }
  
  
  if (ncol(xData)!= 1)
  {
    stop('error input data')
  }
  
  if ((nrow(xData) - lags)<1)
  {
    stop('error input the Data')
  }
  
  #  当前的数据集合
  currentData <- xData
  
  k <- ceiling(sqrt(nrow(xData) - lags))
  
  #  先计算所有的特征向量
  currentVector <- xData[c(( nrow(xData)-lags+1  ):(nrow(xData)))]
  currentVector <- t(as.matrix(currentVector))
  
  Vectors<-NULL
  xOut<-NULL
  #  获取其他的特征向量
  i<-1
  while(TRUE)
  {
    if ((i+lags - 1) == (nrow(xData)))
    {
      break
    }
    vectors<- t(as.matrix(xData[(i):(i+lags - 1)]))
    
    xOut<-c(xOut,xData[i+lags])
    
    Vectors<-rbind(Vectors,vectors)
    
    i<-i+1
  }
  
  #  再从中寻找距离最小的K个值
  nV <- nrow(Vectors)
  
  currData <- matrix(rep(currentVector , nV),nrow = nV,byrow = TRUE)
  Distance <- sqrt( apply((Vectors - currData)^2,1,sum) )
  #  再计算距离的最小的前k个值
  #  从大到小的几个下标
  Index <- order(Distance)
  Index <- Index[c(1:k)]
  
  xOut<-xOut[Index]
  DistanceOut <- Distance[Index]
  
  #  对样本外的结果进行预测
  #  权重的配置
  if (disType == 1)
  {
    xForecasting <- sum(DistanceOut/sum(DistanceOut)*xOut)
  } else
  {
    xForecasting <- sum((c(k:1)/k)/sum((c(k:1)/k))*xOut)
  }
  return(xForecasting)
  
}
data <- as.matrix(cumsum(matrix(rnorm(100),nrow = 100)))
data
nnbrRegress(data,8,1)
nnbrRegress(data,8,2)
#  各种不同的滞后阶数
nnbrRegress(data,10,1)
nnbrRegress(data,10,2)
#  各种不同的滞后阶数
nnbrRegress(data,15,1)
nnbrRegress(data,15,2)
#  eof