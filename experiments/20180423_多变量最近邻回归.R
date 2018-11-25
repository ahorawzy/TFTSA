#  基于knn回归的最近邻算法
rm(list=ls())
nnbrRegressMultivariate<-function (xData , yData , testData)
{
  if(nrow(xData)!=nrow(yData))
  {
    stop('error input data') 
  }
  
  if (ncol(xData)!=ncol(testData))
  {
    stop('error input data')
  }
  
  if (!is.matrix(xData) ||  !is.matrix(yData) || !is.matrix(testData))
  {
    stop('error input data')
  }
  
  #  取得特征向量的个数
  k<-ceiling(sqrt(nrow(xData)))
  
  yForecasting<-matrix(NaN , nrow = nrow(testData),ncol = 1)
  
  for (i in c(1:nrow(testData)))
  {
    x<-testData[i,]
    
    xDataRep <-rep(x,nrow(xData))
    xDataRep <- matrix(xDataRep,nrow = nrow(xData) , byrow = TRUE)
    #  计算距离
    
    Distance <- sqrt(apply((xData - xDataRep)^2,1,sum))
    
    Index <- order(Distance)
    #  选择前k个值
    Index<-Index[c(1:k)]
    ys<-(yData[Index])
    Weight<-(1/Distance)/sum(1/Distance)
    
    yForecasting[i] <-sum(ys*Weight)
    
  }
  return(yForecasting)
  
}


xData <- matrix(rnorm(100),ncol = 4)
yData <- as.matrix(apply(xData,1,mean))
testData <- matrix(rnorm(40),ncol = 4)

nnbrRegressMultivariate(xData , yData , testData)