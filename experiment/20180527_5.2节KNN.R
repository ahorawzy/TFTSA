# Experiment Title: Flow predicting using knn.
# Date: 2018-05-27
# Description: This experiment aims to check the usability of function "flowknn"
# and use it in static predicting. The object traffic flow is 6th of Octuber,
# the flow-base contains all flows except for the object traffic flow.

# -----------------------------------------------------------------------

# 1. Read the two wide flow datasets.
# One is raw data for object flow and flow-base,
# the other is loess data for flow-base.
tmlzzloess <- read.csv("data-raw/tmlzzloess.csv")
tmlzznew <- read.csv("data-raw/tmlzznew.csv",header = T)

rownames(tmlzzloess) <- tmlzzloess[,1]
tmlzzloess <- tmlzzloess[,-1]

rownames(tmlzznew) <- tmlzznew[,1]
tmlzznew <- tmlzznew[,-1]

# -----------------------------------------------------------------------

# 2. flow-base is raw data

## 2.1 object flow and flow-base identify
tmlobj <- tmlzznew[6,]
tmlbase <- tmlzznew[-6,]

## 2.2 execute flowknn function
pre1006 <- flow_knn(tmlobj,tmlbase,start = 73,k = 3,lag_duration = 24,fore_duration = 12)

## 2.3 present outcome
plot(1:288,pre1006,type="l",col="red")
points(1:288,tmlobj,col="blue")
