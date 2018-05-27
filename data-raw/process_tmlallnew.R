# scripts for processing tmlallnew

# select fields
tmlallnew <- tmlallnew[,c(1,2,3)]

# generate LOESS data by day
days <- unique(tmlallnew$日期)
for (i in days) {
  tmlallnew[tmlallnew$日期==i,4] <- handle_loess(tmlallnew[tmlallnew$日期==i,],exp=2,resp=3,sp=0.2)
}
names(tmlallnew)[4] <- "LOESS"

# process loess value which's below 0
tmlallnew[tmlallnew$LOESS<0,"LOESS"] = 0

# select fields include day, timestamp, loess
tmlallnew <- tmlallnew[,c(1,2,4)]

# generate wide data using reshape2::dcast
tmlzzloess <- reshape2::dcast(tmlallnew,tmlallnew$日期~tmlallnew$时间序号)
rownames(tmlzzloess) <- tmlzzloess[,1]
tmlzzloess <- tmlzzloess[,-1]

# check the completeness of tmlzzloess
sum(is.na(tmlzzloess))

# write tmlzzloess to data-raw/
write.csv(tmlzzloess,file="data-raw/tmlzzloess.csv",row.names = F)
