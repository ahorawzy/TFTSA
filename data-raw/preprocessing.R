
# Clear working environment.
rm(list=ls())

# Load related packages.
library(TFTSA)
library(tidyverse)

# Read raw data.
# jd201610 <- read.csv("data-raw/jd201610.csv",stringsAsFactors = F)

# Change variables' names.
names(jd201610) <- c("station_index","station_name","date","hour","minute","CDH","SX",
                     "timestamp","small_pass_car","big_pass_car","small_fre_car",
                     "mid_fre_car","big_fre_car","large_fre_car","box_car","mt_car","tlj_car")

# Delete poor stations with little data.
jd201610 <- filter(jd201610,station_name != "ºÚË®",station_name != "ÎÚÉ³")

# The function to split date and return a dataframe.
str_split_todf <- function(strvec,sep){
  tep <- strsplit(strvec,sep)
  len <- length(tep[[1]])
  return(as.data.frame(matrix(unlist(tep),ncol=len,byrow = T),stringsAsFactors=F))
}

# Split date using str_split_todf.
jd201610 <- cbind(jd201610,str_split_todf(jd201610$date,"-"))
# Give names for new variables.
names(jd201610)[18:20] <- c("day","month","year")

# Change the expression of month.
jd201610[jd201610$month=="9ÔÂ ",19] <- "Sep"
jd201610[jd201610$month=="10ÔÂ",19] <- "Oct"

# Create new variable using "month" and "day".
jd201610 <- mutate(jd201610,date=str_c(month,day,sep = "-"))
# Select related variables.
jd201610 <- select(jd201610,-(day:year))

# Take done the right order of dates.
date_order <- unique(jd201610$date)

# Caculate equivalent traffic volume and select useful variables.
jd201610 <- mutate(jd201610,
                   volume=small_pass_car+small_fre_car+
                     1.5*big_pass_car+1.5*mid_fre_car+
                     3*big_fre_car+4*large_fre_car+4*box_car+
                     mt_car+tlj_car) %>%
  select(station_index:timestamp,volume) %>%
  select(-(hour:minute))

# Caculate the totle equivalent traffic volume per timestamp per day.
by_timestamp <- group_by(jd201610,station_name,date,timestamp)
jd201610tt <- summarise(by_timestamp,ttvolume=sum(volume,na.rm = T))

# Cast the data form long-form to wide-form
jdzzl <- plyr::dlply(jd201610tt,"station_name",
                     function(x) reshape2::dcast(x,date~timestamp))

# Give rownames: corresponding date and remove the first line.
jdzzl <- lapply(jdzzl, function(x) {
  rownames(x) <- x[[1]]
  x <- x[-1]
})

# Deal with missing value.
jdzzl <- lapply(jdzzl, function(x){

  # Remove dates which contains more than 5% missing value.
  x <- x[date_order,]
  x <- x[rowSums(is.na(x))<15,]

  # Unfold matrix.
  x <- as.matrix(x)
  dates <- rownames(x)
  x <- t(x)
  x <- as.vector(x)

  # Fill up missing value.
  na_loc <- which(is.na(x))
  notna_loc <- which(!is.na(x))
  for(i in na_loc){
    x[i] <- mean(x[notna_loc[which(abs(i-notna_loc)<7)]])
  }

  # Restruct matrix.
  x <- matrix(x,ncol = 288,byrow = T)
  rownames(x) <- dates
  colnames(x) <- 1:288
  return(x)
})

# The function which can caculate the loess value for every line in the input dataframe.
# The return value is a equal-scale dataframe.
handle_loess_fordf <- function(df,sp){
  ndays <- nrow(df)
  timestamps <- 1:ncol(df)
  for(i in 1:ndays){
    loess_result <- stats::loess(df[i,]~timestamps,span=sp)
    df[i,] <- loess_result$fitted
  }
  return(df)
}

# Caculate loess value for traffic data of all stations.
jdzzl_loess <- lapply(jdzzl, handle_loess_fordf, sp=0.2)
