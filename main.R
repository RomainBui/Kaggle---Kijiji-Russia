library(xlsx)
library(lubridate)
library(xts)
library(quantmod)
library(reshape2)
library(stringr)
library(data.table)


setwd("/Users/romainbui/Documents/Harvard Classes/Kaggle/Kaggle")


Search.Stream = fread("./train_Search_Stream.txt", skip = 0)
setkey(Search.Stream, "SearchID")
Search.Info   = fread("./search_info.txt", skip = 0)
AD.Info       = fread("./ads_info.txt", skip = 0)
setkey(AD.Info, "AdID")
User.Info     = fread("./user_info.txt", skip = 0)
User.Info[,UserID := as.numeric(UserID)]
Visits.Stream = fread("./visit_stream.txt", skip = 0)
setkey(Visits.Stream, "UserID")

# Creation of the big dataTable
a = Search.Stream[Search.Info]
setkey(a, "UserID")
mainTable = merge(a, User.Info)

# Adjustment of Visit Stream
c = Visits.Stream[AD.Info]

MostFrequent = function(X){
  return(as.numeric(sort(table(X), decreasing = TRUE)[1]))
}

AggUserInfo = copy(c)
AggUserInfo[, LoactionID := MostFrequent(LocationID), by = UserID][,list(UserID, LocationID)]