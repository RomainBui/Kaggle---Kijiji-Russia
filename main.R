library(xlsx)
library(lubridate)
library(xts)
library(quantmod)
library(reshape2)
library(stringr)
library(data.table)


setwd("C:\\Users\\Herve\\Documents\\Projets\\Kaggle---Kijiji-Russia")
setwd("/Users/romainbui/Kaggle---Kijiji-Russia")

customRead = function(x)
{
  names.res = str_split(gsub(pattern = "\"","",readLines(x, n = 1)), " ")[[1]]
  res = fread(x, sep = " ")
  res = res[,V1:=NULL]
  setnames(res, names.res)
  return(res)
}

Search.Stream = customRead("train_Search_Stream_2.txt")
Search.Info   = customRead("search_info_2.txt")
AD.Info       = customRead("ads_info_2.txt")
User.Info     = customRead("user_info_2.txt")
User.Info[,UserID := as.numeric(UserID)]
Visits.Stream = customRead("visit_stream_2.txt")
setkey(Visits.Stream, "UserID")

# Creation of the big dataTable
mainTable = merge(merge(merge(Search.Stream, Search.Info, by = "SearchID", all.x = T), User.Info, by = "UserID", all.x = T), AD.Info, by = "AdID", all.x = T)


# For a given user ID 
userSpe = merge(Visits.Stream, AD.Info, by = "AdID", all.x = T)

topN = function(X, By, N = 3)
{
  res = data.frame(X, By)
  res = res[rev(order(By)),]
  return(as.list(res$X[1:N]))
}

createName = function(names, N = 3)
{
  a = as.vector(sapply(names, rep, times = N))
  b = rep(c(1:N), length(names))
  return(paste0(a,b))
}

# Create the new Column of the 3 Top AdID for the user Specific
myMerge = function(x,y){merge(x,y, by = "UserID")}
userSpeM = Reduce(myMerge, list(userSpe[,topN(ViewDate, ViewDate),UserID],
                 userSpe[,topN(AdID, ViewDate), UserID], 
                 userSpe[,topN(LocationID, ViewDate), UserID]))

setNames(userSpeM, c("UserID", createName(c("ViewDate", "AdID", "LocationID"))))


# Final Table
mainTable = merge(mainTable, userSpeM, by = "UserID", all.x = T)


####### CIMETERY ######

# X = userSpe[UserID == 246263]$AdID
# By = userSpe[UserID == 246263]$ViewDate
# 
# userSpe[UserID == 246263]
# 
# 
# # Adjustment of Visit Stream
# c = Visits.Stream[AD.Info]
# 
# MostFrequent = function(X){
#   return(as.numeric(sort(table(X), decreasing = TRUE)[1]))
# }
# 
# AggUserInfo = copy(c)
# AggUserInfo[, LoactionID := MostFrequent(LocationID), by = UserID][,list(UserID, LocationID)]
# 
# 
# 
# 
