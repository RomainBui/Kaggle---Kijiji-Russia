library(xlsx)
library(lubridate)
library(xts)
library(quantmod)
library(reshape2)
library(stringr)
library(data.table)


#setwd("C:\\Users\\Herve\\Documents\\Projets\\Kaggle---Kijiji-Russia")
#setwd("/Users/romainbui/Kaggle---Kijiji-Russia")
setwd("C:/Users/rbui/Desktop/Kaggle/Kaggle---Kijiji-Russia-master")

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
mainTable$SearchDate = as.POSIXct(mainTable$SearchDate) # To ensure proper filtering on date

# Romain : 1 Popularité de la recherche en clef (categorie)
popSearch = function(x, SearchDate_ref, CategoryID_ref) # x: the mainTable
{
  #x = mainTable
  #x$SearchDate = as.POSIXct(x$SearchDate)
  #SearchDate_temp = x[1,]$SearchDate
  #CategoryID_temp = x[1,]$CategoryID.y
  res =  dim(x[SearchDate < SearchDate_ref & CategoryID.y == CategoryID_ref])[1] / dim(x[SearchDate < SearchDate_ref])[1] 
  return(res)
}

#test= mainTable[1:100]
#test[, r_pop:=popSearch(test, SearchDate, CategoryID.x), by = c("AdID", "UserID")]
mainTable[, r_pop:=popSearch(mainTable, SearchDate, CategoryID.x), by = c("AdID", "UserID")]


# Romain: 2 Most Frequent location for this search (category ?)
locSearch = function(x, SearchDate_ref, CategoryID_ref)
{
  #x = mainTable
  #SearchDate_ref = x[1,]$SearchDate
  #CategoryID_ref = x[1,]$CategoryID.y
  res = x[SearchDate < SearchDate_ref & CategoryID.y == CategoryID_ref]
  res = res$LocationID.x;
  #res[1] = 2643
  res = table(res)
  res = as.numeric(attr(sort(res, decreasing = T)[1], "names"))
  return(res)
}
#test = mainTable[1:100]
#test[, r_loc := locSearch(test, SearchDate, CategoryID.x), by= c("AdID", "UserID")]
mainTable[, r_loc := locSearch(mainTable, SearchDate, CategoryID.x), by= c("AdID", "UserID")]






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
