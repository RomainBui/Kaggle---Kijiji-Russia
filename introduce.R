library(reshape2)
library(data.table)
library(xlsx)
library(lubridate)
library(xts)
library(quantmod)
library(reshape2)
library(stringr)
library(data.table)

setwd("C:\\Users\\Herve\\Documents\\Projets\\Kaggle---Kijiji-Russia")

# debut=proc.time()
# ads_info=fread("C:\\Users\\Herve\\Downloads\\AdsInfo.tsv",sep="\t",nrows=25000)
# train_Search_Stream= fread("C:\\Users\\Herve\\Downloads\\trainSearchStream.tsv",sep="\t",nrows=25000)
# search_info=fread("C:\\Users\\Herve\\Downloads\\SearchInfo.tsv",sep="\t",nrows=25000)
# visit_stream=fread("C:\\Users\\Herve\\Downloads\\VisitsStream.tsv",sep="\t",nrows=25000)
# user_info=fread("C:\\Users\\Herve\\Downloads\\UserInfo.tsv",sep="\t",nrows=25000)
# phone_request=fread("C:\\Users\\Herve\\Downloads\\PhoneRequestsStream.tsv",sep="\t",nrows=25000)
# location=fread("C:\\Users\\Herve\\Downloads\\Location.tsv",sep="\t",nrows=25000)
# category=fread("C:\\Users\\Herve\\Downloads\\Category.tsv",sep="\t",nrows=25000)
# fin=proc.time()
# (fin-debut)
# 
# 
# write.table(train_Search_Stream,"train_Search_Stream_2.txt")
# write.table(search_info,"search_info_2.txt")
# write.table(visit_stream,"visit_stream_2.txt")
# write.table(user_info,"user_info_2.txt")
# write.table(ads_info,"ads_info_2.txt")
# write.table(location, "location_2.txt")
# write.table(category,"category_2.txt")
# write.table(phone_request,"phone_request_2.txt")


train_Search_Stream=data.table(read.table("train_Search_Stream_2.txt",header=TRUE))
ads_info=data.table(read.table("ads_info_2.txt",header=TRUE))
visit_stream=data.table(read.table("visit_stream_2.txt",header=TRUE))
search_info=data.table(read.table("search_info_2.txt",header=TRUE))
user_info=data.table(read.table("user_info_2.txt",header=TRUE))
category=data.table(read.table("category_2.txt",header=TRUE))
location=data.table(read.table("location_2.txt",header=TRUE))
phone_request=data.table(read.table("phone_request_2.txt",header=TRUE))


setkey(train_Search_Stream, "SearchID","AdID")
setkey(ads_info, "AdID")
setkey(visit_stream, "UserID")
setkey(search_info, "SearchID","UserID")

essaie=merge(train_Search_Stream,search_info)
setkey(essaie,"AdID")
essaie2=merge(essaie,ads_info)


test=fread(file.choose(),header=TRUE,nrow=500) 
test

length(unique(search_info$SearchID))
length(unique(search_info$UserID))
# SearchID 
str(train_Search_Stream)

train_Search_Stream[,.N/25000,by=ObjectType]


# Ommetre les ads non-contextuel--> 51% de non contextuels
# So rien a foutre des non-contextuelles 
data_train_search=train_Search_Stream[ObjectType==3]


data_train_search[,.N,by=IsClick]
# Nombre de IsClick=77 sur 12 000--->0.6%
# Prendre une approche de modélisation d'un evenement rare
#  

data_info=merge(data_train_search,search_info)
setkey(data_info,"SearchID","AdID","UserID","LocationID")
essaie=merge(data_info,location)
#######################################################################
############# Creation de variables pour donner du jeu au modele#######

str(search_info)
#### La location

data_info=merge(data_train_search,search_info)
setkey(data_info,"SearchID","AdID","UserID","LocationID")
setkey(location,"LocationID")
setkey(ads_info,"AdID")
essaie=merge(data_info,location)
setkey(essaie,"SearchID","AdID","UserID","LocationID")
essaie2=merge(data_info,ads_info)
# Soucis? DES NA pour la location des ads

ads_info[IsContext==1]

# Reponse Soucis avec les location des ADS, Il semblerait que pour les contextuels, il n'y a que des NA
#Il n'est donc pas possible d'analyser le pouvoir prediciif du match des Locations (Ads et User)


#######################################################################
########### Sur les categorie

new_dat=as.POSIXlt(data_info$SearchDate)

data_info$new_date=new_dat
 get_cat<-function(x){
  
  essaie=date_info[sea<date_info$SearchDate[x]]
}
#a finir



str(data_info)
