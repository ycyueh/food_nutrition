library(readxl)
library(stats)
library(ggcorrplot) #for plot correlation
library(reshape2)#for ggplot 輸入
library(ggplot2)
library(factoextra)
library(tidyverse)
#https://consumer.fda.gov.tw/Food/TFND.aspx?nodeID=178
data1<-read_excel("D:\\食品營養成分資料庫2023版UPDATE1(V1).xlsx",skip = 1)
data<-data1
colnames(data)
head(data)
##去除非數值variable
data<-data[,-c(1,3,4,5)]
colnames(data)
##去除維生素D總量/糖質總量(g)/##脂肪酸S總量(mg)
nd1<-data[,-c(2,3,13:18,29:32,34:36,38:42,54:68,70:74,76:82,84,87:104,106)]
colnames(nd1)
nd1<-nd1[!(is.na(nd1$`修正熱量(kcal)`)),]

sum(is.na(nd1$`修正熱量(kcal)`))
library(corrplot)

##
library(DMwR)
nd1<-as.data.frame(nd1)
na_num<-sapply(nd1, function(x) sum(is.na(x)))
na_num<-as.data.frame( na_num)
na_num<-na_num |> 
  mutate('na比例' = na_num/nrow(nd1))
colnames(na_num)[1]<-'na個數'
#write.csv(na_num,'na_num.csv',fileEncoding = 'big5')
imputeData <- knnImputation(nd1[,-1])
sum(is.na(imputeData))
nd2<-cbind(`食品分類`=nd1[,1],imputeData)

which(apply(nd2[,-1], 2, sd)==0)

names(nd2[,-1])

dim(nd2)
coln <- colnames(nd2)[-1]
hist_fn <- function(x, col_name) {
  hist(x, xlab = col_name,main=paste0('hist of ',col_name))
}
par(mfrow=c(3,5))
# Loop through the column indices
for (i in seq_along(coln)) {
  hist_fn(nd2[[i+1]], coln[i])
}
##除了水分，其他有偏態

##平移取ln
nd3<-nd2
nd3[,-c(1,3)]<-log(nd3[,-c(1,3)]+1)

for (i in seq_along(coln)) {
  hist_fn(nd3[[i+1]], coln[i])
}

lndata<-nd3
save.image("lndata.RData")
