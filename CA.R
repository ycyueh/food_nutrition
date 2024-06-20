library(readxl)
library(stats)
library(ggcorrplot) #for plot correlation
library(reshape2)#for ggplot 輸入
library(ggplot2)
library(factoextra)
library(tidyverse)
load('lndata.Rdata')
hc_data<-nd3
#rownames(data_clean2)<-paste0('r',1:nrow(data_clean2))
hc_data[,-1]<-scale(hc_data[,-1])

library(openxlsx)

meth = k[3]
dif_dis <- function(meth) {
  dis=dist(hc_data[,-1], method = meth, diag = TRUE, upper = TRUE)
  y = hclust(dis,'ward.D2')
  plot(y,xlab = '距離',main = paste('Dendrogram using', meth, 'distance'))
  cut_avg <- cutree(y, k=3)
  seeds_df_cl <- mutate(hc_data, cluster = cut_avg)
  t1<-count(seeds_df_cl,cluster)
  t2<-table(seeds_df_cl$食品分類,seeds_df_cl$cluster)
  xl_lst<-list(count = t1,table = t2)
  #write.xlsx(xl_lst, file=paste0(meth,".xlsx"), rowNames=FALSE)
}
dif_dis('euclidean')

dif_dis_f <- function(meth,i) {
  dis=dist(hc_data[,-1], method = meth, diag = TRUE, upper = TRUE)
  y = hclust(dis,'ward.D2')
  cut_avg <- cutree(y, k=i)
  seeds_df_cl <- mutate(hc_data, cluster = cut_avg)
  t1<-count(seeds_df_cl,cluster)
  t2<-table(seeds_df_cl$食品分類,seeds_df_cl$cluster)
  fviz_cluster(main = paste('cluster using', meth, 'distance'),list(data = hc_data[,-1], cluster = cut_avg))
  xl_lst <- list(t1 = t1, t2 = t2)
  #write.xlsx(xl_lst, file = paste0(meth, "_k_", i, ".xlsx"), rowNames=FALSE)
}
k<-list('euclidean','canberra','manhattan','maximum')
meth
i=2:4
dis<-list('euclidean','manhattan')
results<-lapply(k, dif_dis)
# 使用 lapply 進行嵌套迭代
result <- lapply(dis, function(meth) {
  lapply(i, function(cluster_num) {
    dif_dis_f(meth, cluster_num)
  })
})
names(results) <- k

meth=k[3]
dis=dist(hc_data[,-1], method = meth, diag = TRUE, upper = TRUE)
y = hclust(dis,'ward.D2')
plot(y,xlab = '距離',main = paste('Dendrogram using', meth, 'distance'))


fviz_nbclust(x = hc_data[,-1],FUNcluster = hcut, method = "silhouette",diss = dis)+
  labs(subtitle = paste( meth, 'distance'))
cut_avg <- cutree(y, k=3)
seeds_df_cl <- mutate(hc_data, cluster = cut_avg)
t1<-count(seeds_df_cl,cluster)
t2<-table(seeds_df_cl$食品分類,seeds_df_cl$cluster)
fviz_cluster(list(data = hc_data[,-1], cluster = cut_avg),geom = 'point')





dis2=dist(newdf[,-1], method = "euclidean", diag = TRUE, upper = TRUE)

#kmeans####
#scale each variable to have a mean of 0 and sd of 1
km_data<-nd3
km_data[,-1]<-scale(km_data[,-1])
fviz_nbclust(km_data[,-1], 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 12             # max number of clusters
)+labs(title="平均輪廓法 for K-Means") 
##分2群或4群
set.seed(88)
#perform k-means clustering with k = 3 clusters
km <- kmeans(km_data[,-1], centers = 3, nstart = 30)
tk_2c1<-table(km$cluster)
tk_2c2<-table(km_data$食品分類,km$cluster)
fviz_cluster(km, data = km_data[, -1],
             palette = c("#2E9FDF",  "#E7B800",
                         'pink','brown'
                         ), 
            # axes = c(3, 4),  
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

library(openxlsx)
as.data.frame(t(km$centers))
xl_lst <- list('sheet1' = tk_2c1, 'sheet2' = tk_2c2)

#write.xlsx(xl_lst, file="kmean兩群.xlsx", rowNames=FALSE)
#perform k-means clustering with k = 3 clusters
km <- kmeans(km_data[,-1], centers =3, nstart = 40)
tk_3c1<-table(km$cluster)
tk_3c2<-table(km_data$食品分類,km$cluster)
xl_lst <- list('sheet1' = tk_4c1, 'sheet2' = tk_4c2)
fviz_cluster(km, data = km_data[, -1],
             palette = c("#2E9FDF",  "#E7B800",'pink'), 
             geom = "point",
             
             ellipse.type = "convex", 
             ggtheme = theme_bw())
#perform k-means clustering with k = 4 clusters
km <- kmeans(km_data[,-1], centers =4, nstart = 40)
tk_4c1<-table(km$cluster)
tk_4c2<-table(km_data$食品分類,km$cluster)
xl_lst <- list('sheet1' = tk_4c1, 'sheet2' = tk_4c2)
fviz_cluster(km, data = km_data[, -1],
             palette = c("#2E9FDF",  "#E7B800",'pink','brown'), 
             geom = "point",
             ellipse.type = "convex",
             axes = c(3, 4),
             ggtheme = theme_bw())

#write.xlsx(xl_lst, file="kmean四群.xlsx", rowNames=FALSE)
