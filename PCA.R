library(readxl)
library(stats)
library(ggcorrplot) #for plot correlation
library(reshape2)#for ggplot 輸入
library(ggplot2)
library(factoextra)
library(tidyverse)
#https://consumer.fda.gov.tw/Food/TFND.aspx?nodeID=178
load('lndata.Rdata')
##
options(scipen = 99)
nd3<-lndata
pca <- prcomp(nd3[,-1], center = TRUE, scale. = TRUE)
pca_summary<-summary(pca)
print(pca_summary)
sum(pca_summary$sdev^2>1)
# BY eigenvalues >1
#取7個pc
{plot(pca,type='line')
  abline(h=1,col='pink')}
library("factoextra")
fviz_screeplot(pca,addlabels = TRUE)
loadings <- pca$rotation
print(pca_summary$importance)[,1:7]
print(loadings)

#save.image('pca_new.Rdata')
#### step2:run pca ####
# Perform PCA on the filtered data

##21個pc解釋0.9

# 解釋 0.76490 ---- 
# 查看主成分的載荷矩陣
loadings <- pca$rotation
print(loadings)
# 解釋
### https://www.superlab.com.tw/s138/
(PC1<-loadings[,1][order(abs(loadings[,1]),decreasing = T)])
##胺基酸
(PC2<-loadings[,2][order(abs(loadings[,2]),decreasing = T)])
##油脂類
(PC3<-loadings[,3][order(abs(loadings[,3]),decreasing = T)])##脂肪酸
##順式多元不飽和脂肪酸總量

(PC4<-loadings[,4][order(abs(loadings[,4]),decreasing = T)])
##飽和脂肪酸總量、順式單元不飽和脂肪酸總量
(PC5<-loadings[,5][order(abs(loadings[,5]),decreasing = T)])
##纖維

# 定義一個函數，將載荷矩陣轉換為數據框並繪製條形圖
PC_plot <- function(pc, title) {
  sorted_loadings_df <- data.frame(
    Variable = names(pc),
    Loading = pc
  )
  
  # 繪製主成分載荷的條形圖
  p <- ggplot(sorted_loadings_df, aes(x = reorder(Variable, -abs(Loading)), y = Loading)) +
    geom_bar(stat = "identity") +
    xlab("變量") +
    ylab("載荷值") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(p)
}

# 定義主成分列表
pclist <- list(PC1, PC2, PC3, PC4, PC5)
titles <- c("第一主成分的載荷值", "第二主成分的載荷值", "第三主成分的載荷值", "第四主成分的載荷值", "第五主成分的載荷值")

# 使用循環繪製每個主成分的載荷條形圖
for (i in 1:length(pclist)) {
  #png(filename=paste0("PC",i,'.png'))
  PC_plot(pclist[[i]], titles[i])
  #dev.off()
}



# 查看每個樣本在主成分上的得分
pca$x
# Graph of the variables
# fviz_pca_var(pca, col.var = "cos2",
#              gradient.cols = c("black", "orange", "green"),
#              repel = TRUE)
table(nd1$食品分類)
fviz_pca_ind(pca,geom.ind=c("point"),point.size=3,pointshape=16,
             col.ind=nd1$食品分類,
             #col="Set1",
             legend.title="Species",addlabel=TRUE)

fviz_pca_ind(pca,axes = c(2,3),geom.ind=c("point"),point.size=3,pointshape=16,
             col.ind=nd2$食品分類,
             col="Set1",legend.title="Species",addlabel=TRUE)
fviz_pca_ind(pca,axes = c(4,5),geom.ind=c("point"),point.size=3,pointshape=16,
             col.ind=data$食品分類,
             col="Set1",legend.title="Species",addlabel=TRUE)
pca$rotation
var <- get_pca_var(pca)
library(corrplot)
#corrplot(var$cos2[,c(1:2)],is.corr=FALSE,tl.cex = 0.3)
#較高的cos2代表變數被主成分1及主成分2代表性高，越高就代表變量在主成分分析里面越重要。
fviz_cos2(pca,choice='var',axes=1:2)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept = 0.5)

# Add ellipses
fviz_pca_ind(pca, label="none", habillage=nd2$食品分類)
fviz_pca_ind(pca,axes = c(2,3), label="none", habillage=nd2$食品分類,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_pca_ind(pca,axes = c(4,5), label="none", habillage=data_clean$食品分類,
             addEllipses=TRUE, ellipse.level=0.95)
#在主成分分析（PCA）中
# cos2 是指個體（觀測值）和主成分之間的平方餘弦（或平方相關性）。
# 它衡量了個體在因子圖上的表示質量。
# 較高的 cos2 值表示個體在主成分上的表示效果較好。
# Select and visualize individuals with cos2 > 0.96
fviz_pca_ind(pca, select.ind = list(cos2 = 0.6),habillage = nd2$食品分類,)

# Change the color by groups, add ellipses
fviz_pca_biplot(pca, habillage=nd1$食品分類,invisible = 'var',
                select.ind = list(contrib = 100))



####  BY 累積解釋變異 16個pc ####