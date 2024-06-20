library(readxl)
library(ggcorrplot) #for plot correlation
library(reshape2)#for ggplot 輸入
library(ggplot2)
library(factoextra)
library(tidyverse)
library(psych) 
#https://consumer.fda.gov.tw/Food/TFND.aspx?nodeID=178
##準備數據
load('lndata.Rdata')
#-----------------
#ref:https://rpubs.com/zoexoe/592607
#https://bookdown.org/lien_lamey/R_tutorial/7-3-hierarchical-clustering.html
## fa ##
#####因素分析是主成分分析的推廣與發展，
# 也是降維的一種方式， 
# 可用來分析隱藏在表面現象背後的因素。
# 因素分析是研究相關矩陣或共變異矩陣的內部依賴關係，
# 將多個變數綜合為少數幾個因素，
# 再現原始變數與因素間的關係。
# Scale the data 
library(corrplot)
nd3<-lndata
fa_data<-nd3[,-1]
head(fa_data)
fa_data<-scale(fa_data)
KMO(fa_data)
cor_matrix<-cor(fa_data)
corrplot(cor_matrix,tl.cex = 0.5)


#  Bartlett's Test
bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(fa_data))
print(bartlett_result)


#平行分析
fa.parallel(cor_matrix,  fa="both" ,n.obs = nrow(fa_data), show.legend = TRUE, n.iter = 100)


##8個因子
##cross-loading
##用VARIMAX
#考慮刪除共同性<0.2之題項。
fa<-fa(r = cor_matrix, 
   nfactors =8,rotate="varimax", scores="regression",
   fm="minres")
fa$communality<0.2
## none of those are <0.2

print.psych(fa, cut = 0.4, sort = TRUE)
fa$loadings
#

fa_data<-as.data.frame(fa_data)


##刪除loading皆很小的： 維生素B2(mg)
fa_data<-fa_data |> 
  select(-c(`維生素B2(mg)`))
KMO(fa_data)
cor_matrix<-cor(fa_data)
fa<-fa(r = cor_matrix, 
       nfactors =8,rotate="varimax", scores="regression",
        fm="minres")

print.psych(fa, cut = 0.4, sort = TRUE)
fa$loadings


## communality
#amount of variance in a variable that can be explained by the factors.
# Create the data frame with communality values
df1 <- data.frame(name = names(fa$communality),
                  communality = fa$communality)
# Sort the data frame by communality in decreasing order
df1 <- df1[order(df1$communality, decreasing = TRUE), ]
# ggplot barplot
ggplot(df1, aes(x = reorder(name, communality), y = communality)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() + # Flip coordinates for better readability
  labs(title = "Communality Bar Plot", x = "Name", y = "Communality") +
  theme_minimal()+
  geom_hline(yintercept = 0.5, color = "red")

fa$Vaccounted
summary(fa)

scores <- factor.scores(fa_data, fa)
scores <- as_tibble(scores$scores)
scores <- bind_cols(`食品分類`=nd2[,1], scores) |>
  mutate(食品分類 = factor(食品分類))
library("RColorBrewer")
# Define the number of colors you want
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)
scores |>
  filter(食品分類=='水果類'|食品分類=='魚貝類'|食品分類=='堅果及種子類'|食品分類=='油脂類') |> 
  ggplot(aes(MR1, MR8, color = 食品分類)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  #scale_color_manual(values = mycolors) +
  stat_ellipse()

scores |>
  ggplot(aes(MR2, MR8, color = 食品分類)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_manual(values = mycolors) +
  stat_ellipse()
VM.load = fa$loadings[,1:2]
plot(VM.load, type="n",xlim = c(-0.3,0.7))
text(VM.load,labels=colnames(nd1),cex=.5) # add variable names
# 其中，SS Loadings, Proportion Var, Cumulative Var 定義如下:
#   
# SS Loadings 為公共因素 fi對變數 x1, x2, x3, x4, x5
# 的總方差貢獻，即 g2j=∑5i=1a2ij
# 
# Proportion Var 為方差貢獻率，即 g2j/∑5i=1var(xi)
# 
# Cumulative Var 為累積方差貢獻率，即 ∑jk=1g2j/∑5i=1var(xi)
# 提取负载矩阵并转换为数据框
loadings <- as.data.frame(as.table(fa$loadings))
colnames(loadings) <- c("Variable", "Factor", "Loading")

# 仅保留负载系数绝对值大于等于 0.5 的项
significant_loadings <- loadings %>% filter(abs(Loading) >= 0.5)
significant_loadings <- significant_loadings %>% arrange(Factor, desc(abs(Loading)))

s1<-significant_loadings |> 
  filter(Factor=='MR3'|Factor=='MR4'|Factor=='MR5'|Factor=='MR8')
# 使用 ggplot2 绘制因子负载图
ggplot(s1, aes(x = reorder(Variable, desc(abs(Loading))), y = Loading, fill = as.factor(Factor))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Factor) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Factor Loadings (|Loading| >= 0.5)", x = "Variables", y = "Loadings", fill = "Factor")



# 设置更大的图形设备
par(mar = c(5, 4, 4, 2) + 0.1) # 调整图形设备的边界
plot.new()
# 設定繁體中文字型
# 注意：請根據您自己的系統和字型進行調整
pdf("my_fa_diagram.pdf", height=30, width=15,family = "GB1")
#fa.diagram(fa, main="Factor Analysis")
fa.diagram(fa,cex = 5,cut=0.4)#diagram of this factor structure 

#fa.diagram(fa,cex = 0.1,rsize = 0,cut=0.5, main="Factor Analysis")#diagram of this factor structure 
dev.off()

fa.diagram(fa, main="Factor Analysis")
print.psych(fa, cut = 0.4, sort = TRUE)
li<-list()
for (i in 1:10) {
 li[[i]]<- (fa$loadings[abs(fa$loadings[,i])>=0.5,i])
}
fa$loadings[abs(fa$loadings[,1])>=0.5,1]
