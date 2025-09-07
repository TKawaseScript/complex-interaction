library(VGAM)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)


#TableS4
degree_dist_All_all_digdis<-read.csv("degree_dist_All_all_digdis.csv")
sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T,fileEncoding = "UTF-8")
degree_dist_out_digdis<-read.csv("degree_dist_out_digdis.csv")
degree_dist_in_digdis<-read.csv("degree_dist_in_digdis.csv")

popmeans<-NULL
for(i in 1:length(degree_dist_All_all_digdis$X)){
  popmeans[i]<-sp_food_coltp0[sp_food_coltp0$cause==degree_dist_All_all_digdis$X[i],]$popmean
}

foods<-NULL
for(i in 1:length(degree_dist_All_all_digdis$X)){
  foods[i]<-sp_food_coltp0[sp_food_coltp0$cause==degree_dist_All_all_digdis$X[i],]$foodhabit7
}


Each_Other_lm_data<-data.frame("countCause"=degree_dist_out_digdis$x,"counteffect"=degree_dist_in_digdis$x,"popmean"=popmeans,"food"=foods)

#原因側のリンクにおいての平均個体数と食性との関係

popmeans<-NULL
for(i in 1:length(degree_dist_out_digdis$X)){
  popmeans[i]<-sp_food_coltp0[sp_food_coltp0$cause==degree_dist_out_digdis$X[i],]$popmean
}

foods<-NULL
for(i in 1:length(degree_dist_out_digdis$X)){
  foods[i]<-sp_food_coltp0[sp_food_coltp0$cause==degree_dist_out_digdis$X[i],]$foodhabit7
}

Out_lm_data<-data.frame("count"=degree_dist_out_digdis$x,"popmean"=popmeans,"food"=foods)

glm_Out<-glm(count~popmean*food,data=Out_lm_data,family="poisson")

anova_out<-anova(glm_Out,test = "Chisq")

plotTableS4a<-ggplot(Out_lm_data,aes(x=popmean,y=count,color=food))+
  geom_point(alpha=0.6)+
  geom_smooth(
    aes(group = 1),
    method = "glm",
    method.args = list(family = "poisson"),
    formula = y ~ x,
    se = FALSE,
    color = "black"
  )+
  theme_minimal()+
  theme(
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  axis.line=element_line(color = "black"),
  panel.border=element_blank(),
  legend.position="none"
  )+
  scale_color_manual(values=c("browser"="darkorange","fry-feeder"="royalblue1","grazer"="lightgreen","omnivore"="gray","piscivore"="red","scale-eater"="blueviolet","shrimp-eater"="pink"))+
  ylim(0,14)+
  labs(
    x="Population mean",
    y="Number of causal relationship"
  )

summary(glm_Out)

write.csv(anova_out,"TableS4_anova_out.csv")
write.csv(capture.output(summary(glm_Out)),"TableS4_out.csv")



#結果側のリンクにおいての平均個体数と食性との関係

popmeans<-NULL
for(i in 1:length(degree_dist_in_digdis$X)){
  popmeans[i]<-sp_food_coltp0[sp_food_coltp0$cause==degree_dist_in_digdis$X[i],]$popmean
}

foods<-NULL
for(i in 1:length(degree_dist_in_digdis$X)){
  foods[i]<-sp_food_coltp0[sp_food_coltp0$cause==degree_dist_in_digdis$X[i],]$foodhabit7
}

In_lm_data<-data.frame("count"=degree_dist_in_digdis$x,"popmean"=popmeans,"food"=foods)

glm_In<-glm(count~popmean*food,data=In_lm_data,family="poisson")

anova_in<-anova(glm_In,test = "Chisq")

In_lm_data$pred_countEffect <- predict(glm_In, type = "response")

summary(glm_In)

plotTableS4b<-ggplot(In_lm_data,aes(x=popmean,y=count,color=food))+
  geom_point(alpha=0.6)+
  geom_smooth(method="glm",method.args=list(family="poisson"),se=FALSE)+
  theme_minimal()+
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.line=element_line(color = "black"),
    panel.border=element_blank(),
  )+
  scale_color_manual(values=c("browser"="darkorange","fry-feeder"="royalblue1","grazer"="lightgreen","omnivore"="gray","piscivore"="red","scale-eater"="blueviolet","shrimp-eater"="pink"))+
  ylim(0,14)+
  labs(
    x="Population mean",
    y="Number of repicient relationship"
  )


write.csv(anova_in,"TableS4_anova_in.csv")

write.csv(capture.output(summary(glm_In)),"TableS4_in.csv")

pdf("FigS7.pdf")
plot_grid(plotTableS4a,plotTableS4b)
dev.off()

#TableS5 媒介中心生モデルのものも追加して解析を行う
TableS1data<-read.csv("TableS1.csv")

glmdataCen <-subset(TableS1data,TableS1data$Centrality.betweenness!= "-")

glm_dataFrame<-data.frame("Abbreviation"=glmdataCen$Abbreviation,"centrality"=glmdataCen$`Centrality.betweenness`,"popmean"=glmdataCen$mean,"food"=glmdataCen$Food.habit)

glm_dataFrame_Outname<-NULL
for(i in 1:nrow(degree_dist_out_digdis)){
  glm_dataFrame_Outname<-c(glm_dataFrame_Outname,sp_food_coltp0[degree_dist_out_digdis$X[i]==sp_food_coltp0$cause,]$bindre.namesp)
}

degree_dist_out_digdis$X<-glm_dataFrame_Outname

glm_dataFrame_Inname<-NULL
for(i in 1:nrow(degree_dist_in_digdis)){
  glm_dataFrame_Inname<-c(glm_dataFrame_Inname,sp_food_coltp0[degree_dist_in_digdis$X[i]==sp_food_coltp0$cause,]$bindre.namesp)
}

degree_dist_in_digdis$X<-glm_dataFrame_Inname
colnames(degree_dist_in_digdis)<-c("Abbreviation","in_count")
colnames(degree_dist_out_digdis)<-c("Abbreviation","out_count")

mergeData_in<-merge(glmAICdataCen[,-1], degree_dist_in_digdis, by = "Abbreviation", all.x = TRUE)


mergeData_in<-merge(glmdataCen, degree_dist_in_digdis, by = "Abbreviation", all.x = TRUE)

mergeData_in_out<-merge(mergeData_in, degree_dist_out_digdis, by = "Abbreviation", all.x = TRUE)

mergeData_in_out$Centrality.betweenness<-as.numeric(mergeData_in_out$Centrality.betweenness)

mergeData_in_out$mean<-as.numeric(mergeData_in_out$mean)

lm_pred<-predict(lm(mergeData_in_out$Centrality.betweenness~mergeData_in_out$mean*mergeData_in_out$Food.habit),type="response")

mergeData_in_out$pred_centrality<-lm_pred

options(na.action="na.fail")
anova_cent<-anova(lm(mergeData_in_out$Centrality.betweenness~mergeData_in_out$mean*mergeData_in_out$Food.habit))

write.csv(anova_cent,"TableS5.csv")

#VGAM(双方向の因果関係数~popmean+food)

# グラフの作成
FigS6_a<-ggplot(Each_Other_lm_data, aes(x = popmean)) +   # countCause の散布図
  geom_point(aes(y = countCause, shape = "Cause", color = food), size = 3) +   # countCause の散布図
  geom_point(aes(y = counteffect, shape = "Recipietnt", color = food), size = 3) +   # 回帰曲線（counteffect）
  labs(title = "(a)",x = "", y = "Count (Causative / Recipient)") + 
  scale_shape_manual(values = c(16, 17),name = "Interaction type") +
  scale_color_manual(values = c('shrimp-eater' = 'pink', 
                                'omnivore' = 'gray', 
                                'piscivore' = 'red', 
                                'fry-feeder' = 'royalblue1', 
                                'scale-eater' = 'blueviolet', 
                                'grazer' = 'lightgreen', 
                                'browser' = 'darkorange'),
                     guide = "none") + # foodごとに色を設定
  theme(panel.background = element_blank(),   # 背景を透明または白に設定
        plot.background = element_blank(),    # プロットエリアの背景を透明または白に設定
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

#原因側の因果関係数~popmean*food
ggplot(Out_lm_data, aes(x = popmean, y = count)) +
  # 散布図
  geom_point(aes(color = food), size = 3) +
  # 回帰曲線（予測値に基づく）
  geom_line(aes(y = pred_count, color = food), size = 1) +
  # 軸ラベル
  labs(x = "Population mean", y = "Count of Causative interaction") +
  theme_minimal() +
  scale_color_manual(values = c('shrimp-eater' = 'pink', 'omnivore' = 'gray', 'piscivore' = 'red', 'fry-feeder' = 'royalblue1', 'scale-eater' = 'blueviolet', 'grazer' = 'lightgreen', 'browser' = 'darkorange')) # foodごとに色を設定



#結果側の因果関係数~popmean*food

ggplot(In_lm_data, aes(x = popmean)) +
  # countEffect の散布図
  geom_point(aes(y = count, color = food), size = 3) +
  # countEffect の回帰曲線（予測値に基づく）
  geom_line(aes(y = pred_countEffect, color = food), size = 1, linetype = "dashed") +
  # 軸ラベル
  labs(x = "Population mean", y = "Count of recipient interaction") +
  theme_minimal() +
  scale_color_manual(values = c('shrimp-eater' = 'pink', 'omnivore' = 'gray', 'piscivore' = 'red', 'fry-feeder' = 'royalblue1', 'scale-eater' = 'blueviolet', 'grazer' = 'lightgreen', 'browser' = 'darkorange')) # foodごとに色を設定

#媒介中心性~popmean*food

FigS6_b<-ggplot(mergeData_in_out, aes(x = mean,y=Centrality.betweenness,color=Food.habit)) +   # Centrality.betweenness の散布図
  geom_point(aes(y = Centrality.betweenness, color = Food.habit), size = 3,shape=15) +
  labs(title="(b)",x = "Population mean", y = "Centrality betweenness") + 
  scale_color_manual(values = c('Shrimp-Eater' = 'pink', 
                                'Omnivore' = 'gray', 
                                'Piscivore' = 'red', 
                                'Fry-Feeder' = 'royalblue1', 
                                'Scale-Eater' = 'blueviolet', 
                                'Grazer' = 'lightgreen', 
                                'Browser' = 'darkorange')) +  # foodごとに色を設定
  theme(panel.background = element_blank(),   # 背景を透明または白に設定
        plot.background = element_blank(),    # プロットエリアの背景を透明または白に設定
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

FigS6 <- grid.arrange(FigS6_a, FigS6_b, ncol = 1)  # ncol = 1 は縦配置、nrow = 1 なら横配置

ggsave("FigS6.pdf", plot = FigS6, width = 8, height = 6)



