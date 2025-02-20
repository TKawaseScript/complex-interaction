library(VGAM)
library(dplyr)
library(ggplot2)


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

#igraphから抜き出している場合種名の順番が因果関係の原因側でも結果側でも同じため以下のような式になる
glm_Each_Other_all<-vglm(cbind(Each_Other_lm_data$countCause,Each_Other_lm_data$counteffect)~popmean+food,data=Each_Other_lm_data,family=poissonff)

Each_Other_lm_data$pred_countCause <- predict(glm_Each_Other_all, type = "response")[,1]
Each_Other_lm_data$pred_countEffect <- predict(glm_Each_Other_all, type = "response")[,2]

summary(glm_Each_Other_all)

write.csv(capture.output(summary(glm_Each_Other_all)),"TableS4.csv")

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

Out_lm_data$pred_count <- predict(glm_Out, type = "response")

summary(glm_Out)

write.csv(capture.output(summary(glm_Out)),"Out_all_models_num_interact.csv")



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

In_lm_data$pred_countEffect <- predict(glm_In, type = "response")

summary(glm_In)

write.csv(capture.output(summary(glm_In)),"In_all_models_num_interact.csv")


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

mergeData_in<-merge(glmdataCen[,-1], degree_dist_in_digdis, by = "Abbreviation", all.x = TRUE)


mergeData_in<-merge(glmdataCen, degree_dist_in_digdis, by = "Abbreviation", all.x = TRUE)

mergeData_in_out<-merge(mergeData_in, degree_dist_out_digdis, by = "Abbreviation", all.x = TRUE)

mergeData_in_out$Centrality.betweenness<-as.numeric(mergeData_in_out$Centrality.betweenness)

mergeData_in_out$mean<-as.numeric(mergeData_in_out$mean)

options(na.action="na.fail")
glm_cent<-glm(Centrality.betweenness~mean*Food.habit,data=mergeData_in_out,family="gaussian")

mergeData_in_out$pred_centrality <- predict(glm_cent, type = "response")

write.csv(capture.output(summary(glm_cent)),"TableS5.csv")

#VGAM(双方向の因果関係数~popmean+food)

# グラフの作成
FigS6<-ggplot(Each_Other_lm_data, aes(x = popmean)) +
  # countCause の散布図
  geom_point(aes(y = countCause, shape = "countCause"), color = "black", size = 3) +
  # countEffect の散布図
  geom_point(aes(y = counteffect, shape = "countEffect"), color = "black", size = 3) +
  # 回帰曲線（countCause）
  geom_line(aes(y = pred_countCause, color = food), size = 1) +
  # 回帰曲線（countEffect）
  geom_line(aes(y = pred_countEffect, color = food), size = 1, linetype = "dashed") +
  # 軸ラベル
  labs(x = "Pop Mean", y = "Count (Causative / Recipient)") +
  theme_minimal() +
  scale_shape_manual(values = c(16, 17)) +  # countCauseとcountEffectの形を異なるものに設定
  scale_color_manual(values = c('shrimp-eater' = 'pink', 'omnivore' = 'gray', 'piscivore' = 'red', 'fry-feeder' = 'royalblue1', 'scale-eater' = 'blueviolet', 'grazer' = 'lightgreen', 'browser' = 'darkorange')) # foodごとに色を設定

ggsave("FigS6.pdf", plot = FigS6, width = 21, height = 29.7, units = "cm")

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

FigS7<-ggplot(mergeData_in_out, aes(x = mean)) +
  # Centrality.betweenness の散布図
  geom_point(aes(y = Centrality.betweenness, color = Food.habit), size = 3) +
  # Centrality.betweenness の回帰曲線（予測値に基づく）
  geom_line(aes(y = pred_centrality, color = Food.habit), size = 1) +
  # 軸ラベル
  labs(x = "Population mean", y = "Centrality betweenness") +
  theme_minimal() +
  scale_color_manual(values = c('Shrimp-Eater' = 'pink', 'Omnivore' = 'gray', 'Piscivore' = 'red', 'Fry-Feeder' = 'royalblue1', 'Scale-Eater' = 'blueviolet', 'Grazer' = 'lightgreen', 'Browser' = 'darkorange')) # foodごとに色を設定

ggsave("FigS7.pdf", plot = FigS7, width = 21, height = 29.7, units = "cm")

