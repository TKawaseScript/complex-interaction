library(MuMIn)
library(VGAM)
library(dplyr)

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


summary(glm_Each_Other_all)

write.csv(capture.output(summary(glm_Each_Other_all)),"Each_all_models_num_interact.csv")

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

summary(glm_In)

write.csv(capture.output(summary(glm_In)),"In_all_models_num_interact.csv")


#TableS5 媒介中心生モデルのものも追加して解析を行う
TableS1data<-read.csv("TableS1.csv")

glmAICdataCen <-subset(TableS1data,TableS1data$Centrality.betweenness!= "-")

glmAIC_dataFrame<-data.frame("Abbreviation"=glmAICdataCen$Abbreviation,"centrality"=glmAICdataCen$`Centrality.betweenness`,"popmean"=glmAICdataCen$mean,"food"=glmAICdataCen$Food.habit)

glmAIC_dataFrame_Outname<-NULL
for(i in 1:nrow(degree_dist_out_digdis)){
  glmAIC_dataFrame_Outname<-c(glmAIC_dataFrame_Outname,sp_food_coltp0[degree_dist_out_digdis$X[i]==sp_food_coltp0$cause,]$bindre.namesp)
}

degree_dist_out_digdis$X<-glmAIC_dataFrame_Outname

glmAIC_dataFrame_Inname<-NULL
for(i in 1:nrow(degree_dist_in_digdis)){
  glmAIC_dataFrame_Inname<-c(glmAIC_dataFrame_Inname,sp_food_coltp0[degree_dist_in_digdis$X[i]==sp_food_coltp0$cause,]$bindre.namesp)
}

degree_dist_in_digdis$X<-glmAIC_dataFrame_Inname
colnames(degree_dist_in_digdis)<-c("Abbreviation","in_count")
colnames(degree_dist_out_digdis)<-c("Abbreviation","out_count")

mergeData_in<-merge(glmAICdataCen, degree_dist_in_digdis, by = "Abbreviation", all.x = TRUE)

mergeData_in_out<-merge(mergeData_in, degree_dist_out_digdis, by = "Abbreviation", all.x = TRUE)



#0.000001のような小さい値を足す/
mergeData_in_out$Centrality.betweenness<-as.numeric(mergeData_in_out$Centrality.betweenness)

mergeData_in_out$mean<-as.numeric(mergeData_in_out$mean)

options(na.action="na.fail")
glm_Out_cent<-glm(Centrality.betweenness~mean*Food.habit,data=mergeData_in_out,family="gaussian")

write.csv(capture.output(summary(glm_Out_cent)),"Out_all_models_Cent.csv")


glm_In_cent<-glm(Centrality.betweenness~mean*Food.habit,data=mergeData_in_out,family="gaussian")

summary(glm_In_cent)

write.csv(capture.output(summary(glm_In_cent)),"In_all_models_Cent.csv")


