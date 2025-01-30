library(MuMIn)

#TableS6
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

Each_Other_lm_data<-data.frame("count"=degree_dist_All_all_digdis$x,"popmean"=popmeans,"food"=foods)

glm_Each_Other<-glm(count~popmean*food,data=Each_Other_lm_data,family="poisson")

options(na.action = "na.fail")  # 欠損値がある場合にエラーを出す設定

Each_all_models <- dredge(glm_Each_Other)

write.csv(capture.output(Each_all_models),"Each_all_models_num_interact.csv")

# AICが最小のモデルを表示
Each_best_model <- get.models(Each_all_models, subset = 1)[[1]]
summary(Each_best_model)


summary(glm(count~popmean,data=Each_Other_lm_data,family="poisson"))
summary(glm(count~foods,data=Each_Other_lm_data,family="poisson"))

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


Out_all_models <- dredge(glm_Out)
write.csv(capture.output(Out_all_models),"Out_all_models_num_interact.csv")


# AICが最小のモデルを表示
Out_best_model <- get.models(Out_all_models, subset = 1)[[1]]
summary(Out_best_model)


summary(glm(count~popmean,data=Out_lm_data,family="poisson"))
summary(glm(count~foods,data=Out_lm_data,family="poisson"))

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


In_all_models <- dredge(glm_In)

write.csv(capture.output(In_all_models),"In_all_models_num_interact.csv")


# AICが最小のモデルを表示
In_best_model <- get.models(In_all_models, subset = 1)[[1]]
summary(In_best_model)


summary(glm(count~popmean,data=In_lm_data,family="poisson"))
summary(glm(count~food,data=In_lm_data,family="poisson"))



