library(dplyr)

GLMbasedatatp0<-read.csv("GLMbasedatatp0.csv",header=T)[,-1]
correlation_Hori<-read.csv("Hori_aggressive_behaviors.csv")
hori_foodweb_interaction<-read.csv("Hori_Food_web.csv",header=T)
sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T)


intraInteraction<-NULL
interInteraction<-NULL

for(i in 1:nrow(GLMbasedatatp0)){
  if (GLMbasedatatp0$cause.x[i]==GLMbasedatatp0$effect.x[i]){
    intraInteraction<-rbind(intraInteraction,GLMbasedatatp0[i,])
  }else{
    interInteraction<-rbind(interInteraction,GLMbasedatatp0[i,])
  }
}

effect_cause<-paste(interInteraction$effect.x,interInteraction$cause.x,sep="_")

interInteraction$effect_cause<-effect_cause

correlation_Hori_key<-NULL
correlation_Hori_rkey<-NULL
for(i in 1:nrow(correlation_Hori)){
  cause<-sp_food_coltp0[correlation_Hori$cause[i] == sp_food_coltp0$re.namesp,]$cause
  effect<-sp_food_coltp0[correlation_Hori$effect[i] == sp_food_coltp0$re.namesp,]$cause
  correlation_Hori_key<-rbind(correlation_Hori_key,paste(cause,effect,sep="_"))
  correlation_Hori_rkey<-rbind(correlation_Hori_rkey,paste(effect,cause,sep="_"))
}

correlation_Hori_with_key<-cbind(correlation_Hori,correlation_Hori_key,correlation_Hori_rkey)
colnames(correlation_Hori_with_key)<-c("cause","effect","foodhabit","ratio","cause_effect","effect_cause")

hori_foodweb_interaction_key<-NULL
hori_foodweb_interaction_rkey<-NULL
for(i in 1:nrow(hori_foodweb_interaction)){
  cause<-sp_food_coltp0[hori_foodweb_interaction$cause[i] == sp_food_coltp0$re.namesp,]$cause
  effect<-sp_food_coltp0[hori_foodweb_interaction$effect[i] == sp_food_coltp0$re.namesp,]$cause
  hori_foodweb_interaction_key<-rbind(hori_foodweb_interaction_key,paste(cause,effect,sep="_"))
  hori_foodweb_interaction_rkey<-rbind(hori_foodweb_interaction_rkey,paste(effect,cause,sep="_"))
  
}

hori_foodweb_interaction_with_key<-cbind(hori_foodweb_interaction,hori_foodweb_interaction_key,hori_foodweb_interaction_rkey)
colnames(hori_foodweb_interaction_with_key)<-c("cause","effect","foodhabit","ratio","cause_effect","effect_cause")

# interInteractionデータフレームに新しい列 'match_type' を追加
interInteraction$match_type <- "No Match"  # 初期値として "No Match" を設定
interInteraction$rmatch_type <- "No Match"  # 初期値として "No Match" を設定

# hori_foodweb_interaction_with_key との一致を確認し、該当する行の 'match_type' に "Prey-Predator" を記録
interInteraction$match_type[interInteraction$cause_effect %in% hori_foodweb_interaction_with_key$cause_effect] <- "Predator-Prey"
interInteraction$rmatch_type[interInteraction$effect_cause %in% hori_foodweb_interaction_with_key$cause_effect] <- "Prey-Predator"

# correlation_Hori_with_key との一致を確認し、該当する行の 'match_type' に "Aggressor-Victim" を記録
interInteraction$match_type[interInteraction$cause_effect %in% correlation_Hori_with_key$cause_effect] <- "Aggressor-Victim"
interInteraction$rmatch_type[interInteraction$effect_cause %in% correlation_Hori_with_key$cause_effect] <- "Victim-Aggressor"

# 両方に一致する場合は "Prey-Predator, Aggressor-Victim" と記録
interInteraction$match_type[interInteraction$cause_effect %in% hori_foodweb_interaction_with_key$cause_effect &
                              interInteraction$cause_effect %in% correlation_Hori_with_key$cause_effect] <- "Predator-Prey, Aggressor-Victim"

interInteraction$rmatch_type[interInteraction$effect_cause %in% hori_foodweb_interaction_with_key$cause_effect &
                              interInteraction$effect_cause %in% correlation_Hori_with_key$cause_effect] <- "Prey-Predator, Victim-Aggressor"

# 結果の表示
print(interInteraction$match_type)
print(interInteraction$rmatch_type)

# reversed_cause_effect と effect_cause が一致するかを確認し、新しい列 match_reversed に TRUE/FALSE を記録
interInteraction$match_reversed <- interInteraction$effect_cause %in% interInteraction$cause_effect

# 結果の表示
print(interInteraction$match_reversed)

# 結果の表示
View(interInteraction)

TableS2<-interInteraction %>% select(cause.x,effect.x,causehabtp0,min,mean,max,strength,ratio,match_type,rmatch_type,match_reversed)

write.csv(TableS2,"TableS2.csv")

#FigS2
tableS3<-TableS2 %>%
  filter(match_type !="No Match" | rmatch_type !="No Match")

write.csv(tableS3,"TableS3.csv")

