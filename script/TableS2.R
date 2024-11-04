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
interInteraction$rmatch_type[interInteraction$effect_cause %in% hori_foodweb_interaction_with_key$effect_cause] <- "Prey-Predator"

# correlation_Hori_with_key との一致を確認し、該当する行の 'match_type' に "Aggressor-Victim" を記録
interInteraction$match_type[interInteraction$cause_effect %in% correlation_Hori_with_key$cause_effect] <- "Aggressor-Victim"
interInteraction$rmatch_type[interInteraction$effect_cause %in% correlation_Hori_with_key$effect_cause] <- "Victim-Aggressor"

# 両方に一致する場合は "Prey-Predator, Aggressor-Victim" と記録
interInteraction$match_type[interInteraction$cause_effect %in% hori_foodweb_interaction_with_key$cause_effect &
                              interInteraction$cause_effect %in% correlation_Hori_with_key$cause_effect] <- "Predator-Prey, Aggressor-Victim"

interInteraction$rmatch_type[interInteraction$effect_cause %in% hori_foodweb_interaction_with_key$effect_cause &
                              interInteraction$effect_cause %in% correlation_Hori_with_key$effect_cause] <- "Prey-Predator, Victim-Aggressor"

# 結果の表示
print(interInteraction$match_type)
print(interInteraction$rmatch_type)

#cause_effectを入れ替えた文字列を生成
interInteraction$reversed_cause_effect <- NULL
  
for(i in 1:length(interInteraction$cause.x)){
  interInteraction$reversed_cause_effect[i]<-paste(interInteraction$effect.x[i],interInteraction$cause.x[i],sep="_")
}

# reversed_cause_effect と effect_cause が一致するかを確認し、新しい列 match_reversed に TRUE/FALSE を記録
interInteraction$match_reversed <- interInteraction$reversed_cause_effect %in% interInteraction$cause_effect

# 結果の表示
print(interInteraction$match_reversed)

# match_reversed が TRUE かつ strength が "positive" の行を抽出
Pos_pos_data <- interInteraction %>%
  filter(match_reversed == TRUE, strength == "positive")

for(i in 1:nrow(Pos_pos_data)){
  Pos_pos_data$cause.x[i]<-sp_food_coltp0[Pos_pos_data$cause.x[i]==sp_food_coltp0$cause,]$bindre.namesp
  Pos_pos_data$effect.x[i]<-sp_food_coltp0[Pos_pos_data$effect.x[i]==sp_food_coltp0$cause,]$bindre.namesp
}


# 結果の表示
print(Pos_pos_data)
write.csv(Pos_pos_data,"Pos_pos_data.csv")

# match_reversed が TRUE かつ strength が "negative" の行を抽出

Neg_neg_data <- interInteraction %>%
  filter(match_reversed == TRUE, strength == "negative")

for(i in 1:nrow(Neg_neg_data)){
  Neg_neg_data$cause.x[i]<-sp_food_coltp0[Neg_neg_data$cause.x[i]==sp_food_coltp0$cause,]$bindre.namesp
  Neg_neg_data$effect.x[i]<-sp_food_coltp0[Neg_neg_data$effect.x[i]==sp_food_coltp0$cause,]$bindre.namesp
}


# 結果の表示
print(Neg_neg_data)
write.csv(Neg_neg_data,"Neg_neg_data.csv")

Pos_None_data <- interInteraction %>%
  filter(match_reversed == FALSE, strength == "positive")

# match_reversed が FALSE かつ strength が "positive" の行を抽出

#Pos_None_dataはなし
for(i in 1:nrow(Pos_None_data)){
  Pos_None_data$cause.x[i]<-sp_food_coltp0[Pos_None_data$cause.x[i]==sp_food_coltp0$cause,]$bindre.namesp
  Pos_None_data$effect.x[i]<-sp_food_coltp0[Pos_None_data$effect.x[i]==sp_food_coltp0$cause,]$bindre.namesp
}


# 結果の表示
print(Pos_None_data)
write.csv(Pos_None_data,"Pos_None_data.csv")

# match_reversed が FALSE かつ strength が "negative" の行を抽出

Neg_None_data <- interInteraction %>%
  filter(match_reversed == FALSE, strength == "negative")

# 結果の表示

for(i in 1:nrow(Neg_None_data)){
  Neg_None_data$cause.x[i]<-sp_food_coltp0[Neg_None_data$cause.x[i]==sp_food_coltp0$cause,]$bindre.namesp
  Neg_None_data$effect.x[i]<-sp_food_coltp0[Neg_None_data$effect.x[i]==sp_food_coltp0$cause,]$bindre.namesp
}


print(Neg_None_data)
write.csv(Neg_None_data,"Neg_None_data.csv")

#4つのデータで漏れがないか確認
# 4つの抽出結果を結合して除外対象データを作成
# First, filter out matching rows from the first dataset
filtered_data <- interInteraction %>%
  anti_join(Pos_pos_data, by = c("cause.x", "effect.x", "causemeantp0", "effectmeantp0", "causesdtp0", "effectsdtp0", "causehabtp0"))

# Then filter out matching rows from the second dataset
filtered_data <- filtered_data %>%
  anti_join(Neg_neg_data, by = c("cause.x", "effect.x", "causemeantp0", "effectmeantp0", "causesdtp0", "effectsdtp0", "causehabtp0"))

# Finally, filter out matching rows from the third dataset
filtered_data <- filtered_data %>%
  anti_join(Pos_None_data, by = c("cause.x", "effect.x", "causemeantp0", "effectmeantp0", "causesdtp0", "effectsdtp0", "causehabtp0"))

filtered_data <- filtered_data %>%
  anti_join(Neg_None_data, by = c("cause.x", "effect.x", "causemeantp0", "effectmeantp0", "causesdtp0", "effectsdtp0", "causehabtp0"))

# View the resulting filtered data
print(filtered_data)

#Pos-Neg,Neg-Posのようなデータはいないのか？の確認
# 元のデータから除外対象データを取り除いたデータを作成
filtered_data <- anti_join(interInteraction, filtered_data)


