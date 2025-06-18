library(ggplot2)
library(gridExtra)
library(renv)
library(dplyr)
library(purrr)
library(tidyverse)

renv::restore()

strengthtp0<-read.csv("GLMbasedatatp0.csv",header=T)[,-c(1,9:11)]
sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T)


#(a)spName:種間相互作用の原因種の名前、RecipientFood、RecipientCol：その種間相互用の受け手側の食性と色
PosList<-strengthtp0[strengthtp0$strength=="positive",]
NegList<-strengthtp0[strengthtp0$strength=="negative",]

intraInteractionPos<-NULL
interInteractionPos<-NULL
for(i in 1:nrow(PosList)){
  if (PosList$cause.x[i]==PosList$effect.x[i]){
    intraInteractionPos <- rbind(intraInteractionPos,PosList[i,])
  }else{
    interInteractionPos <- rbind(interInteractionPos,PosList[i,])
  }
}

intraInteractionNeg<-NULL
interInteractionNeg<-NULL
for(i in 1:nrow(NegList)){
  if (NegList$cause.x[i]==NegList$effect.x[i]){
    intraInteractionNeg <- rbind(intraInteractionNeg,NegList[i,])
  }else{
    interInteractionNeg <- rbind(interInteractionNeg,NegList[i,])
  }
}

PosListRecFood<-NULL
for(i in 1:nrow(interInteractionPos)){
  PosListRecFood[i]<-sp_food_coltp0[interInteractionPos$effect.x[i]==sp_food_coltp0$cause,]$foodhabit7
}
PosListRecCol<-NULL
for(i in 1:nrow(interInteractionPos)){
  PosListRecCol[i]<-sp_food_coltp0[interInteractionPos$effect.x[i]==sp_food_coltp0$cause,]$col7
}

PosListCause<-data.frame("spName"=interInteractionPos$cause.x,"RecipientFood"=PosListRecFood,"RecipientCol"=PosListRecCol)

NegListRecFood<-NULL
for(i in 1:nrow(interInteractionNeg)){
  NegListRecFood[i]<-sp_food_coltp0[interInteractionNeg$effect.x[i]==sp_food_coltp0$cause,]$foodhabit7
}

NegListRecCol<-NULL
for(i in 1:nrow(interInteractionNeg)){
  NegListRecCol[i]<-sp_food_coltp0[interInteractionNeg$effect.x[i]==sp_food_coltp0$cause,]$col7
}

NegListCause<-data.frame("spName"=interInteractionNeg$cause.x,"RecipientFood"=NegListRecFood,"RecipientCol"=NegListRecCol)
# PosListCauseとNegListCauseを結合
PosListCause$Category <- "Positive"
NegListCause$Category <- "Negative"

# 両方のデータを結合
combined_cause_data <- rbind(PosListCause, NegListCause)

# 負の値のRecipientFoodに対して積み上げ方向を反転
combined_cause_data$Value <- ifelse(combined_cause_data$Category == "Negative", -1, 1)

# RecipientFoodを因子型に変換
combined_cause_data$RecipientFood <- as.factor(combined_cause_data$RecipientFood)

#spNameを3文字ルールに変更
for(i in 1:nrow(combined_cause_data)){
  combined_cause_data$rename[i]<-sp_food_coltp0[combined_cause_data$spName[i]==sp_food_coltp0$cause,]$bindre.namesp
}

cau_order<-c("Lam.cal","Aul.dew","Lep.att","Per.mic","Gna.pfe","Lob.lab","Neo.sex","Alt.com","Lep.elo","Lam.lem","Neo.fas","Var.moo","Lam.tan","Xen.pap","Oph.ven","Lim.dar","Int.loo","Jul.orn","Par.spp","Hap.mic","Pet.tre","Pet.pol","Syn.mul","Tro.moo","Tel.vit","Tel.tem")

cum_order<-c("fry-feeder","scale-eater","shrimp-eater","piscivore","omnivore","grazer","browser")

combined_cause_data$RecipientFood<-factor(combined_cause_data$RecipientFood,levels=cum_order)

combined_cause_data$rename<-factor(combined_cause_data$rename, levels = cau_order)

background_rects_cause <- data.frame(
  xmin = c(0, 3.5,4.5,8.5,11.5,20.5,23.5),
  xmax = c(3.5, 4.5,8.5,11.5,20.5,23.5,27),
  fill_color = c("royalblue1", "blueviolet","pink","red","gray","lightgreen","darkorange")
)

FigS4a<-ggplot(combined_cause_data, aes(x = rename, fill = RecipientFood)) +
  geom_bar(aes(y = Value), stat = "identity", position = "stack") + 
  scale_fill_manual(
    values = c(
      "omnivore" = "gray", 
      "piscivore" = "red", 
      "fry-feeder" = "royalblue1", 
      "shrimp-eater" = "pink", 
      "browser" = "darkorange", 
      "scale-eater" = "blueviolet", 
      "grazer" = "lightgreen"
    )
  ) +  # 手動で色を指定
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # y = 0 の破線
  scale_y_continuous(breaks = seq(-10, 10, by = 1),
                     limits = c(-8, 8)  # y軸の範囲を手動で指定
  ) +  # y軸のメモリを1ごとに
  labs(title = "(a)",
       x = "Species Name",
       y = "Number of causal relationship",
       fill = "Trophic guild") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"),  # x軸ラベルを縦
    panel.background = element_rect(fill = "white"),  # 背景色を白に設定
    plot.background = element_rect(fill = "white"),   # プロット全体の背景色も白に設定
    axis.line = element_line(color = "black"),  # 軸の線を黒に設定
    panel.grid = element_blank()  # グリッド線を非表示
  )+
  annotate("rect", xmin = 0, xmax = 3.5, ymin = -8, ymax = -7.8,fill = "royalblue1")+
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -8, ymax = -7.8,fill = "blueviolet")+
  annotate("rect", xmin = 4.5, xmax = 8.5, ymin = -8, ymax = -7.8,fill = "pink")+
  annotate("rect", xmin = 8.5, xmax = 11.5, ymin = -8, ymax = -7.8,fill = "red")+
  annotate("rect", xmin = 11.5, xmax = 20.5, ymin = -8, ymax = -7.8,fill = "gray")+
  annotate("rect", xmin = 20.5, xmax = 23.5, ymin = -8, ymax = -7.8,fill = "lightgreen")+
  annotate("rect", xmin = 23.5, xmax = 27, ymin = -8, ymax = -7.8,fill = "darkorange")



#(b)spName:種間相互作用の受け手側の名前、RecipientFood、RecipientCol：その種間相互用の原因側の食性と色

PosListCauFood<-NULL
for(i in 1:nrow(interInteractionPos)){
  PosListCauFood[i]<-sp_food_coltp0[interInteractionPos$cause.x[i]==sp_food_coltp0$cause,]$foodhabit7
}
PosListCauCol<-NULL
for(i in 1:nrow(interInteractionPos)){
  PosListCauCol[i]<-sp_food_coltp0[interInteractionPos$cause.x[i]==sp_food_coltp0$cause,]$col7
}

PosListRec<-data.frame("spName"=interInteractionPos$effect.x,"CauseFood"=PosListCauFood,"CauseCol"=PosListCauCol)

NegListCauFood<-NULL
for(i in 1:nrow(interInteractionNeg)){
  NegListCauFood[i]<-sp_food_coltp0[interInteractionNeg$cause.x[i]==sp_food_coltp0$cause,]$foodhabit7
}
NegListCauCol<-NULL
for(i in 1:nrow(interInteractionNeg)){
  NegListCauCol[i]<-sp_food_coltp0[interInteractionNeg$cause.x[i]==sp_food_coltp0$cause,]$col7
}

NegListRec<-data.frame("spName"=interInteractionNeg$effect.x,"CauseFood"=NegListCauFood,"CauseCol"=NegListCauCol)
# PosListCauseとNegListCauseを結合
PosListRec$Category <- "Positive"
NegListRec$Category <- "Negative"

# 両方のデータを結合
combined_recipient_data <- rbind(PosListRec, NegListRec)

# 負の値のRecipientFoodに対して積み上げ方向を反転
combined_recipient_data$Value <- ifelse(combined_recipient_data$Category == "Negative", -1, 1)

# RecipientFoodを因子型に変換
combined_recipient_data$CauseFood <- as.factor(combined_recipient_data$CauseFood)

#spNameを3文字ルールに変更
for(i in 1:nrow(combined_recipient_data)){
  combined_recipient_data$rename[i]<-sp_food_coltp0[combined_recipient_data$spName[i]==sp_food_coltp0$cause,]$bindre.namesp
}

rec_order<-c("Lam.cal","Aul.dew","Lep.att","Per.mic","Lob.lab","Neo.sex","Alt.com","Lep.elo","Lam.lem","Neo.fas","Var.moo","Lam.tan","Xen.pap","Oph.ven","Lim.dar","Int.loo","Jul.orn","Par.spp","Pet.tre","Pet.pol","Syn.mul","Tro.moo","Tel.vit","Tel.tem")

combined_recipient_data$rename<-factor(combined_recipient_data$rename, levels = rec_order)

combined_recipient_data$CauseFood<-factor(combined_recipient_data$CauseFood,levels=cum_order)


FigS4b<-ggplot(combined_recipient_data, aes(x = rename, fill = CauseFood)) +
  geom_bar(aes(y = Value), stat = "identity", position = "stack") + 
  scale_fill_manual(
    values = c(
      "omnivore" = "gray", 
      "piscivore" = "red", 
      "fry-feeder" = "royalblue1", 
      "shrimp-eater" = "pink", 
      "browser" = "darkorange", 
      "scale-eater" = "blueviolet", 
      "grazer" = "lightgreen"
    )
  ) +  # 手動で色を指定
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # y = 0 の破線
  scale_y_continuous(breaks = seq(-10, 10, by = 1),
                     limits = c(-8, 8)  # y軸の範囲を手動で指定
  ) +  # y軸のメモリを1ごとに
  labs(title = "(b)",
       x = "",
       y = "Number of recipient relationship",
       fill = "Trophic guild") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"),  # x軸ラベルを縦
    panel.background = element_rect(fill = "white"),  # 背景色を白に設定
    plot.background = element_rect(fill = "white"),   # プロット全体の背景色も白に設定
    axis.line = element_line(color = "black"),  # 軸の線を黒に設定
    panel.grid = element_blank()  # グリッド線を非表示
  )+
  annotate("rect", xmin = 0, xmax = 3.5, ymin = -8, ymax = -7.8,fill = "royalblue1")+
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -8, ymax = -7.8,fill = "blueviolet")+
  annotate("rect", xmin = 4.5, xmax = 7.5, ymin = -8, ymax = -7.8,fill = "pink")+
  annotate("rect", xmin = 7.5, xmax = 10.5, ymin = -8, ymax = -7.8,fill = "red")+
  annotate("rect", xmin = 10.5, xmax = 18.5, ymin = -8, ymax = -7.8,fill = "gray")+
  annotate("rect", xmin = 18.5, xmax = 21.5, ymin = -8, ymax = -7.8,fill = "lightgreen")+
  annotate("rect", xmin = 21.5, xmax = 25, ymin = -8, ymax = -7.8,fill = "orange")
  

pdf("FigS4.pdf",width = 11.9, height = 8.27)
grid.arrange(FigS4a, FigS4b, ncol = 2)
dev.off()
library(dplyr)

# サンプルデータ
sp_food_coltp0 <- data.frame(
  cause = c("A_compressiceps", "A_dewindti", "Cyathopharynx", "C_horei", "Cyprichromissp", "C_longiventralis", "E_cyanostictus", "G_pfefferi"),
  foodhabit7 = c("shrimp-eater", "fry-feeder", "omnivore", "fry-feeder", "omnivore", "nodata", "grazer", "shrimp-eater"),
  bindre.namesp = c("Alt.com", "Aul.dew", "Cya.pha", "C.hor", "Cyp.ssp", "C.lon", "E.cya", "Gna.pfe")
)

combined_cause_data <- data.frame(
  spName = c("A_compressiceps", "A_compressiceps", "A_dewindti", "A_dewindti", "A_dewindti", "A_dewindti", "A_dewindti", "G_pfefferi", "G_pfefferi", "G_pfefferi", "G_pfefferi"),
  RecipientFood = c("omnivore", "piscivore", "fry-feeder", "piscivore", "shrimp-eater", "omnivore", "browser", "shrimp-eater", "piscivore", "shrimp-eater", "omnivore"),
  Category = rep("Positive", 11),
  rename = c("Alt.com", "Alt.com", "Aul.dew", "Aul.dew", "Aul.dew", "Aul.dew", "Aul.dew", "Gna.pfe", "Gna.pfe", "Gna.pfe", "Gna.pfe")
)

# ギルドごとのリスト作成
guildList <- split(sp_food_coltp0$bindre.namesp, sp_food_coltp0$foodhabit7)
guild_order <- c("fry-feeder", "scale-eater", "shrimp-eater", "piscivore", "omnivore", "grazer", "browser","nodata")

# 統計計算
res_cause <- lapply(names(guildList), function(guild) {
  rename_values <- guildList[[guild]]
  df_cause <- combined_cause_data %>% filter(rename %in% rename_values)
  
  matched <- df_cause %>% filter(RecipientFood == guild)
  unmatched <- df_cause %>% filter(RecipientFood != guild)
  
  matched_pos <- sum(matched$Category == "Positive")
  matched_neg <- sum(matched$Category == "Negative")
  unmatched_pos <- sum(unmatched$Category == "Positive")
  unmatched_neg <- sum(unmatched$Category == "Negative")
  
  total <- matched_pos + matched_neg + unmatched_pos + unmatched_neg
  
  tibble(
    Guild = guild,
    Matched_Pos = matched_pos,
    Matched_Neg = matched_neg,
    Unmatched_Pos = unmatched_pos,
    Unmatched_Neg = unmatched_neg,
    Total = total,
    Matched_Pos_Ratio = ifelse((matched_pos + matched_neg) > 0, round(matched_pos / (matched_pos + matched_neg), 3), 0),
    Matched_Neg_Ratio = ifelse((matched_pos + matched_neg) > 0, round(matched_neg / (matched_pos + matched_neg), 3), 0),
    Unmatched_Pos_Ratio = ifelse((unmatched_pos + unmatched_neg) > 0, round(unmatched_pos / (unmatched_pos + unmatched_neg), 3), 0),
    Unmatched_Neg_Ratio = ifelse((unmatched_pos + unmatched_neg) > 0, round(unmatched_neg / (unmatched_pos + unmatched_neg), 3), 0)
  )
}) %>% bind_rows()

# 列順を調整（Totalを6列目に）
res_cause <- res_cause %>%
  select(Guild, Matched_Pos, Matched_Neg, Unmatched_Pos, Unmatched_Neg, Total,
         Matched_Pos_Ratio, Matched_Neg_Ratio, Unmatched_Pos_Ratio, Unmatched_Neg_Ratio)

# 統計計算
res_cause <- lapply(names(guildList), function(guild) {
  rename_values <- guildList[[guild]]
  df_cause <- combined_cause_data %>% filter(rename %in% rename_values)
  
  matched <- df_cause %>% filter(RecipientFood == guild)
  unmatched <- df_cause %>% filter(RecipientFood != guild)
  
  matched_pos <- sum(matched$Category == "Positive")
  matched_neg <- sum(matched$Category == "Negative")
  unmatched_pos <- sum(unmatched$Category == "Positive")
  unmatched_neg <- sum(unmatched$Category == "Negative")
  
  total <- matched_pos + matched_neg + unmatched_pos + unmatched_neg
  
  tibble(
    Guild = guild,
    Matched_Pos = matched_pos,
    Matched_Neg = matched_neg,
    Unmatched_Pos = unmatched_pos,
    Unmatched_Neg = unmatched_neg,
    Total = total,
    Matched_Pos_Ratio = round(matched_pos / total, 2),
    Matched_Neg_Ratio = round(matched_neg / total, 2),
    Unmatched_Pos_Ratio =round(unmatched_pos / total, 2),
    Unmatched_Neg_Ratio = round(unmatched_neg / total, 2)
  )
}) %>% bind_rows()

# 列順を調整（Totalを6列目に）
res_cause <- res_cause %>%
  select(Guild, Matched_Pos, Matched_Neg, Unmatched_Pos, Unmatched_Neg, Total,
         Matched_Pos_Ratio, Matched_Neg_Ratio, Unmatched_Pos_Ratio, Unmatched_Neg_Ratio)

# 統計計算
res_recipient <- lapply(names(guildList), function(guild) {
  rename_values <- guildList[[guild]]
  df_recipient <- combined_recipient_data %>% filter(rename %in% rename_values)
  
  matched <- df_recipient %>% filter(CauseFood == guild)
  unmatched <- df_recipient %>% filter(CauseFood != guild)
  
  matched_pos <- sum(matched$Category == "Positive")
  matched_neg <- sum(matched$Category == "Negative")
  unmatched_pos <- sum(unmatched$Category == "Positive")
  unmatched_neg <- sum(unmatched$Category == "Negative")
  
  total <- matched_pos + matched_neg + unmatched_pos + unmatched_neg
  
  tibble(
    Guild = guild,
    Matched_Pos = matched_pos,
    Matched_Neg = matched_neg,
    Unmatched_Pos = unmatched_pos,
    Unmatched_Neg = unmatched_neg,
    Total = total,
    Matched_Pos_Ratio = round(matched_pos / total, 2),
    Matched_Neg_Ratio = round(matched_neg / total, 2),
    Unmatched_Pos_Ratio = round(unmatched_pos / total, 2),
    Unmatched_Neg_Ratio = round(unmatched_neg / total, 2)
  )
}) %>% bind_rows()

# 列順を調整（Totalを6列目に）
res_recipient <- res_recipient %>%
  select(Guild, Matched_Pos, Matched_Neg, Unmatched_Pos, Unmatched_Neg, Total,
         Matched_Pos_Ratio, Matched_Neg_Ratio, Unmatched_Pos_Ratio, Unmatched_Neg_Ratio)

res_cause<-res_cause %>%
  mutate(Guild = factor(Guild, levels = guild_order)) %>%
  arrange(Guild)

res_recipient<-res_recipient %>%
  mutate(Guild = factor(Guild, levels = guild_order)) %>%
  arrange(Guild)


write.csv(res_cause,"guild_match_stats_cause.csv")
write.csv(res_recipient,"recipient_data_guild_match_stats.csv")


res_cause_long <- res_cause %>%
  filter(Guild != "nodata") %>%
  select(-Total) %>%
  pivot_longer(
    cols = c(Matched_Pos, Matched_Neg, Unmatched_Pos, Unmatched_Neg),
    names_to = "Category",
    values_to = "Count"
  )


res_cause_long$Category <- factor(
  res_cause_long$Category,
  levels = c("Matched_Pos", "Matched_Neg", "Unmatched_Pos", "Unmatched_Neg"),
  labels = c("Intraguild Positive", "Intraguild Negative", "Interguild Positive", "Interguild Negative")
)

TableS6Figa<-ggplot(res_cause_long, aes(x = Guild, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "Intraguild Positive" = "skyblue",   # 青
      "Intraguild Negative" = "pink",   # オレンジ
      "Interguild Positive" = "blue", # 緑
      "Interguild Negative" = "red"  # 赤
    )
  ) +
  labs(
    title = "(a)",
    x = "Guild",
    y = "Count"
  ) +
  theme_minimal() +
  ylim(0,25)+
  xlab("Trophic guild")+
  ylab("Number of causal relationship")+ 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"),  # x軸ラベルを縦
    panel.background = element_rect(fill = "white"),  # 背景色を白に設定
    plot.background = element_rect(fill = "white"),   # プロット全体の背景色も白に設定
    axis.line = element_line(color = "black"),  # 軸の線を黒に設定
    panel.grid = element_blank()  # グリッド線を非表示
  )

res_recipient_long <- res_recipient %>%
  filter(Guild != "nodata") %>%
  select(-Total) %>%
  pivot_longer(
    cols = c(Matched_Pos, Matched_Neg, Unmatched_Pos, Unmatched_Neg),
    names_to = "Category",
    values_to = "Count"
  )

res_recipient_long$Category <- factor(
  res_recipient_long$Category,
  levels = c("Matched_Pos", "Matched_Neg", "Unmatched_Pos", "Unmatched_Neg"),
  labels = c("Intraguild Positive", "Intraguild Negative", "Interguild Positive", "Interguild Negative")
)

TableS6Figb<-ggplot(res_recipient_long, aes(x = Guild, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "Intraguild Positive" = "skyblue",   # 青
      "Intraguild Negative" = "pink",   # オレンジ
      "Interguild Positive" = "blue", # 緑
      "Interguild Negative" = "red"  # 赤
    )
  ) +
  labs(
    title = "(b)",
    x = "Guild",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,25)+
  xlab("Trophic guild")+
  ylab("Number of recipient relationship")+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"),  # x軸ラベルを縦
    panel.background = element_rect(fill = "white"),  # 背景色を白に設定
    plot.background = element_rect(fill = "white"),   # プロット全体の背景色も白に設定
    axis.line = element_line(color = "black"),  # 軸の線を黒に設定
    panel.grid = element_blank()  # グリッド線を非表示
  )



pdf("TableS6Fig.pdf", width = 11.7, height = 8.3) # A4 landscape in inches
grid.arrange(TableS6Figa, TableS6Figb, ncol = 2)
dev.off()


