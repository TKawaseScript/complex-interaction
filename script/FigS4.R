library(ggplot2)
library(gridExtra)


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

combined_cause_data$rename<-factor(combined_cause_data$rename, levels = cau_order)


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
                     limits = c(-10, 10)  # y軸の範囲を手動で指定
  ) +  # y軸のメモリを1ごとに
  labs(title = "(a)",
       x = "Species Name",
       y = "Number of causal relationship",
       fill = "Food Habitat") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"),  # x軸ラベルを縦
    panel.background = element_rect(fill = "white"),  # 背景色を白に設定
    plot.background = element_rect(fill = "white"),   # プロット全体の背景色も白に設定
    axis.line = element_line(color = "black"),  # 軸の線を黒に設定
    panel.grid = element_blank()  # グリッド線を非表示
  )



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
                     limits = c(-10, 10)  # y軸の範囲を手動で指定
  ) +  # y軸のメモリを1ごとに
  labs(title = "(b)",
       x = "",
       y = "Number of recipient relationship",
       fill = "Food Habitat") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"),  # x軸ラベルを縦
    panel.background = element_rect(fill = "white"),  # 背景色を白に設定
    plot.background = element_rect(fill = "white"),   # プロット全体の背景色も白に設定
    axis.line = element_line(color = "black"),  # 軸の線を黒に設定
    panel.grid = element_blank()  # グリッド線を非表示
  )

pdf("FigS4.pdf",width = 11.9, height = 8.27)
grid.arrange(FigS4a, FigS4b, ncol = 2)
dev.off()

