library(ggplot2)
library(gridExtra)
library(renv)
library(dplyr)
library(purrr)
library(tidyverse)
library(tidyr)

renv::restore()

strengthtp0<-read.csv("GLMbasedatatp0.csv",header=T)[,-c(1,10:12)]
sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T)

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
combined_cause_data <- rbind(PosListCause, NegListCause)

combined_cause_data$Value <- ifelse(combined_cause_data$Category == "Negative", -1, 1)
combined_cause_data$RecipientFood <- as.factor(combined_cause_data$RecipientFood)

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
  ) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  scale_y_continuous(breaks = seq(-10, 10, by = 1),
                     limits = c(-8, 8) 
  ) + 
  labs(title = "(a)",
       x = "Species Name",
       y = "Number of causal relationship",
       fill = "Trophic guild") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"), 
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),   
    axis.line = element_line(color = "black"), 
    panel.grid = element_blank()  
  )+
  annotate("rect", xmin = 0, xmax = 3.5, ymin = -8, ymax = -7.8,fill = "royalblue1")+
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -8, ymax = -7.8,fill = "blueviolet")+
  annotate("rect", xmin = 4.5, xmax = 8.5, ymin = -8, ymax = -7.8,fill = "pink")+
  annotate("rect", xmin = 8.5, xmax = 11.5, ymin = -8, ymax = -7.8,fill = "red")+
  annotate("rect", xmin = 11.5, xmax = 20.5, ymin = -8, ymax = -7.8,fill = "gray")+
  annotate("rect", xmin = 20.5, xmax = 23.5, ymin = -8, ymax = -7.8,fill = "lightgreen")+
  annotate("rect", xmin = 23.5, xmax = 27, ymin = -8, ymax = -7.8,fill = "darkorange")


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
PosListRec$Category <- "Positive"
NegListRec$Category <- "Negative"
combined_recipient_data <- rbind(PosListRec, NegListRec)
combined_recipient_data$Value <- ifelse(combined_recipient_data$Category == "Negative", -1, 1)
combined_recipient_data$CauseFood <- as.factor(combined_recipient_data$CauseFood)

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
  ) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  scale_y_continuous(breaks = seq(-10, 10, by = 1),
                     limits = c(-8, 8)  
  ) +  
  labs(title = "(b)",
       x = "",
       y = "Number of recipient relationship",
       fill = "Trophic guild") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,face = "italic"), 
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"),  
    axis.line = element_line(color = "black"),  
    panel.grid = element_blank() 
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


sp_food_coltp0 <- data.frame(
  cause = c("A_compressiceps", "A_dewindti", "Cyathopharynx", "C_horei", "Cyprichromissp", "C_longiventralis", "E_cyanostictus", "G_pfefferi"),
  foodhabit7 = c("shrimp-eater", "fry-feeder", "omnivore", "fry-feeder", "omnivore", "nodata", "grazer", "shrimp-eater"),
  bindre.namesp = c("Alt.com", "Aul.dew", "Cya.pha", "C.hor", "Cyp.ssp", "C.lon", "E.cya", "Gna.pfe")
)

guildList <- split(sp_food_coltp0$bindre.namesp, sp_food_coltp0$foodhabit7)
guild_order <- c("fry-feeder", "scale-eater", "shrimp-eater", "piscivore", "omnivore", "grazer", "browser","nodata")
# Cause側の集計
res_cause <- lapply(names(guildList), function(guild) {
  rename_values <- guildList[[guild]]
  df_cause <- combined_cause_data %>% filter(rename %in% rename_values)
  
  matched   <- df_cause %>% filter(RecipientFood == guild)
  unmatched <- df_cause %>% filter(RecipientFood != guild)
  
  matched_pos   <- sum(matched$Category == "Positive")
  matched_neg   <- sum(matched$Category == "Negative")
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
}) %>% bind_rows() %>%
  mutate(Guild = factor(Guild, levels = guild_order)) %>%
  arrange(Guild)

# Recipient species
res_recipient <- lapply(names(guildList), function(guild) {
  rename_values <- guildList[[guild]]
  df_recipient <- combined_recipient_data %>% filter(rename %in% rename_values)
  
  matched   <- df_recipient %>% filter(CauseFood == guild)
  unmatched <- df_recipient %>% filter(CauseFood != guild)
  
  matched_pos   <- sum(matched$Category == "Positive")
  matched_neg   <- sum(matched$Category == "Negative")
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
}) %>% bind_rows() %>%
  mutate(Guild = factor(Guild, levels = guild_order)) %>%
  arrange(Guild)

write.csv(res_cause,"guild_match_stats_cause.csv")
write.csv(res_recipient,"recipient_data_guild_match_stats.csv")

# Making data frame
fisher_df_cause <-res_cause[,c(1:5)]

fisher_df_cause$Matched <- fisher_df_cause$Matched_Pos + fisher_df_cause$Matched_Neg
fisher_df_cause$Unmatched <- fisher_df_cause$Unmatched_Pos + fisher_df_cause$Unmatched_Neg

table_mat_cause <- as.table(rbind(fisher_df_cause$Matched, fisher_df_cause$Unmatched))


colnames(table_mat_cause) <- fisher_df_cause$Guild
rownames(table_mat_cause) <- c("Matched", "Unmatched")

fisher.test(table_mat_cause)

res_cause_long <- res_cause %>%
  mutate(
    Matched_Pos_Ratio = ifelse(is.na(Matched_Pos_Ratio), 0, Matched_Pos_Ratio),
    Unmatched_Pos_Ratio = ifelse(is.na(Unmatched_Pos_Ratio), 0, Unmatched_Pos_Ratio)
  ) %>%
  select(Guild, Matched_Pos_Ratio, Unmatched_Pos_Ratio) %>%
  pivot_longer(cols = c(Matched_Pos_Ratio, Unmatched_Pos_Ratio),
               names_to = "Type", values_to = "Ratio") %>%
  mutate(Type = recode(Type,
                       Matched_Pos_Ratio = "Matched",
                       Unmatched_Pos_Ratio = "Unmatched"))

ggplot(res_cause_long, aes(x = Guild, y = Ratio, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Matched vs Unmatched Ratio by Guild",
       x = "Guild", y = "Ratio", fill = "Type") +
  theme_minimal()


fisher_df_effect <-res_recipient[,c(1:5)]

fisher_df_effect$Matched <- fisher_df_effect$Matched_Pos + fisher_df_effect$Matched_Neg
fisher_df_effect$Unmatched <- fisher_df_effect$Unmatched_Pos + fisher_df_effect$Unmatched_Neg

table_mat_effect <- as.table(rbind(fisher_df_effect$Matched, fisher_df_effect$Unmatched))


colnames(table_mat_effect) <- fisher_df_effect$Guild
rownames(table_mat_effect) <- c("Matched", "Unmatched")

fisher.test(table_mat_effect)

fisher_df_long <- fisher_df_effect %>%
  mutate(
    Total = Matched + Unmatched,
    Matched_Ratio = ifelse(Total > 0, Matched / Total, 0),
    Unmatched_Ratio = ifelse(Total > 0, Unmatched / Total, 0)
  ) %>%
  select(Guild, Matched_Ratio, Unmatched_Ratio) %>%
  pivot_longer(cols = c(Matched_Ratio, Unmatched_Ratio),
               names_to = "Type", values_to = "Ratio") %>%
  mutate(Type = recode(Type,
                       Matched_Ratio = "Matched",
                       Unmatched_Ratio = "Unmatched"))

ggplot(fisher_df_long, aes(x = Guild, y = Ratio, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Matched vs Unmatched Ratio by Guild",
       x = "Guild", y = "Ratio", fill = "Type") +
  theme_minimal()

renv::snapshot()

