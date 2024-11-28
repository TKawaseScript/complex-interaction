library(igraph)
library(dplyr)
library(stringr)
library(ggrepel)
library(ggplot2)
library(ape)
library(geiger)
library(nlme)
library(phytools)
library(patchwork)
library(Cairo)
library(purrr)


PMark <- function(x) {
  if (x < 0.001) {
    pMark <- "***"
  } else if (x >= 0.001 & x < 0.01) {
    pMark <- "**"
  } else if (x >= 0.01 & x < 0.05) {
    pMark <- "*"
  } else {
    pMark <- "ns"
  }
  return(pMark)
}

#Typemiss AL_callipterus
anoleTree <- read.tree("cichlidtree.nwk")

interspecific_interaction<-read.csv("interspecific_interaction.csv",header=T)
sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T,fileEncoding = "UTF-8")

species<-V(graph(t(cbind(interspecific_interaction$cause,interspecific_interaction$effect))))$name
species<-sort(species)


#正負全ての原因側について
causeAllint<-table(interspecific_interaction$cause)
causeAllintName<-sort(names(causeAllint))
#全ての種が網羅されているか確認
setdiff(species, causeAllintName)
causeAllDataframe<-as.data.frame(causeAllint)
colnames(causeAllDataframe)<-c("spName","causeAllCount")
maxCauseAllDataframe<-max(causeAllDataframe$causeAllCount)

#正負全ての結果側について
effectAllint<-table(interspecific_interaction$effect)
effectAllintName<-sort(names(effectAllint))
#全ての種が網羅されているか確認
diffEffectAllintname<-setdiff(species, effectAllintName)
effectAllint<-as.data.frame(effectAllint)
colnames(effectAllint)<-c("spName","Count")
setdiffEffectAllintData<-cbind(diffEffectAllintname,rep(0,length(diffEffectAllintname)))
setdiffEffectAllintData<-as.data.frame(setdiffEffectAllintData)
colnames(setdiffEffectAllintData)<-c("spName","Count")
effectAllDataframe<-rbind(effectAllint,setdiffEffectAllintData)
colnames(effectAllDataframe)<-c("spName","effectAllCount")
effectAllDataframe$effectAllCount<-as.numeric(effectAllDataframe$effectAllCount)
maxeffectAllDataframe<-max(effectAllDataframe$effectAllCount)


#正の原因側について
causePositiveint<-subset(interspecific_interaction,interspecific_interaction$strength=="positive")
causePositiveint<-table(causePositiveint$cause)
causePosintName<-sort(names(causePositiveint))
#全ての種が網羅されているか確認
diffCausePosintname<-setdiff(species, causePosintName)
causePosDataframe<-as.data.frame(causePositiveint)
colnames(causePosDataframe)<-c("spName","Count")
setdiffCausePosintData<-cbind(diffCausePosintname,rep(0,length(diffCausePosintname)))
setdiffCausePosintData<-as.data.frame(setdiffCausePosintData)
colnames(setdiffCausePosintData)<-c("spName","Count")
CausePosDataframe<-rbind(causePosDataframe,setdiffCausePosintData)
colnames(CausePosDataframe)<-c("spName","causePosCount")
CausePosDataframe$causePosCount<-as.numeric(CausePosDataframe$causePosCount)
maxcausePosDataframe<-max(CausePosDataframe$causePosCount)


#負の原因側について
causeNegitiveint<-subset(interspecific_interaction,interspecific_interaction$strength=="negative")
causeNegitiveint<-table(causeNegitiveint$cause)
causeNegintName<-sort(names(causeNegitiveint))
#全ての種が網羅されているか確認
diffCauseNegintname<-setdiff(species, causeNegintName)
causeNegDataframe<-as.data.frame(causeNegitiveint)
colnames(causeNegDataframe)<-c("spName","Count")
setdiffCauseNegintData<-cbind(diffCauseNegintname,rep(0,length(diffCauseNegintname)))
setdiffCauseNegintData<-as.data.frame(setdiffCauseNegintData)
colnames(setdiffCauseNegintData)<-c("spName","Count")
CauseNegDataframe<-rbind(causeNegDataframe,setdiffCauseNegintData)
colnames(CauseNegDataframe)<-c("spName","causeNegCount")
CauseNegDataframe$causeNegCount<-as.numeric(CauseNegDataframe$causeNegCount)
maxcauseNegDataframe<-max(CauseNegDataframe$causeNegCount)


#正の受け手側について
effectPositiveint<-subset(interspecific_interaction,interspecific_interaction$strength=="positive")
effectPositiveint<-table(effectPositiveint$effect)
effectPosintName<-sort(names(effectPositiveint))
#全ての種が網羅されているか確認
diffeffectPosintname<-setdiff(species, effectPosintName)
effectPosDataframe<-as.data.frame(effectPositiveint)
colnames(effectPosDataframe)<-c("spName","Count")
setdiffeffectPosintData<-cbind(diffeffectPosintname,rep(0,length(diffeffectPosintname)))
setdiffeffectPosintData<-as.data.frame(setdiffeffectPosintData)
colnames(setdiffeffectPosintData)<-c("spName","Count")
effectPosDataframe<-rbind(effectPosDataframe,setdiffeffectPosintData)
colnames(effectPosDataframe)<-c("spName","effectPosCount")
effectPosDataframe$effectPosCount<-as.numeric(effectPosDataframe$effectPosCount)
maxeffectPosDataframe<-max(effectPosDataframe$effectPosCount)


#負の受け手について
effectNegitiveint<-subset(interspecific_interaction,interspecific_interaction$strength=="negative")
effectNegitiveint<-table(effectNegitiveint$effect)
effectNegintName<-sort(names(effectNegitiveint))
#全ての種が網羅されているか確認
diffeffectNegintname<-setdiff(species, effectNegintName)
effectNegDataframe<-as.data.frame(effectNegitiveint)
colnames(effectNegDataframe)<-c("spName","Count")
setdiffeffectNegintData<-cbind(diffeffectNegintname,rep(0,length(diffeffectNegintname)))
setdiffeffectNegintData<-as.data.frame(setdiffeffectNegintData)
colnames(setdiffeffectNegintData)<-c("spName","Count")
effectNegDataframe<-rbind(effectNegDataframe,setdiffeffectNegintData)
colnames(effectNegDataframe)<-c("spName","effectNegCount")
effectNegDataframe$effectNegCount<-as.numeric(effectNegDataframe$effectNegCount)
maxeffectNegDataframe<-max(effectNegDataframe$effectNegCount)


#Fig2のためのDataFrameを作成
Fig2DataframeInteractionOnly<-reduce(list(causeAllDataframe,effectAllDataframe,CausePosDataframe,CauseNegDataframe,effectPosDataframe,effectNegDataframe),full_join,by="spName")

Fig2DataframeFoodOnly<-NULL
for(i in 1:nrow(Fig2DataframeInteractionOnly)){
  Fig2DataframeFoodOnly<-c(Fig2DataframeFoodOnly,sp_food_coltp0[Fig2DataframeInteractionOnly$spName[i]==sp_food_coltp0$cause,]$foodhabit7)
}
Fig2DataframeFoodOnly
Fig2DataframeFoodOnly<-str_to_title(Fig2DataframeFoodOnly)

Fig2DataframeColOnly<-NULL
for(i in 1:nrow(Fig2DataframeInteractionOnly)){
  Fig2DataframeColOnly<-c(Fig2DataframeColOnly,sp_food_coltp0[Fig2DataframeInteractionOnly$spName[i]==sp_food_coltp0$cause,]$col7)
}
Fig2DataframeColOnly

Fig2DataframeNameOnly<-NULL
for(i in 1:nrow(Fig2DataframeInteractionOnly)){
  Fig2DataframeNameOnly<-c(Fig2DataframeNameOnly,sp_food_coltp0[Fig2DataframeInteractionOnly$spName[i]==sp_food_coltp0$cause,]$bindre.namesp)
}
Fig2DataframeNameOnly

Fig2Dataframe<-cbind(Fig2DataframeInteractionOnly,Fig2DataframeFoodOnly,Fig2DataframeColOnly)
Fig2Dataframe<-as.data.frame(Fig2Dataframe)
colnames(Fig2Dataframe)<-c("spName","causeAllCount","effectAllCount","causePosCount","causeNegCount","effectPosCount","effectNegCount","foodHabitat","Col")
Fig2Dataframe$spName<-Fig2DataframeNameOnly
maxCount<-max(maxCauseAllDataframe,maxeffectAllDataframe,maxcausePosDataframe,maxcauseNegDataframe,maxeffectPosDataframe,maxeffectNegDataframe)+1


FoodCol <- c(
  "Shrimp-Eater" = "pink",
  "Fry-Feeder" = "royalblue1",
  "Omnivore" = "gray",
  "Piscivore" = "red",
  "Scale-Eater" = "blueviolet",
  "Grazer" = "lightgreen",
  "Browser" = "darkorange"
)



cor_result_a<-cor.test(Fig2Dataframe$causePosCount,Fig2Dataframe$effectPosCount)

# 系統樹のタクソン名を確認
tree_species <- anoleTree$tip.label
print(tree_species)

# データフレームの種名を確認
df_species <- unique(Fig2Dataframe$spName)
print(df_species)

# データフレームと系統樹で異なる種名を特定
noPgls<-setdiff(df_species, tree_species)

pgls_data<-Fig2Dataframe[!Fig2Dataframe$spName %in% noPgls, ]

pgls_result_a <- gls(
  effectPosCount ~ causePosCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_a <- summary(pgls_result_a)

# causePosCount の Value と p-value を抽出
pgls_cause_value_a <- summary_a$tTable["causePosCount", "Value"]
pgls_cause_pvalue_a <- summary_a$tTable["causePosCount", "p-value"]

Fig2_a<-ggplot(data = Fig2Dataframe, aes(x = causePosCount,y=effectPosCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "", y = "Positive input", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(a)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_a$estimate, digits = 2)) * ~ .(PMark(cor_result_a$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_a, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_a))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
           )

cor_result_b<-cor.test(Fig2Dataframe$causeNegCount,Fig2Dataframe$effectPosCount)

pgls_result_b <- gls(
  effectPosCount ~ causeNegCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_b <- summary(pgls_result_b)

# causePosCount の Value と p-value を抽出
pgls_cause_value_b <- summary_b$tTable["causeNegCount", "Value"]
pgls_cause_pvalue_b <- summary_b$tTable["causeNegCount", "p-value"]




Fig2_b<-ggplot(data = Fig2Dataframe, aes(x = causeNegCount,y=effectPosCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "", y = "", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(b)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_b$estimate, digits = 2)) * ~ .(PMark(cor_result_b$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_b, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_b))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )


cor_result_c<-cor.test(Fig2Dataframe$causeAllCount,Fig2Dataframe$effectPosCount)


pgls_result_c <- gls(
  effectPosCount ~ causeAllCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_c <- summary(pgls_result_c)

# causePosCount の Value と p-value を抽出
pgls_cause_value_c <- summary_c$tTable["causeAllCount", "Value"]
pgls_cause_pvalue_c <- summary_c$tTable["causeAllCount", "p-value"]



Fig2_c<-ggplot(data = Fig2Dataframe, aes(x = causeAllCount,y=effectPosCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "", y = "", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(c)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_c$estimate, digits = 2)) * ~ .(PMark(cor_result_c$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_c, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_c))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )

cor_result_d<-cor.test(Fig2Dataframe$causePosCount,Fig2Dataframe$effectNegCount)


pgls_result_d <- gls(
  effectNegCount ~ causePosCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_d <- summary(pgls_result_d)

# causePosCount の Value と p-value を抽出
pgls_cause_value_d <- summary_d$tTable["causePosCount", "Value"]
pgls_cause_pvalue_d <- summary_d$tTable["causePosCount", "p-value"]



Fig2_d<-ggplot(data = Fig2Dataframe, aes(x = causePosCount,y=effectNegCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "", y = "Negative input", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(d)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_d$estimate, digits = 2)) * ~ .(PMark(cor_result_d$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_d, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_d))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )


cor_result_e<-cor.test(Fig2Dataframe$causeNegCount,Fig2Dataframe$effectNegCount)

pgls_result_e <- gls(
  effectNegCount ~ causeNegCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_e <- summary(pgls_result_e)

# causePosCount の Value と p-value を抽出
pgls_cause_value_e <- summary_e$tTable["causeNegCount", "Value"]
pgls_cause_pvalue_e <- summary_e$tTable["causeNegCount", "p-value"]




Fig2_e<-ggplot(data = Fig2Dataframe, aes(x = causeNegCount,y=effectNegCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "", y = "", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
    xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(e)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_e$estimate, digits = 2)) * ~ .(PMark(cor_result_e$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_e, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_e))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )



cor_result_f<-cor.test(Fig2Dataframe$causeAllCount,Fig2Dataframe$effectNegCount)


pgls_result_f <- gls(
  effectNegCount ~ causeAllCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_f <- summary(pgls_result_f)

# causePosCount の Value と p-value を抽出
pgls_cause_value_f <- summary_f$tTable["causeAllCount", "Value"]
pgls_cause_pvalue_f <- summary_f$tTable["causeAllCount", "p-value"]

Fig2_f<-ggplot(data = Fig2Dataframe, aes(x = causeAllCount,y=effectNegCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "", y = "", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(f)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_f$estimate, digits = 2)) * ~ .(PMark(cor_result_f$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_f, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_f))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )

cor_result_g<-cor.test(Fig2Dataframe$causePosCount,Fig2Dataframe$effectAllCount)

pgls_result_g <- gls(
  effectAllCount ~ causePosCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_g <- summary(pgls_result_g)

# causePosCount の Value と p-value を抽出
pgls_cause_value_g <- summary_g$tTable["causePosCount", "Value"]
pgls_cause_pvalue_g <- summary_g$tTable["causePosCount", "p-value"]


Fig2_g<-ggplot(data = Fig2Dataframe, aes(x = causePosCount,y=effectAllCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "Positive output", y = "Both input", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(g)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_g$estimate, digits = 2)) * ~ .(PMark(cor_result_g$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_g, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_g))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )




cor_result_h<-cor.test(Fig2Dataframe$causeNegCount,Fig2Dataframe$effectAllCount)

pgls_result_h <- gls(
  effectAllCount ~ causeNegCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_h <- summary(pgls_result_h)

# causePosCount の Value と p-value を抽出
pgls_cause_value_h <- summary_h$tTable["causeNegCount", "Value"]
pgls_cause_pvalue_h <- summary_h$tTable["causeNegCount", "p-value"]


Fig2_h<-ggplot(data = Fig2Dataframe, aes(x = causeNegCount,y=effectAllCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "Negative output", y = "", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(h)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_h$estimate, digits = 2)) * ~ .(PMark(cor_result_h$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_h, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_h))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )



cor_result_i<-cor.test(Fig2Dataframe$causeAllCount,Fig2Dataframe$effectAllCount)

pgls_result_i <- gls(
  effectAllCount ~ causeAllCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_i <- summary(pgls_result_i)

# causePosCount の Value と p-value を抽出
pgls_cause_value_i <- summary_i$tTable["causeAllCount", "Value"]
pgls_cause_pvalue_i <- summary_i$tTable["causeAllCount", "p-value"]


Fig2_i<-ggplot(data = Fig2Dataframe, aes(x = causeAllCount,y=effectAllCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "Both output", y = "", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(i)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_i$estimate, digits = 2)) * ~ .(PMark(cor_result_i$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_i, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_i))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )

cor_result_j<-cor.test(Fig2Dataframe$causePosCount,Fig2Dataframe$effectAllCount)

pgls_result_j <- gls(
  effectAllCount ~ causeNegCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_j <- summary(pgls_result_j)

# causePosCount の Value と p-value を抽出
pgls_cause_value_j <- summary_j$tTable["causeNegCount", "Value"]
pgls_cause_pvalue_j <- summary_j$tTable["causeNegCount", "p-value"]


Fig2_j<-ggplot(data = Fig2Dataframe, aes(x = causePosCount,y=causeNegCount, color = Col)) +
  geom_jitter(size = 3, width = 0.2, height = 0.2) +  
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "Positive output", y = "Negative output", color = "Food Habit") +  # 軸ラベルと凡例タイトル
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
    ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_identity() +
  annotate("text", x = 0, y = maxCount, label ="(j)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_j$estimate, digits = 2)) * ~ .(PMark(cor_result_j$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+
  annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_j, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_j))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )


cor_result_k<-cor.test(Fig2Dataframe$effectPosCount,Fig2Dataframe$effectNegCount)

pgls_result_k <- gls(
  effectNegCount ~ effectPosCount,
  correlation = corBrownian(phy = anoleTree, form = ~ spName), 
  data = pgls_data,
  method = "ML"
)

# モデルのサマリーを取得
summary_k <- summary(pgls_result_k)

# causePosCount の Value と p-value を抽出
pgls_cause_value_k <- summary_k$tTable["effectPosCount", "Value"]
pgls_cause_pvalue_k <- summary_k$tTable["effectPosCount", "p-value"]


Fig2_k<-ggplot(data = Fig2Dataframe, aes(x = effectPosCount, y = effectNegCount, color = as.factor(Col))) +  # as.factorを使用
  geom_jitter(size = 3, width = 0.2, height = 0.2) +
  geom_text_repel(
    aes(label = paste0("~italic(", spName, ")")), 
    parse = TRUE, 
    size = 5, 
    box.padding = 0.5, 
    max.overlaps = Inf,     # 全ラベルを描画
    force = 2,              # ラベルの重なりを少なくする
    nudge_y = 0.15,         # ラベルの配置をわずかに調整
    direction = "both"      # 両方向に調整
  ) +
  labs(x = "Positive input", y = "Negative input", color = "Food Habit") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    legend.position = c(0.8, 0.7)
  ) +
  xlim(-1, maxCount) +
  ylim(-1, maxCount) +
  scale_color_manual(values = unique(FoodCol), breaks = unique(Fig2Dataframe$Col), labels = unique(Fig2Dataframe$foodHabitat)) +  # 色の指定
  annotate("text", x = 0, y = maxCount, label = "(k)", size = 5, hjust = 0, vjust = 1, family = "Times New Roman")+
  annotate(
    "text", 
    x = 8, y = 2, 
    label = bquote(italic(r) == .(round(cor_result_j$estimate, digits = 2)) * ~ .(PMark(cor_result_k$p.value))), 
    size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )+annotate("text", 
           x = 8, y = 1, 
           label = bquote(italic(a) == .(round(pgls_cause_value_k, digits = 2)) * ~ .(PMark(pgls_cause_pvalue_k))), 
           size = 5, hjust = 0, vjust = 1, family = "Times New Roman"
  )

Fig2_a<-Fig2_a +coord_fixed(ratio = 1)
Fig2_b<-Fig2_b +coord_fixed(ratio = 1)
Fig2_c<-Fig2_c +coord_fixed(ratio = 1)
Fig2_d<-Fig2_d +coord_fixed(ratio = 1)
Fig2_e<-Fig2_e +coord_fixed(ratio = 1)
Fig2_f<-Fig2_f +coord_fixed(ratio = 1)
Fig2_g<-Fig2_g +coord_fixed(ratio = 1)
Fig2_h<-Fig2_h +coord_fixed(ratio = 1)
Fig2_i<-Fig2_i +coord_fixed(ratio = 1)
Fig2_j<-Fig2_j +coord_fixed(ratio = 1)
Fig2_k<-Fig2_k +coord_fixed(ratio = 1)



Fig2<-Fig2_a + Fig2_b + Fig2_c +
  Fig2_d + Fig2_e + Fig2_f +
  Fig2_g + Fig2_h + Fig2_i +
  Fig2_j + Fig2_k + plot_layout(ncol=3)

CairoPDF("Fig2.pdf", width = 8.27, height = 11.69)
print(Fig2)
dev.off()
