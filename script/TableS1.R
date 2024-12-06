library(dplyr)
library(tidyr)
library(tools)

TimeSeries<-read.csv("TimeSeries_40.csv",header=T)

sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T,fileEncoding = "UTF-8")
#C_furciferとP_paradoxusはHoriObservationでは出てこない(Hori 1993の行動観察には出てくる？)
sp_food_coltp0 <- sp_food_coltp0 %>%
  filter(!is.na(No.))

All_sp_list<-read.csv("all_species.csv",header=T)
cantLinearSPList<-read.csv("cantLinearSPList.csv",header=T)
cantEmbedSPList<-read.csv("cantEmbedSPList.csv",header=T)
less10_pop_sp<-read.csv("less10popsp.csv",header=T)
between<-read.csv("betweenness.csv",header=T)
colnames(between)<-c("Species.name","Centrality betweenness")

between$`Centrality betweenness`<-round(between$`Centrality betweenness`,digits = 2)

#種名の表記名を統一
All_sp_list$x<-gsub("Cunningtonia_longiventralis","C_longiventralis",All_sp_list$x)
All_sp_list$x<-gsub("Mastacembelus_moorii","M_moorii",All_sp_list$x)
All_sp_list$x<-gsub("Synodontis_multipunctatus","S_multipunctatus",All_sp_list$x)
All_sp_list$x<-gsub("Variabilichromis_moorii","V_moorii",All_sp_list$x)
All_sp_list$x<-gsub("Xenotilapia_flavipinnis","X_flavipinnis",All_sp_list$x)
All_sp_list$x<-gsub("Xenotilapia_boulengeri","X_boulengeri",All_sp_list$x)
All_sp_list$x<-gsub("Xenotilapia_sp","X_sp",All_sp_list$x)

#9個体以下のもの
#種名の表記名を統一
less10_sp<-gsub("Variabilichromis_moorii","V_moorii",less10_pop_sp$x)
less10_sp<-gsub("Cunningtonia_longiventralis","C_longiventralis",less10_sp)
less10_sp<-gsub("Xenotilapia_flavipinnis","X_flavipinnis",less10_sp)
less10_sp<-gsub("Xenotilapia_boulengeri","X_boulengeri",less10_sp)
less10_sp<-gsub("Xenotilapia_sp","X_sp",less10_sp)

TimeSeries_mean<-apply(TimeSeries,MARGIN=2,mean)
names(TimeSeries_mean)<-gsub("Cunningtonia_longiventralis","C_longiventralis",names(TimeSeries_mean))
names(TimeSeries_mean)<-gsub("Synodontis_multipunctatus","S_multipunctatus",names(TimeSeries_mean))
names(TimeSeries_mean)<-gsub("Variabilichromis_moorii","V_moorii",names(TimeSeries_mean))
names(TimeSeries_mean)<-gsub("Xenotilapia_flavipinnis","X_flavipinnis",names(TimeSeries_mean))
names(TimeSeries_mean)<-gsub("Xenotilapia_boulengeri","X_boulengeri",names(TimeSeries_mean))
names(TimeSeries_mean)<-gsub("Xenotilapia_sp","X_sp",names(TimeSeries_mean))


TimeSeries_mean_df <- data.frame(
  Species.name = names(TimeSeries_mean),
  mean = round(as.numeric(TimeSeries_mean),digits = 1)
)

TimeSeries_sd<-apply(TimeSeries,MARGIN=2,sd)
names(TimeSeries_sd)<-gsub("Cunningtonia_longiventralis","C_longiventralis",names(TimeSeries_sd))
names(TimeSeries_sd)<-gsub("Synodontis_multipunctatus","S_multipunctatus",names(TimeSeries_sd))
names(TimeSeries_sd)<-gsub("Variabilichromis_moorii","V_moorii",names(TimeSeries_sd))
names(TimeSeries_sd)<-gsub("Xenotilapia_flavipinnis","X_flavipinnis",names(TimeSeries_sd))
names(TimeSeries_sd)<-gsub("Xenotilapia_boulengeri","X_boulengeri",names(TimeSeries_sd))
names(TimeSeries_sd)<-gsub("Xenotilapia_sp","X_sp",names(TimeSeries_sd))

TimeSeries_sd_df <- data.frame(
  Species.name = names(TimeSeries_sd),
  sd = round(as.numeric(TimeSeries_sd),digits = 1)
)

TableS1Basedata<-data.frame("Species name"=sp_food_coltp0$cause,"Tribe"=sp_food_coltp0$tribe,"Abbreviation"=sp_food_coltp0$bindre.namesp,"Food habit"=sp_food_coltp0$foodhabit7)

TableS1data <- TableS1Basedata %>%
  left_join(TimeSeries_mean_df, by = "Species.name") %>%
  left_join(TimeSeries_sd_df, by = "Species.name") %>%
  mutate(
    `Reasons to exclude from CCM` = case_when(
      Species.name %in% less10_pop_sp$x ~ "Max. N < 10",
      Species.name %in% cantEmbedSPList$x ~ "Projection failed",
      Species.name %in% cantLinearSPList$x ~ "Linearity detected",
      TRUE ~ "(Passed)"
    )
  )%>%
  left_join(between, by = "Species.name")%>%
  mutate(`Centrality betweenness` = as.character(`Centrality betweenness`)) %>%
  mutate(`Centrality betweenness` = replace_na(`Centrality betweenness`, "-"))


for(i in 1:nrow(TableS1data)){
  TableS1data$Species.name[i]<-sp_food_coltp0[TableS1data$Species.name==sp_food_coltp0$cause[i],]$SpFullName
}

TableS1data$Food.habit<-toTitleCase(TableS1data$Food.habit)

write.csv(TableS1data,"TableS1.csv")

