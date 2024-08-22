
# 元のベクトルを作成

#TimeSeries_40a<-read.csv("TimeSeries_40a.csv",header=T,row.names = 1)

causalitybaseName <- gsub("Cunningtonia_longiventralis","C_longiventralis",colnames(TimeSeries_40a))

causalitybaseName<-gsub("Synodontis_multipunctatus","S_multipunctatus",causalitybaseName)

causalitybaseName<-gsub("Variabilichromis_moorii","V_moorii",causalitybaseName)

causalitybaseName<-gsub("Xenotilapia_flavipinnis","X_flavipinnis",causalitybaseName)

causalitybaseName<-gsub("Xenotilapia_boulengeri","X_boulengeri",causalitybaseName)

causalitybaseName<-gsub("Xenotilapia_sp","X_sp",causalitybaseName)

#number of items to replace is not a multiple of replacement length?
combination<-NULL
for(i in 1:length(causalitybaseName)){
  combination[i]<- sp_food_coltp0[causalitybaseName[i]==sp_food_coltp0$cause,]$re.namesp
}

# 2つのベクトルの全ての組み合わせを生成
combinations <- expand.grid(combination,combination)


combinations <- combinations[combinations$Var1 != combinations$Var2, ]

View(combinations)


causalitylist<-read.csv("listtp0.csv")

Hori_aggressive<-read.csv("Hori_aggressive_behaviors.csv")
Hori_aggressive<-Hori_aggressive[Hori_aggressive$ratio>0,]
Hori_aggressive<-Hori_aggressive[,c(1:2)]

Hori_foodweb<-read.csv("Hori_Food_web_withScaleeating.csv")
Hori_foodweb<-Hori_foodweb[Hori_foodweb$ratio>0,]
Hori_foodweb<-Hori_foodweb[,c(1:2)]


renameCCMcause<-NULL
renameCCMeffect<-NULL
for(i in 1:length(causalitylist$cause)){
  renameCCMcause[i] <- sp_food_coltp0[causalitylist$cause[i]==sp_food_coltp0$cause,]$re.namesp
  renameCCMeffect[i] <- sp_food_coltp0[causalitylist$effect[i]==sp_food_coltp0$cause,]$re.namesp
}

renameCCM<-cbind(renameCCMcause,renameCCMeffect)

renameCCM <- renameCCM[renameCCM[,1] != renameCCM[,2],]

# renameCCM の各行と combinations の各行を比較し、一致する行を特定
matching_rows_indices_CCM_comb <- numeric(0)
for (i in 1:nrow(combinations)) {
  for (j in 1:nrow(renameCCM)) {
    if (all(combinations[i,] == renameCCM[j,])) {
      matching_rows_indices_CCM_comb <- c(matching_rows_indices_CCM_comb, i)
    }
  }
}

#総当たりリストからCCMのものを抽出
combinations[matching_rows_indices_CCM_comb,]

CCMmatchList<-rep(NA,nrow(combinations))

CCMmatchList[matching_rows_indices_CCM_comb]<-"present"

#Horiaggressive の各行と combinations の各行を比較し、一致する行を特定
matching_rows_indices_agg_comb <- numeric(0)
for (i in 1:nrow(combinations)) {
  for (j in 1:nrow(Hori_aggressive)) {
    if (all(combinations[i, ] == Hori_aggressive[j, ])) {
      matching_rows_indices_agg_comb <- c(matching_rows_indices_agg_comb, i)
    }
  }
}

combinations[matching_rows_indices_agg_comb,]


AggmatchList<-rep(NA,nrow(combinations))

AggmatchList[matching_rows_indices_agg_comb]<-"present"

#Horihoodweb の各行と combinations の各行を比較し、一致する行を特定
matching_rows_indices_hood_comb <- numeric(0)
for (i in 1:nrow(combinations)) {
  for (j in 1:nrow(Hori_foodweb)) {
    if (all(combinations[i, ] == Hori_foodweb[j, ])) {
      matching_rows_indices_hood_comb <- c(matching_rows_indices_hood_comb, i)
    }
  }
}

combinations[matching_rows_indices_hood_comb,]


foodmatchList<-rep(NA,nrow(combinations))

foodmatchList[matching_rows_indices_hood_comb]<-"present"


#10個体未満

index10lower<-unique(c(
  which(combinations$Var1=="Cte. hor"),
  which(combinations$Var2=="Cte. hor"),
  
  which(combinations$Var1=="Cun. lon"),
  which(combinations$Var2=="Cun. lon"),
  
  which(combinations$Var1=="Lep. cun"),
  which(combinations$Var2=="Lep. cun"),
  
  which(combinations$Var1=="Lep. ken"),
  which(combinations$Var2=="Lep. ken"),
  
  which(combinations$Var1=="Lep. mim"),
  which(combinations$Var2=="Lep. mim"),
  
  which(combinations$Var1=="Lep. pro"),
  which(combinations$Var2=="Lep. pro"),
  
  which(combinations$Var1=="Lat. M"),
  which(combinations$Var2=="Lat. M"),
  
  which(combinations$Var1=="Neo. bus"),
  which(combinations$Var2=="Neo. bus"),
  
  which(combinations$Var1=="Neo. cyl"),
  which(combinations$Var2=="Neo. cyl"),
  
  which(combinations$Var1=="Neo. fur"),
  which(combinations$Var2=="Neo. fur"),
  
  which(combinations$Var1=="Neo. mus"),
  which(combinations$Var2=="Neo. mus"),
  
  which(combinations$Var1=="Neo. obs"),
  which(combinations$Var2=="Neo. obs"),
  
  which(combinations$Var1=="Neo. pet"),
  which(combinations$Var2=="Neo. pet"),
  
  which(combinations$Var1=="Neo. pro"),
  which(combinations$Var2=="Neo. pro"),
  
  which(combinations$Var1=="Neo. tet"),
  which(combinations$Var2=="Neo. tet"),
  
  which(combinations$Var1=="Pse. cur"),
  which(combinations$Var2=="Pse. cur"),
  
  which(combinations$Var1=="Pet. fam"),
  which(combinations$Var2=="Pet. fam"),
  
  which(combinations$Var1=="Pet. hor"),
  which(combinations$Var2=="Pet. hor"),
  
  which(combinations$Var1=="Xen. sim"),
  which(combinations$Var2=="Xen. sim"),
  
  which(combinations$Var1=="Xen. fla"),
  which(combinations$Var2=="Xen. fla"),
  
  which(combinations$Var1=="Xen. bou"),
  which(combinations$Var2=="Xen. bou"),
  
  which(combinations$Var1=="Xen. sp"),
  which(combinations$Var2=="Xen. sp"))
)

lowermatchList<-rep(NA,nrow(combinations))

lowermatchList[index10lower]<-"10未満"


#CantEmbedding

indexCantEmbedding<-unique(c(
  which(combinations$Var1=="M. spp"),
  which(combinations$Var2=="M. spp"),
  
  which(combinations$Var1=="Neo. cau"),
  which(combinations$Var2=="Neo. cau"),
  
  which(combinations$Var1=="Pet. fas"),
  which(combinations$Var2=="Pet. fas"),
  
  which(combinations$Var1=="Xen. spi"),
  which(combinations$Var2=="Xen. spi"))
)

cantEmbeddingmatchList<-rep(NA,nrow(combinations))

cantEmbeddingmatchList[indexCantEmbedding]<-"埋め込み不可"

#Linearity

indexLinearity<-unique(c(
  which(combinations$Var1=="Cyp. spp"),
  which(combinations$Var2=="Cyp. spp"),
  
  which(combinations$Var1=="Ere. cya"),
  which(combinations$Var2=="Ere. cya"),
  
  which(combinations$Var1=="Neo. cau"),
  which(combinations$Var2=="Neo. cau"),
  
  which(combinations$Var1=="Neo. pul"),
  which(combinations$Var2=="Neo. pul"),
  
  which(combinations$Var1=="Neo. sav"),
  which(combinations$Var2=="Neo. sav"),
  
  which(combinations$Var1=="Pet. fas"),
  which(combinations$Var2=="Pet. fas"),
  
  which(combinations$Var1=="Sim. dia"),
  which(combinations$Var2=="Sim. dia"))
)


linearitymatchList<-rep(NA,nrow(combinations))

linearitymatchList[indexLinearity]<-"線形性"


causalityTable<- cbind(combinations,CCMmatchList,AggmatchList,foodmatchList,lowermatchList,cantEmbeddingmatchList,linearitymatchList)


#Script S-mapからオブジェクトstrengthtp0は引き継ぐ
strengthtp0<- read.csv("strengthtp0.csv",header=T)

keyTableSmap<-NULL
for(i in 1:nrow(strengthtp0)){
  renameCause<-sp_food_coltp0[strengthtp0$cause[i]==sp_food_coltp0$cause,]$re.namesp
  renameEffect<-sp_food_coltp0[strengthtp0$effect[i]==sp_food_coltp0$cause,]$re.namesp
  keyTableSmap[i]<-paste(renameCause,renameEffect,sep="_")
}
rm(renameCause)
rm(renameEffect)

keyTable<-NULL
keyTable<-paste(combinations$Var1,combinations$Var2,sep="_")


causalityTablewKey<-cbind(causalityTable,keyTable)

colnames(causalityTablewKey)<-c("cause","effect",
                                "CCMmatchList","AggmatchList",
                                "foodmatchList","lowermatchList",
                                "cantEmbeddingmatchList","linearitymatchList","keyTable")

smapTableForTable<-cbind(strengthtp0$smapmean,keyTableSmap)

colnames(smapTableForTable)<-c("SmapMean","keyTable")

smapTableForTable<-as.data.frame(smapTableForTable)

smapTableForTable$SmapMean<-as.numeric(smapTableForTable$SmapMean)

causalityTablewSmap<-dplyr::left_join(causalityTablewKey,smapTableForTable,by="keyTable")


Smapsdkeys<-NULL


renameStrengthtp0Cause<-NULL
renameStrengthtp0Effect<-NULL
for(i in 1:length(strengthtp0$cause)){
  renameStrengthtp0Cause<-sp_food_coltp0[strengthtp0$cause[i]==sp_food_coltp0$cause,]$re.namesp
  renameStrengthtp0Effect<-sp_food_coltp0[strengthtp0$effect[i]==sp_food_coltp0$cause,]$re.namesp
  Smapsdkeys[i]<-paste(renameStrengthtp0Cause,renameStrengthtp0Effect,sep="_")
}

strengthtp0wKeys<-cbind(strengthtp0,Smapsdkeys)[,-1]

colnames(strengthtp0wKeys)<-c(colnames(strengthtp0)[-1],colnames(Smapsdkeys),"keyTable")


STable<-dplyr::left_join(causalityTablewSmap,strengthtp0wKeys,by="keyTable")

colnames(STable)

#線形性、観測個体数不足などのテーブルまとめ
write.csv(STable[,-c(9,11,12)],"causalityTablewSmap.csv")



igraph_allww8sFoodHabit<-NULL

for(i in 1:length(igraph_allww8s$cause.x)){
  igraph_allww8sFoodHabit[i]<-sp_food_coltp0[igraph_allww8s$cause.x[i]==sp_food_coltp0$cause,]$foodhabitat
}

igraph_allww8sRenameCause<-NULL
for(i in 1:length(igraph_allww8s$cause.x)){
  igraph_allww8sRenameCause[i]<-sp_food_coltp0[igraph_allww8s$cause.x[i]==sp_food_coltp0$cause,]$re.namesp
}


igraph_allww8sRenameEffect<-NULL
for(i in 1:length(igraph_allww8s$cause.x)){
  igraph_allww8sRenameEffect[i]<-sp_food_coltp0[igraph_allww8s$effect.x[i]==sp_food_coltp0$cause,]$re.namesp
}

igraph_allww8s$cause.x<-igraph_allww8sRenameCause
igraph_allww8s$effect.x<-igraph_allww8sRenameEffect

ForIgraph1<-cbind(igraph_allww8s[-c(3,6,7,9)],igraph_allww8sFoodHabit)

write.csv(cbind(igraph_allww8s[-c(3,6,7,9)],igraph_allww8sFoodHabit),"ForIgraph1.csv")

ForIgraph2<-STable[,-c(9,11,12)]

keyTable<-paste(igraph_allww8s$cause.x,igraph_allww8s$effect.x,sep="_")

igraph_allww8sWkey<-cbind(igraph_allww8s,keyTable)

ForIgraph<-left_join(STable,igraph_allww8sWkey,by="keyTable")

ForIgraph$keyTable<-gsub(" ","",ForIgraph$keyTable)

ForIgraph$cause.x.x<-gsub(" ","",ForIgraph$cause.x.x)

ForIgraph$effect.x.x<-gsub(" ","",ForIgraph$effect.x.x)

Aggressive_Non_Persist_ForIgraph<-read.csv("Aggressive_Non_Persist.csv")

Aggressive_Non_Persist_ForIgraph<-subset(Aggressive_Non_Persist_ForIgraph,Aggressive_Non_Persist_ForIgraph$ratio!=0)

Aggressive_Non_Persist_ForIgraphWkey<-cbind(Aggressive_Non_Persist_ForIgraph,paste(Aggressive_Non_Persist_ForIgraph$cause,Aggressive_Non_Persist_ForIgraph$effect,sep="_"))

colnames(Aggressive_Non_Persist_ForIgraphWkey)<-c("cause","effect","ratio","keyTable")

#cya.fur を Cya.sppに変更

Aggressive_Non_Persist_ForIgraphWkey$keyTable<-gsub("Cya.fur","Cya.spp",Aggressive_Non_Persist_ForIgraphWkey$keyTable)

for(i in 1:length(Aggressive_Non_Persist_ForIgraphWkey$keyTable)){
  ForIgraph[ForIgraph$keyTable==Aggressive_Non_Persist_ForIgraphWkey$keyTable[i],]$AggmatchList<-Aggressive_Non_Persist_ForIgraphWkey$ratio[i]
}





Foodweb_Non_Persist_ForIgraph<-read.csv("Foodweb_Non_Persist.csv")


Foodweb_Non_Persist_ForIgraph<-subset(Foodweb_Non_Persist_ForIgraph,Foodweb_Non_Persist_ForIgraph$ratio!=0)

Foodweb_Non_Persist_ForIgraphWkey<-cbind(Foodweb_Non_Persist_ForIgraph,paste(Foodweb_Non_Persist_ForIgraph$cause,Foodweb_Non_Persist_ForIgraph$effect,sep="_"))

colnames(Foodweb_Non_Persist_ForIgraphWkey)<-c("cause","effect","ratio","keyTable")

Foodweb_Non_Persist_ForIgraphWkey$keyTable<-gsub("Cya.fur","Cya.spp",Foodweb_Non_Persist_ForIgraphWkey$keyTable)



for(i in 1:length(Foodweb_Non_Persist_ForIgraphWkey$keyTable)){
  ForIgraph[ForIgraph$keyTable==Foodweb_Non_Persist_ForIgraphWkey$keyTable[i],]$foodmatchList<-Foodweb_Non_Persist_ForIgraphWkey$ratio[i]
}

write.csv(ForIgraph,"ForIgraph.csv")


colnames(ForIgraph)

pieForCausality<-read.csv("correlation_ratio.csv")

par(family="Helvetica")
pie(pieForCausality$Count,pieForCausality$Food.habitat,col=pieForCausality$Col,main="Detecting causality with CCM for ratios about food habitat")


intertp0_wKey <- data.frame(
  cause = intertp0$effect,
  effect = intertp0$cause,
  causepopmean = intertp0$effectpopmean,
  effectpopmean = intertp0$causepopmean,
  causepopsd = intertp0$effectpopsd,
  effectpopsd = intertp0$causepopsd,
  causehabitat = intertp0$causehabitat,
  smapmedian = intertp0$smapmedian,
  smapmin = intertp0$smapmin,
  smapmax = intertp0$smapmax,
  smap1st = intertp0$smapX1st,
  smap3rd = intertp0$smapX3rd,
  strength = intertp0$strength)

for(i in 1:nrow(intertp0_wKey)){
  intertp0_wKey$cause[i]<-sp_food_coltp0[intertp0_wKey$cause[i]==sp_food_coltp0$cause,]$bindre.namesp
  intertp0_wKey$effect[i]<-sp_food_coltp0[intertp0_wKey$effect[i]==sp_food_coltp0$cause,]$bindre.namesp
}

intertp0_wKey <- data.frame(
  cause = intertp0$effect,
  effect = intertp0$cause,
  causepopmean = intertp0$effectpopmean,
  effectpopmean = intertp0$causepopmean,
  causepopsd = intertp0$effectpopsd,
  effectpopsd = intertp0$causepopsd,
  causehabitat = intertp0$causehabitat,
  smapmedian = intertp0$smapmedian,
  smapmin = intertp0$smapmin,
  smapmax = intertp0$smapmax,
  smap1st = intertp0$smapX1st,
  smap3rd = intertp0$smapX3rd,
  strength = intertp0$strength,
  key = paste(intertp0_wKey$cause,intertp0_wKey$effect)
)

Hori_aggressive_key<-paste(Hori_aggressive$cause,Hori_aggressive$effect)
Hori_foodweb_key<-paste(Hori_foodweb$cause,Hori_foodweb$effect)

as.numeric(rownames(intertp0_wKey[intertp0_wKey$key %in% Hori_aggressive_key,]))
as.numeric(rownames(intertp0_wKey[intertp0_wKey$key %in% Hori_foodweb_key,]))

intertp0_wKey<-cbind(intertp0_wKey,"match"=rep(NA,nrow(intertp0_wKey)))

intertp0_wKey$match[as.numeric(rownames(intertp0_wKey[intertp0_wKey$key %in% Hori_aggressive_key,]))]<-"aggressive_behavior"

for(i in 1:nrow(intertp0_wKey)){
  intertp0_wKey$causehabitat[i]<- sp_food_coltp0[sp_food_coltp0$cause==intertp0_wKey$cause[i],]$foodhabit7
}


intertp0_positive<-subset(intertp0_wKey,intertp0_wKey$strength=="positive")
# intertp0_positiveのcauseとeffectの組み合わせを作成
intertp0_positive_combined <- paste(intertp0_positive$cause, intertp0_positive$effect)
intertp0_positive_swapped<-paste(intertp0_positive$effect, intertp0_positive$cause)

# intertp0_positive_combinedがintertp0_positive_swappedに含まれるかどうかを確認し、その結果を使って抽出する
reversed_causal_positive_relationships <- intertp0_positive[intertp0_positive_combined %in% intertp0_positive_swapped, ]


for(i in 1:nrow(reversed_causal_positive_relationships)){
  reversed_causal_positive_relationships$cause[i]<- sp_food_coltp0[sp_food_coltp0$cause==reversed_causal_positive_relationships$cause[i],]$bindre.namesp
  reversed_causal_positive_relationships$effect[i]<- sp_food_coltp0[sp_food_coltp0$cause==reversed_causal_positive_relationships$effect[i],]$bindre.namesp
}

write.csv(reversed_causal_positive_relationships,"pos-pos.csv")


intertp0_negative<-subset(intertp0_wKey,intertp0_wKey$strength=="negative")

# intertp0_negativeのcauseとeffectの組み合わせを作成
intertp0_negative_combined <- paste(intertp0_negative$cause, intertp0_negative$effect)
intertp0_negative_swapped <- paste(intertp0_negative$effect, intertp0_negative$cause)


# intertp0_negative_combinedがintertp0_negative_swappedに含まれるかどうかを確認し、その結果を使って抽出する
reversed_causal_negative_relationships <- intertp0_negative[intertp0_negative_combined %in% intertp0_negative_swapped, ]


for(i in 1:nrow(reversed_causal_negative_relationships)){
  reversed_causal_negative_relationships$cause[i]<- sp_food_coltp0[sp_food_coltp0$cause==reversed_causal_negative_relationships$cause[i],]$bindre.namesp
  reversed_causal_negative_relationships$effect[i]<- sp_food_coltp0[sp_food_coltp0$cause==reversed_causal_negative_relationships$effect[i],]$bindre.namesp
}

write.csv(reversed_causal_negative_relationships,"neg-neg.csv")

anti_joinPos<-anti_join(intertp0_wKey,reversed_causal_positive_relationships,by="key")
anti_joinPosNeg<-anti_join(anti_joinPos,reversed_causal_negative_relationships,by="key")

# intertp0_negativeのcauseとeffectの組み合わせを作成
anti_joinPosNeg_combined <- paste(anti_joinPosNeg$cause, anti_joinPosNeg$effect)
anti_joinPosNeg_swapped <- paste(anti_joinPosNeg$effect, anti_joinPosNeg$cause)

anti_joinPosNeg_relationships <- anti_joinPosNeg[anti_joinPosNeg_combined %in% anti_joinPosNeg_swapped, ]

for(i in 1:nrow(anti_joinPosNeg_relationships)){
  anti_joinPosNeg_relationships$cause[i]<- sp_food_coltp0[sp_food_coltp0$cause==anti_joinPosNeg_relationships$cause[i],]$bindre.namesp
  anti_joinPosNeg_relationships$effect[i]<- sp_food_coltp0[sp_food_coltp0$cause==anti_joinPosNeg_relationships$effect[i],]$bindre.namesp
}

write.csv(anti_joinPosNeg_relationships,"pos-neg.csv")

#一方方向
oneDirection<-anti_join(anti_joinPosNeg,anti_joinPosNeg_relationships,by="key")

for(i in 1:nrow(oneDirection)){
  oneDirection$cause[i]<- sp_food_coltp0[sp_food_coltp0$cause==oneDirection$cause[i],]$bindre.namesp
  oneDirection$effect[i]<- sp_food_coltp0[sp_food_coltp0$cause==oneDirection$effect[i],]$bindre.namesp
}

write.csv(oneDirection,"oneDirection.csv")









#10未満で削除されたもの
nrow(ForIgraph[!is.na(ForIgraph$lowermatchList),])


ForIgraphOver10<-ForIgraph[is.na(ForIgraph$lowermatchList),]

#埋め込み不可
nrow(ForIgraphOver10[!is.na(ForIgraphOver10$cantEmbeddingmatchList),])

ForIgraphCanEmbed<-ForIgraphOver10[is.na(ForIgraphOver10$cantEmbeddingmatchList),]

#線形性
nrow(ForIgraphCanEmbed[!is.na(ForIgraphCanEmbed$linearitymatchList),])

ForIgraphCanCCM<-ForIgraphCanEmbed[is.na(ForIgraphCanEmbed$linearitymatchList),]

#因果ナシ
nrow(ForIgraphCanCCM[is.na(ForIgraphCanCCM$CCMmatchList),])


#FigS1
population<-TimeSeries_40[,CCM_list]

popname<-NULL
for(i in 1:length(colnames(population))){
  popname[i]<-sp_food_coltp0[colnames(population)[i]==sp_food_coltp0$cause,]$bindre.namesp
}

colnames(population)<-popname

population<-rbind(population[c(1:20),],NA,population[c(21:40),])

populationList<-NULL
for(i in 1:ncol(population)){
  populationList[[i]]<-population[,i]
  print(max(population[,i],na.rm = TRUE))
}

names(populationList)<-colnames(population)

years <- seq(1995, 2014, by = 1)


pdf("plotS1.pdf", width = 8.27, height = 11.69)  # A4サイズ
par(family="Times",mfrow=c(6,3),mar=c(3,3,1,1))
for(i in 1:length(populationList)){
  plot(populationList[[i]],xlab = "Time index",ylab="Population",type="l",ylim=c(0,460),main=names(populationList)[i],xaxt = "n")
  abline(v=21,lty=2)
  axis(1, at = 1:length(populationList[[i]]),labels = c(years,NA,years),las=2)
}
dev.off()


#FigS2

load("spsmaptp0.Rdata")


index<-NULL

for(i in 1:length(spsmaptp0)){
  index[i]<-ncol(spsmaptp0[[i]][[1]])>2
}

SmapInterListWithIntB<-spsmaptp0[index]

SmapInterOnly<-list()
for(i in 1:length(SmapInterListWithIntB)){
  SmapInterOnly[[i]]<-SmapInterListWithIntB[[i]][[1]][,c(-1,-ncol(SmapInterListWithIntB[[i]][[1]]))]
}

names(SmapInterOnly)<-names(SmapInterListWithIntB)



#------Smap係数描画のためのデータ整形

dataframe_Smap_A_dews<-list()

for(i in 1:ncol(SmapInterOnly$A_dewindti)){
  dataframe_Smap_A_dews[[i]] <- c(SmapInterOnly$A_dewindti[,i][c(1:19)],rep(NA,1),SmapInterOnly$A_dewindti[,i][c(20:38)])
  dataframe_Smap_A_dews[[i]] <- cbind(dataframe_Smap_A_dews[[i]],rep(names(SmapInterOnly$A_dewindti[i]),length(dataframe_Smap_A_dews[[i]])))
}

dataframe_Smap_A_dew_index<-list()

for(i in 1:length(dataframe_Smap_A_dews)){
  dataframe_Smap_A_dew_index[[i]] <- cbind(dataframe_Smap_A_dews[[i]],c(1:39))
  dataframe_Smap_A_dew_index[[i]] <- as.data.frame(dataframe_Smap_A_dew_index[[i]])
}

for(i in 1:length(dataframe_Smap_A_dew_index)){
  colnames(dataframe_Smap_A_dew_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_A_dew<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_A_dew_index)){
  dataframe_Smap_A_dew <- rbind(dataframe_Smap_A_dew,dataframe_Smap_A_dew_index[[i]])
}

dataframe_Smap_A_dew$Smap<-as.numeric(dataframe_Smap_A_dew$Smap)
dataframe_Smap_A_dew$index<-as.numeric(dataframe_Smap_A_dew$index)

dataframe_Smap_I_loos<-list()
for(i in 1:ncol(SmapInterOnly$I_loocki)){
  dataframe_Smap_I_loos[[i]] <- c(SmapInterOnly$I_loocki[,i][c(1:19)],rep(NA,1),SmapInterOnly$I_loocki[,i][c(20:38)])
  dataframe_Smap_I_loos[[i]] <- cbind(dataframe_Smap_I_loos[[i]],rep(names(SmapInterOnly$I_loocki[i]),length(dataframe_Smap_I_loos[[i]])))
}

dataframe_Smap_I_loos_index<-list()

for(i in 1:length(dataframe_Smap_I_loos)){
  dataframe_Smap_I_loos_index[[i]] <- cbind(dataframe_Smap_I_loos[[i]],c(1:39))
  dataframe_Smap_I_loos_index[[i]] <- as.data.frame(dataframe_Smap_I_loos_index[[i]])
}

for(i in 1:length(dataframe_Smap_I_loos_index)){
  colnames(dataframe_Smap_I_loos_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_I_loo<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_I_loos_index)){
  dataframe_Smap_I_loo <- rbind(dataframe_Smap_I_loo,dataframe_Smap_I_loos_index[[i]])
}


dataframe_Smap_I_loo$Smap<-as.numeric(dataframe_Smap_I_loo$Smap)
dataframe_Smap_I_loo$index<-as.numeric(dataframe_Smap_I_loo$index)


dataframe_Smap_L_cals<-list()

for(i in 1:ncol(SmapInterOnly$L_callipterus)){
  dataframe_Smap_L_cals[[i]] <- c(SmapInterOnly$L_callipterus[,i][c(1:19)],rep(NA,1),SmapInterOnly$L_callipterus[,i][c(20:38)])
  dataframe_Smap_L_cals[[i]] <- cbind(dataframe_Smap_L_cals[[i]],rep(names(SmapInterOnly$L_callipterus[i]),length(dataframe_Smap_L_cals[[i]])))
}

dataframe_Smap_L_cals_index<-list()

for(i in 1:length(dataframe_Smap_L_cals)){
  dataframe_Smap_L_cals_index[[i]] <- cbind(dataframe_Smap_L_cals[[i]],c(1:39))
  dataframe_Smap_L_cals_index[[i]] <- as.data.frame(dataframe_Smap_L_cals_index[[i]])
}

for(i in 1:length(dataframe_Smap_L_cals_index)){
  colnames(dataframe_Smap_L_cals_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_L_cal<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_L_cals_index)){
  dataframe_Smap_L_cal <- rbind(dataframe_Smap_L_cal,dataframe_Smap_L_cals_index[[i]])
}

dataframe_Smap_L_cal$Smap<- as.numeric(dataframe_Smap_L_cal$Smap)

dataframe_Smap_L_cal$index<- as.numeric(dataframe_Smap_L_cal$index)

dataframe_Smap_L_elos<-list()

for(i in 1:ncol(SmapInterOnly$L_elongatus)){
  dataframe_Smap_L_elos[[i]] <- c(SmapInterOnly$L_elongatus[,i][c(1:19)],rep(NA,1),SmapInterOnly$L_elongatus[,i][c(20:38)])
  dataframe_Smap_L_elos[[i]] <- cbind(dataframe_Smap_L_elos[[i]],rep(names(SmapInterOnly$L_elongatus[i]),length(dataframe_Smap_L_elos[[i]])))
}

dataframe_Smap_L_elos_index<-list()

for(i in 1:length(dataframe_Smap_L_elos)){
  dataframe_Smap_L_elos_index[[i]] <- cbind(dataframe_Smap_L_elos[[i]],c(1:39))
  dataframe_Smap_L_elos_index[[i]] <- as.data.frame(dataframe_Smap_L_elos_index[[i]])
}

for(i in 1:length(dataframe_Smap_L_elos_index)){
  colnames(dataframe_Smap_L_elos_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_L_elo<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_L_elos_index)){
  dataframe_Smap_L_elo <- rbind(dataframe_Smap_L_elo,dataframe_Smap_L_elos_index[[i]])
}

dataframe_Smap_L_elo$Smap<-as.numeric(dataframe_Smap_L_elo$Smap)
dataframe_Smap_L_elo$index<-as.numeric(dataframe_Smap_L_elo$index)




dataframe_Smap_N_fass<-list()

for(i in 1:ncol(SmapInterOnly$N_fasciatus)){
  dataframe_Smap_N_fass[[i]] <- c(SmapInterOnly$N_fasciatus[,i][c(1:19)],rep(NA,1),SmapInterOnly$N_fasciatus[,i][c(20:38)])
  dataframe_Smap_N_fass[[i]] <- cbind(dataframe_Smap_N_fass[[i]],rep(names(SmapInterOnly$N_fasciatus[i]),length(dataframe_Smap_N_fass[[i]])))
}

dataframe_Smap_N_fass_index<-list()

for(i in 1:length(dataframe_Smap_N_fass)){
  dataframe_Smap_N_fass_index[[i]] <- cbind(dataframe_Smap_N_fass[[i]],c(1:39))
  dataframe_Smap_N_fass_index[[i]] <- as.data.frame(dataframe_Smap_N_fass_index[[i]])
}

for(i in 1:length(dataframe_Smap_N_fass_index)){
  colnames(dataframe_Smap_N_fass_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_N_fas<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_N_fass_index)){
  dataframe_Smap_N_fas <- rbind(dataframe_Smap_N_fas,dataframe_Smap_N_fass_index[[i]])
}

dataframe_Smap_N_fas$Smap<-as.numeric(dataframe_Smap_N_fas$Smap)
dataframe_Smap_N_fas$index<-as.numeric(dataframe_Smap_N_fas$index)

dataframe_Smap_N_sexs<-list()

for(i in 1:ncol(SmapInterOnly$N_sexfasciatus)){
  dataframe_Smap_N_sexs[[i]] <- c(SmapInterOnly$N_sexfasciatus[,i][c(1:19)],rep(NA,1),SmapInterOnly$N_sexfasciatus[,i][c(20:38)])
  dataframe_Smap_N_sexs[[i]] <- cbind(dataframe_Smap_N_sexs[[i]],rep(names(SmapInterOnly$N_sexfasciatus[i]),length(dataframe_Smap_N_sexs[[i]])))
}

dataframe_Smap_N_sexs_index<-list()

for(i in 1:length(dataframe_Smap_N_sexs)){
  dataframe_Smap_N_sexs_index[[i]] <- cbind(dataframe_Smap_N_sexs[[i]],c(1:39))
  dataframe_Smap_N_sexs_index[[i]] <- as.data.frame(dataframe_Smap_N_sexs_index[[i]])
}

for(i in 1:length(dataframe_Smap_N_sexs_index)){
  colnames(dataframe_Smap_N_sexs_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_N_sex<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_N_sexs_index)){
  dataframe_Smap_N_sex <- rbind(dataframe_Smap_N_sex,dataframe_Smap_N_sexs_index[[i]])
}

dataframe_Smap_N_sex$Smap<-as.numeric(dataframe_Smap_N_sex$Smap)
dataframe_Smap_N_sex$index<-as.numeric(dataframe_Smap_N_sex$index)

dataframe_Smap_O_vens<-list()
for(i in 1:ncol(SmapInterOnly$O_ventralis)){
  dataframe_Smap_O_vens[[i]] <- c(SmapInterOnly$O_ventralis[,i][c(1:19)],rep(NA,1),SmapInterOnly$O_ventralis[,i][c(20:38)])
  dataframe_Smap_O_vens[[i]] <- cbind(dataframe_Smap_O_vens[[i]],rep(names(SmapInterOnly$O_ventralis[i]),length(dataframe_Smap_O_vens[[i]])))
}

dataframe_Smap_O_vens_index<-list()

for(i in 1:length(dataframe_Smap_O_vens)){
  dataframe_Smap_O_vens_index[[i]] <- cbind(dataframe_Smap_O_vens[[i]],c(1:39))
  dataframe_Smap_O_vens_index[[i]] <- as.data.frame(dataframe_Smap_O_vens_index[[i]])
}

for(i in 1:length(dataframe_Smap_O_vens_index)){
  colnames(dataframe_Smap_O_vens_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_O_ven<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_O_vens_index)){
  dataframe_Smap_O_ven <- rbind(dataframe_Smap_O_ven,dataframe_Smap_O_vens_index[[i]])
}

dataframe_Smap_O_ven$Smap<-as.numeric(dataframe_Smap_O_ven$Smap)
dataframe_Smap_O_ven$index<-as.numeric(dataframe_Smap_O_ven$index)

dataframe_Smap_P_mics<-list()
for(i in 1:ncol(SmapInterOnly$P_microlepis)){
  dataframe_Smap_P_mics[[i]] <- c(SmapInterOnly$P_microlepis[,i][c(1:19)],rep(NA,1),SmapInterOnly$P_microlepis[,i][c(20:38)])
  dataframe_Smap_P_mics[[i]] <- cbind(dataframe_Smap_P_mics[[i]],rep(names(SmapInterOnly$P_microlepis[i]),length(dataframe_Smap_P_mics[[i]])))
}

dataframe_Smap_P_mics_index<-list()

for(i in 1:length(dataframe_Smap_P_mics)){
  dataframe_Smap_P_mics_index[[i]] <- cbind(dataframe_Smap_P_mics[[i]],c(1:39))
  dataframe_Smap_P_mics_index[[i]] <- as.data.frame(dataframe_Smap_P_mics_index[[i]])
}

for(i in 1:length(dataframe_Smap_P_mics_index)){
  colnames(dataframe_Smap_P_mics_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_P_mic<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_P_mics_index)){
  dataframe_Smap_P_mic <- rbind(dataframe_Smap_P_mic,dataframe_Smap_P_mics_index[[i]])
}

dataframe_Smap_P_mic$Smap<-as.numeric(dataframe_Smap_P_mic$Smap)
dataframe_Smap_P_mic$index<-as.numeric(dataframe_Smap_P_mic$index)

dataframe_Smap_P_tres<-list()
for(i in 1:ncol(SmapInterOnly$P_trewavasae)){
  dataframe_Smap_P_tres[[i]] <- c(SmapInterOnly$P_trewavasae[,i][c(1:19)],rep(NA,1),SmapInterOnly$P_trewavasae[,i][c(20:38)])
  dataframe_Smap_P_tres[[i]] <- cbind(dataframe_Smap_P_tres[[i]],rep(names(SmapInterOnly$P_trewavasae[i]),length(dataframe_Smap_P_tres[[i]])))
}

dataframe_Smap_P_tres_index<-list()

for(i in 1:length(dataframe_Smap_P_tres)){
  dataframe_Smap_P_tres_index[[i]] <- cbind(dataframe_Smap_P_tres[[i]],c(1:39))
  dataframe_Smap_P_tres_index[[i]] <- as.data.frame(dataframe_Smap_P_tres_index[[i]])
}

for(i in 1:length(dataframe_Smap_P_tres_index)){
  colnames(dataframe_Smap_P_tres_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_P_tre<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_P_tres_index)){
  dataframe_Smap_P_tre <- rbind(dataframe_Smap_P_tre,dataframe_Smap_P_tres_index[[i]])
}

dataframe_Smap_P_tre$Smap<-as.numeric(dataframe_Smap_P_tre$Smap)
dataframe_Smap_P_tre$index<-as.numeric(dataframe_Smap_P_tre$index)

#L_lemairii(eff)-A_dewindti(cau)


dataframe_Smap_T_moos<-list()
for(i in 1:ncol(SmapInterOnly$T_moorii)){
  dataframe_Smap_T_moos[[i]] <- c(SmapInterOnly$T_moorii[,i][c(1:19)],rep(NA,1),SmapInterOnly$T_moorii[,i][c(20:38)])
  dataframe_Smap_T_moos[[i]] <- cbind(dataframe_Smap_T_moos[[i]],rep(names(SmapInterOnly$T_moorii[i]),length(dataframe_Smap_T_moos[[i]])))
}

dataframe_Smap_T_moos_index<-list()

for(i in 1:length(dataframe_Smap_T_moos)){
  dataframe_Smap_T_moos_index[[i]] <- cbind(dataframe_Smap_T_moos[[i]],c(1:39))
  dataframe_Smap_T_moos_index[[i]] <- as.data.frame(dataframe_Smap_T_moos_index[[i]])
}

for(i in 1:length(dataframe_Smap_T_moos_index)){
  colnames(dataframe_Smap_T_moos_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_T_moo<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_T_moos_index)){
  dataframe_Smap_T_moo <- rbind(dataframe_Smap_T_moo,dataframe_Smap_T_moos_index[[i]])
}

dataframe_Smap_T_moo$Smap<-as.numeric(dataframe_Smap_T_moo$Smap)
dataframe_Smap_T_moo$index<-as.numeric(dataframe_Smap_T_moo$index)






##-----

dataframe_Smap_T_vits<-list()
for(i in 1:ncol(SmapInterOnly$T_vittatus)){
  dataframe_Smap_T_vits[[i]] <- c(SmapInterOnly$T_vittatus[,i][c(1:19)],rep(NA,1),SmapInterOnly$T_vittatus[,i][c(20:38)])
  dataframe_Smap_T_vits[[i]] <- cbind(dataframe_Smap_T_vits[[i]],rep(names(SmapInterOnly$T_vittatus[i]),length(dataframe_Smap_T_vits[[i]])))
}


dataframe_Smap_T_vits_index<-list()

for(i in 1:length(dataframe_Smap_T_vits)){
  dataframe_Smap_T_vits_index[[i]] <- cbind(dataframe_Smap_T_vits[[i]],c(1:39))
  dataframe_Smap_T_vits_index[[i]] <- as.data.frame(dataframe_Smap_T_vits_index[[i]])
}

for(i in 1:length(dataframe_Smap_T_vits_index)){
  colnames(dataframe_Smap_T_vits_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_T_vit<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_T_vits_index)){
  dataframe_Smap_T_vit <- rbind(dataframe_Smap_T_vit,dataframe_Smap_T_vits_index[[i]])
}

dataframe_Smap_T_vit$Smap<-as.numeric(dataframe_Smap_T_vit$Smap)
dataframe_Smap_T_vit$index<-as.numeric(dataframe_Smap_T_vit$index)

dataframe_Smap_L_dars<-list()

for(i in 1:ncol(SmapInterOnly$L_dardennii)){
  dataframe_Smap_L_dars[[i]] <- c(SmapInterOnly$L_dardennii[,i][c(1:19)],rep(NA,1),SmapInterOnly$L_dardennii[,i][c(20:38)])
  dataframe_Smap_L_dars[[i]] <- cbind(dataframe_Smap_L_dars[[i]],rep(names(SmapInterOnly$L_dardennii[i]),length(dataframe_Smap_L_dars[[i]])))
}

dataframe_Smap_L_dars_index<-list()

for(i in 1:length(dataframe_Smap_L_dars)){
  dataframe_Smap_L_dars_index[[i]] <- cbind(dataframe_Smap_L_dars[[i]],c(1:39))
  dataframe_Smap_L_dars_index[[i]] <- as.data.frame(dataframe_Smap_L_dars_index[[i]])
}

for(i in 1:length(dataframe_Smap_L_dars_index)){
  colnames(dataframe_Smap_L_dars_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_L_dar<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_L_dars_index)){
  dataframe_Smap_L_dar <- rbind(dataframe_Smap_L_dar,dataframe_Smap_L_dars_index[[i]])
}

dataframe_Smap_L_dar$Smap<-as.numeric(dataframe_Smap_L_dar$Smap)
dataframe_Smap_L_dar$index<-as.numeric(dataframe_Smap_L_dar$index)

dataframe_Smap_L_tans<-list()
for(i in 1:ncol(SmapInterOnly$L_tanganicanus)){
  dataframe_Smap_L_tans[[i]] <- c(SmapInterOnly$L_tanganicanus[,i][c(1:19)],rep(NA,1),SmapInterOnly$L_tanganicanus[,i][c(20:38)])
  dataframe_Smap_L_tans[[i]] <- cbind(dataframe_Smap_L_tans[[i]],rep(names(SmapInterOnly$L_tanganicanus[i]),length(dataframe_Smap_L_tans[[i]])))
}

dataframe_Smap_L_tans_index<-list()

for(i in 1:length(dataframe_Smap_L_tans)){
  dataframe_Smap_L_tans_index[[i]] <- cbind(dataframe_Smap_L_tans[[i]],c(1:39))
  dataframe_Smap_L_tans_index[[i]] <- as.data.frame(dataframe_Smap_L_tans_index[[i]])
}

for(i in 1:length(dataframe_Smap_L_tans_index)){
  colnames(dataframe_Smap_L_tans_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_L_tan<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_L_tans_index)){
  dataframe_Smap_L_tan <- rbind(dataframe_Smap_L_tan,dataframe_Smap_L_tans_index[[i]])
}

dataframe_Smap_L_tan$Smap<-as.numeric(dataframe_Smap_L_tan$Smap)
dataframe_Smap_L_tan$index<-as.numeric(dataframe_Smap_L_tan$index)

dataframe_Smap_J_orns<-list()

for(i in 1:ncol(SmapInterOnly$J_ornatus)){
  dataframe_Smap_J_orns[[i]] <- c(SmapInterOnly$J_ornatus[,i][c(1:19)],rep(NA,1),SmapInterOnly$J_ornatus[,i][c(20:38)])
  dataframe_Smap_J_orns[[i]] <- cbind(dataframe_Smap_J_orns[[i]],rep(names(SmapInterOnly$J_ornatus[i]),length(dataframe_Smap_J_orns[[i]])))
}

dataframe_Smap_J_orns_index<-list()

for(i in 1:length(dataframe_Smap_J_orns)){
  dataframe_Smap_J_orns_index[[i]] <- cbind(dataframe_Smap_J_orns[[i]],c(1:39))
  dataframe_Smap_J_orns_index[[i]] <- as.data.frame(dataframe_Smap_J_orns_index[[i]])
}

for(i in 1:length(dataframe_Smap_J_orns_index)){
  colnames(dataframe_Smap_J_orns_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_J_orn<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_J_orns_index)){
  dataframe_Smap_J_orn <- rbind(dataframe_Smap_J_orn,dataframe_Smap_J_orns_index[[i]])
}

dataframe_Smap_J_orn$Smap<-as.numeric(dataframe_Smap_J_orn$Smap)
dataframe_Smap_J_orn$index<-as.numeric(dataframe_Smap_J_orn$index)

dataframe_Smap_Pars<-list()

for(i in 1:ncol(SmapInterOnly$Paracyprichromisspp)){
  dataframe_Smap_Pars[[i]] <- c(SmapInterOnly$Paracyprichromisspp[,i][c(1:19)],rep(NA,1),SmapInterOnly$Paracyprichromisspp[,i][c(20:38)])
  dataframe_Smap_Pars[[i]] <- cbind(dataframe_Smap_Pars[[i]],rep(names(SmapInterOnly$Paracyprichromisspp[i]),length(dataframe_Smap_Pars[[i]])))
}

dataframe_Smap_Pars_index<-list()

for(i in 1:length(dataframe_Smap_Pars)){
  dataframe_Smap_Pars_index[[i]] <- cbind(dataframe_Smap_Pars[[i]],c(1:39))
  dataframe_Smap_Pars_index[[i]] <- as.data.frame(dataframe_Smap_Pars_index[[i]])
}

for(i in 1:length(dataframe_Smap_Pars_index)){
  colnames(dataframe_Smap_Pars_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_Par<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_Pars_index)){
  dataframe_Smap_Par <- rbind(dataframe_Smap_Par,dataframe_Smap_Pars_index[[i]])
}

dataframe_Smap_Par$Smap<-as.numeric(dataframe_Smap_Par$Smap)
dataframe_Smap_Par$index<-as.numeric(dataframe_Smap_Par$index)

dataframe_Smap_L_atts<-list()

for(i in 1:ncol(SmapInterOnly$L_attenuatus)){
  dataframe_Smap_L_atts[[i]] <- c(SmapInterOnly$L_attenuatus[,i][c(1:19)],rep(NA,1),SmapInterOnly$L_attenuatus[,i][c(20:38)])
  dataframe_Smap_L_atts[[i]] <- cbind(dataframe_Smap_L_atts[[i]],rep(names(SmapInterOnly$L_attenuatus[i]),length(dataframe_Smap_L_atts[[i]])))
}

dataframe_Smap_L_atts_index<-list()

for(i in 1:length(dataframe_Smap_L_atts)){
  dataframe_Smap_L_atts_index[[i]] <- cbind(dataframe_Smap_L_atts[[i]],c(1:39))
  dataframe_Smap_L_atts_index[[i]] <- as.data.frame(dataframe_Smap_L_atts_index[[i]])
}

for(i in 1:length(dataframe_Smap_L_atts_index)){
  colnames(dataframe_Smap_L_atts_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_L_att<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_L_atts_index)){
  dataframe_Smap_L_att <- rbind(dataframe_Smap_L_att,dataframe_Smap_L_atts_index[[i]])
}

dataframe_Smap_L_att$Smap<-as.numeric(dataframe_Smap_L_att$Smap)
dataframe_Smap_L_att$index<-as.numeric(dataframe_Smap_L_att$index)

dataframe_Smap_X_paps<-list()
for(i in 1:ncol(SmapInterOnly$X_papilio)){
  dataframe_Smap_X_paps[[i]] <- c(SmapInterOnly$X_papilio[,i][c(1:19)],rep(NA,1),SmapInterOnly$X_papilio[,i][c(20:38)])
  dataframe_Smap_X_paps[[i]] <- cbind(dataframe_Smap_X_paps[[i]],rep(names(SmapInterOnly$X_papilio[i]),length(dataframe_Smap_X_paps[[i]])))
}

dataframe_Smap_X_paps_index<-list()

for(i in 1:length(dataframe_Smap_X_paps)){
  dataframe_Smap_X_paps_index[[i]] <- cbind(dataframe_Smap_X_paps[[i]],c(1:39))
  dataframe_Smap_X_paps_index[[i]] <- as.data.frame(dataframe_Smap_X_paps_index[[i]])
}

for(i in 1:length(dataframe_Smap_X_paps_index)){
  colnames(dataframe_Smap_X_paps_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_X_pap<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_X_paps_index)){
  dataframe_Smap_X_pap <- rbind(dataframe_Smap_X_pap,dataframe_Smap_X_paps_index[[i]])
}

dataframe_Smap_X_pap$Smap<-as.numeric(dataframe_Smap_X_pap$Smap)
dataframe_Smap_X_pap$index<-as.numeric(dataframe_Smap_X_pap$index)

dataframe_Smap_V_moos<-list()

for(i in 1:ncol(SmapInterOnly$V_moorii)){
  dataframe_Smap_V_moos[[i]] <- c(SmapInterOnly$V_moorii[,i][c(1:19)],rep(NA,1),SmapInterOnly$V_moorii[,i][c(20:38)])
  dataframe_Smap_V_moos[[i]] <- cbind(dataframe_Smap_V_moos[[i]],rep(names(SmapInterOnly$V_moorii[i]),length(dataframe_Smap_V_moos[[i]])))
}

dataframe_Smap_V_moos_index<-list()

for(i in 1:length(dataframe_Smap_V_moos)){
  dataframe_Smap_V_moos_index[[i]] <- cbind(dataframe_Smap_V_moos[[i]],c(1:39))
  dataframe_Smap_V_moos_index[[i]] <- as.data.frame(dataframe_Smap_V_moos_index[[i]])
}

for(i in 1:length(dataframe_Smap_V_moos_index)){
  colnames(dataframe_Smap_V_moos_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_V_moo<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_V_moos_index)){
  dataframe_Smap_V_moo <- rbind(dataframe_Smap_V_moo,dataframe_Smap_V_moos_index[[i]])
}

dataframe_Smap_V_moo$Smap<-as.numeric(dataframe_Smap_V_moo$Smap)
dataframe_Smap_V_moo$index<-as.numeric(dataframe_Smap_V_moo$index)


#--L_labiatus(eff)対L_att(cas)



dataframe_Smap_P_pols<-list()

for(i in 1:ncol(SmapInterOnly$P_polyodon)){
  dataframe_Smap_P_pols[[i]] <- c(SmapInterOnly$P_polyodon[,i][c(1:19)],rep(NA,1),SmapInterOnly$P_polyodon[,i][c(20:38)])
  dataframe_Smap_P_pols[[i]] <- cbind(dataframe_Smap_P_pols[[i]],rep(names(SmapInterOnly$P_polyodon[i]),length(dataframe_Smap_P_pols[[i]])))
}

dataframe_Smap_P_pols_index<-list()

for(i in 1:length(dataframe_Smap_P_pols)){
  dataframe_Smap_P_pols_index[[i]] <- cbind(dataframe_Smap_P_pols[[i]],c(1:39))
  dataframe_Smap_P_pols_index[[i]] <- as.data.frame(dataframe_Smap_P_pols_index[[i]])
}

for(i in 1:length(dataframe_Smap_P_pols_index)){
  colnames(dataframe_Smap_P_pols_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_P_pol<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_P_pols_index)){
  dataframe_Smap_P_pol <- rbind(dataframe_Smap_P_pol,dataframe_Smap_P_pols_index[[i]])
}


dataframe_Smap_P_pol$Smap<-as.numeric(dataframe_Smap_P_pol$Smap)
dataframe_Smap_P_pol$index<-as.numeric(dataframe_Smap_P_pol$index)


dataframe_Smap_S_muls<-list()

for(i in 1:ncol(SmapInterOnly$S_multipunctatus)){
  dataframe_Smap_S_muls[[i]] <- c(SmapInterOnly$S_multipunctatus[,i][c(1:19)],rep(NA,1),SmapInterOnly$S_multipunctatus[,i][c(20:38)])
  dataframe_Smap_S_muls[[i]] <- cbind(dataframe_Smap_S_muls[[i]],rep(names(SmapInterOnly$S_multipunctatus[i]),length(dataframe_Smap_S_muls[[i]])))
}

dataframe_Smap_S_muls_index<-list()

for(i in 1:length(dataframe_Smap_S_muls)){
  dataframe_Smap_S_muls_index[[i]] <- cbind(dataframe_Smap_S_muls[[i]],c(1:39))
  dataframe_Smap_S_muls_index[[i]] <- as.data.frame(dataframe_Smap_S_muls_index[[i]])
}

for(i in 1:length(dataframe_Smap_S_muls_index)){
  colnames(dataframe_Smap_S_muls_index[[i]]) <- c("Smap","cause","index")
}

dataframe_Smap_S_mul<-data.frame(NULL)

for(i in 1:length(dataframe_Smap_S_muls_index)){
  dataframe_Smap_S_mul <- rbind(dataframe_Smap_S_mul,dataframe_Smap_S_muls_index[[i]])
}

dataframe_Smap_S_mul$Smap<-as.numeric(dataframe_Smap_S_mul$Smap)
dataframe_Smap_S_mul$index<-as.numeric(dataframe_Smap_S_mul$index)

dataframe_Smap_L_lems<-list()

dataframe_Smap_L_lems<- c(SmapInterOnly$L_lemairii[c(1:19)],rep(NA,1),SmapInterOnly$L_lemairii[c(20:38)])
dataframe_Smap_L_lems <- cbind(dataframe_Smap_L_lems,rep(names(SmapInterOnly$L_lemairii),length(dataframe_Smap_L_lems)))


dataframe_Smap_L_lems_index<-list()

dataframe_Smap_L_lems_index <- cbind(dataframe_Smap_L_lems,rep("A_dewindti",length(c(1:39))),c(1:39))
dataframe_Smap_L_lems_index <- as.data.frame(dataframe_Smap_L_lems_index)

colnames(dataframe_Smap_L_lems_index) <- c("Smap","cause","index")


dataframe_Smap_L_lem<-data.frame(dataframe_Smap_L_lems_index)



dataframe_Smap_L_lem$Smap<-as.numeric(dataframe_Smap_L_lem$Smap)
dataframe_Smap_L_lem$index<-as.numeric(dataframe_Smap_L_lem$index)

dataframe_Smap_L_labs<-list()

dataframe_Smap_L_labs<- c(SmapInterOnly$L_labiatus[c(1:19)],rep(NA,1),SmapInterOnly$L_labiatus[c(20:38)])
dataframe_Smap_L_labs <- cbind(dataframe_Smap_L_labs,rep(names(SmapInterOnly$L_labiatus),length(dataframe_Smap_L_labs)))


dataframe_Smap_L_labs_index<-list()

dataframe_Smap_L_labs_index <- cbind(dataframe_Smap_L_labs,rep("L_attenuatus",length(c(1:39))),c(1:39))
dataframe_Smap_L_labs_index <- as.data.frame(dataframe_Smap_L_labs_index)

colnames(dataframe_Smap_L_labs_index) <- c("Smap","cause","index")


dataframe_Smap_L_lab<-data.frame(dataframe_Smap_L_labs_index)



dataframe_Smap_L_lab$Smap<-as.numeric(dataframe_Smap_L_lab$Smap)
dataframe_Smap_L_lab$index<-as.numeric(dataframe_Smap_L_lab$index)

#---------Smap係数描画のためのデータ整形その2（因果関係ペアの挿入） 2023/07/25編集 22種

for_all_A_dew<-cbind(dataframe_Smap_A_dew,rep("A_dewindti",nrow(dataframe_Smap_A_dew)))
colnames(for_all_A_dew)<-c("Smap","cause","index","effect")
for_all_A_dew<-cbind(for_all_A_dew,rep(paste(for_all_A_dew$effect,"-",for_all_A_dew$cause),nrow(for_all_A_dew)))
colnames(for_all_A_dew)<-c("Smap","cause","index","effect","effect_cause")

for_all_I_loo<-cbind(dataframe_Smap_I_loo,rep("I_loocki",nrow(dataframe_Smap_I_loo)))
colnames(for_all_I_loo)<-c("Smap","cause","index","effect")
for_all_I_loo<-cbind(for_all_I_loo,rep(paste(for_all_I_loo$effect,"-",for_all_I_loo$cause),nrow(for_all_I_loo)))
colnames(for_all_I_loo)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_cal<-cbind(dataframe_Smap_L_cal,rep("L_callipterus",nrow(dataframe_Smap_L_cal)))
colnames(for_all_L_cal)<-c("Smap","cause","index","effect")
for_all_L_cal<-cbind(for_all_L_cal,rep(paste(for_all_L_cal$effect,"-",for_all_L_cal$cause),nrow(for_all_L_cal)))
colnames(for_all_L_cal)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_elo<-cbind(dataframe_Smap_L_elo,rep("L_elongatus",nrow(dataframe_Smap_L_elo)))
colnames(for_all_L_elo)<-c("Smap","cause","index","effect")
for_all_L_elo<-cbind(for_all_L_elo,rep(paste(for_all_L_elo$effect,"-",for_all_L_elo$cause),nrow(for_all_L_elo)))
colnames(for_all_L_elo)<-c("Smap","cause","index","effect","effect_cause")

for_all_N_fas<-cbind(dataframe_Smap_N_fas,rep("N_fasciatus",nrow(dataframe_Smap_N_fas)))
colnames(for_all_N_fas)<-c("Smap","cause","index","effect")
for_all_N_fas<-cbind(for_all_N_fas,rep(paste(for_all_N_fas$effect,"-",for_all_N_fas$cause),nrow(for_all_N_fas)))
colnames(for_all_N_fas)<-c("Smap","cause","index","effect","effect_cause")


for_all_N_sex<-cbind(dataframe_Smap_N_sex,rep("N_sexpressiceps",nrow(dataframe_Smap_N_sex)))
colnames(for_all_N_sex)<-c("Smap","cause","index","effect")
for_all_N_sex<-cbind(for_all_N_sex,rep(paste(for_all_N_sex$effect,"-",for_all_N_sex$cause),nrow(for_all_N_sex)))
colnames(for_all_N_sex)<-c("Smap","cause","index","effect","effect_cause")


for_all_O_ven<-cbind(dataframe_Smap_O_ven,rep("O_ventralis",nrow(dataframe_Smap_O_ven)))
colnames(for_all_O_ven)<-c("Smap","cause","index","effect")
for_all_O_ven<-cbind(for_all_O_ven,rep(paste(for_all_O_ven$effect,"-",for_all_O_ven$cause),nrow(for_all_O_ven)))
colnames(for_all_O_ven)<-c("Smap","cause","index","effect","effect_cause")

for_all_P_mic<-cbind(dataframe_Smap_P_mic,rep("P_microlepis",nrow(dataframe_Smap_P_mic)))
colnames(for_all_P_mic)<-c("Smap","cause","index","effect")
for_all_P_mic<-cbind(for_all_P_mic,rep(paste(for_all_P_mic$effect,"-",for_all_P_mic$cause),nrow(for_all_P_mic)))
colnames(for_all_P_mic)<-c("Smap","cause","index","effect","effect_cause")


for_all_P_tre<-cbind(dataframe_Smap_P_tre,rep("P_trewavasae",nrow(dataframe_Smap_P_tre)))
colnames(for_all_P_tre)<-c("Smap","cause","index","effect")
for_all_P_tre<-cbind(for_all_P_tre,rep(paste(for_all_P_tre$effect,"-",for_all_P_tre$cause),nrow(for_all_P_tre)))
colnames(for_all_P_tre)<-c("Smap","cause","index","effect","effect_cause")


for_all_T_moo<-cbind(dataframe_Smap_T_moo,rep("T_moorii",nrow(dataframe_Smap_T_moo)))
colnames(for_all_T_moo)<-c("Smap","cause","index","effect")
for_all_T_moo<-cbind(for_all_T_moo,rep(paste(for_all_T_moo$effect,"-",for_all_T_moo$cause),nrow(for_all_T_moo)))
colnames(for_all_T_moo)<-c("Smap","cause","index","effect","effect_cause")


for_all_T_vit<-cbind(dataframe_Smap_T_vit,rep("T_vittatus",nrow(dataframe_Smap_T_vit)))
colnames(for_all_T_vit)<-c("Smap","cause","index","effect")
for_all_T_vit<-cbind(for_all_T_vit,rep(paste(for_all_T_vit$effect,"-",for_all_T_vit$cause),nrow(for_all_T_vit)))
colnames(for_all_T_vit)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_dar<-cbind(dataframe_Smap_L_dar,rep("L_dardennii",nrow(dataframe_Smap_L_dar)))
colnames(for_all_L_dar)<-c("Smap","cause","index","effect")
for_all_L_dar<-cbind(for_all_L_dar,rep(paste(for_all_L_dar$effect,"-",for_all_L_dar$cause),nrow(for_all_L_dar)))
colnames(for_all_L_dar)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_tan<-cbind(dataframe_Smap_L_tan,rep("L_tanganicanus",nrow(dataframe_Smap_L_tan)))
colnames(for_all_L_tan)<-c("Smap","cause","index","effect")
for_all_L_tan<-cbind(for_all_L_tan,rep(paste(for_all_L_tan$effect,"-",for_all_L_tan$cause),nrow(for_all_L_tan)))
colnames(for_all_L_tan)<-c("Smap","cause","index","effect","effect_cause")



for_all_J_orn<-cbind(dataframe_Smap_J_orn,rep("J_ornatus",nrow(dataframe_Smap_J_orn)))
colnames(for_all_J_orn)<-c("Smap","cause","index","effect")
for_all_J_orn<-cbind(for_all_J_orn,rep(paste(for_all_J_orn$effect,"-",for_all_J_orn$cause),nrow(for_all_J_orn)))
colnames(for_all_J_orn)<-c("Smap","cause","index","effect","effect_cause")

for_all_Par<-cbind(dataframe_Smap_Par,rep("Paracyprichromisspp",nrow(dataframe_Smap_Par)))
colnames(for_all_Par)<-c("Smap","cause","index","effect")
for_all_Par<-cbind(for_all_Par,rep(paste(for_all_Par$effect,"-",for_all_Par$cause),nrow(for_all_Par)))
colnames(for_all_Par)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_att<-cbind(dataframe_Smap_L_att,rep("L_attenuatus",nrow(dataframe_Smap_L_att)))
colnames(for_all_L_att)<-c("Smap","cause","index","effect")
for_all_L_att<-cbind(for_all_L_att,rep(paste(for_all_L_att$effect,"-",for_all_L_att$cause),nrow(for_all_L_att)))
colnames(for_all_L_att)<-c("Smap","cause","index","effect","effect_cause")


for_all_X_pap<-cbind(dataframe_Smap_X_pap,rep("X_papilio",nrow(dataframe_Smap_X_pap)))
colnames(for_all_X_pap)<-c("Smap","cause","index","effect")
for_all_X_pap<-cbind(for_all_X_pap,rep(paste(for_all_X_pap$effect,"-",for_all_X_pap$cause),nrow(for_all_X_pap)))
colnames(for_all_X_pap)<-c("Smap","cause","index","effect","effect_cause")


for_all_V_moo<-cbind(dataframe_Smap_V_moo,rep("V__moorii",nrow(dataframe_Smap_V_moo)))
colnames(for_all_V_moo)<-c("Smap","cause","index","effect")
for_all_V_moo<-cbind(for_all_V_moo,rep(paste(for_all_V_moo$effect,"-",for_all_V_moo$cause),nrow(for_all_V_moo)))
colnames(for_all_V_moo)<-c("Smap","cause","index","effect","effect_cause")


for_all_P_pol<-cbind(dataframe_Smap_P_pol,rep("P_polyodon",nrow(dataframe_Smap_P_pol)))
colnames(for_all_P_pol)<-c("Smap","cause","index","effect")
for_all_P_pol<-cbind(for_all_P_pol,rep(paste(for_all_P_pol$effect,"-",for_all_P_pol$cause),nrow(for_all_P_pol)))
colnames(for_all_P_pol)<-c("Smap","cause","index","effect","effect_cause")


for_all_S_mul<-cbind(dataframe_Smap_S_mul,rep("S_multipunctatus",nrow(dataframe_Smap_S_mul)))
colnames(for_all_S_mul)<-c("Smap","cause","index","effect")
for_all_S_mul<-cbind(for_all_S_mul,rep(paste(for_all_S_mul$effect,"-",for_all_S_mul$cause),nrow(for_all_S_mul)))
colnames(for_all_S_mul)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_lem<-cbind(dataframe_Smap_L_lem,rep("L_lemairii",nrow(dataframe_Smap_L_lem)))
colnames(for_all_L_lem)<-c("Smap","cause","index","effect")
for_all_L_lem<-cbind(for_all_L_lem,rep(paste(for_all_L_lem$effect,"-",for_all_L_lem$cause),nrow(for_all_L_lem)))
colnames(for_all_L_lem)<-c("Smap","cause","index","effect","effect_cause")

for_all_L_lab<-cbind(dataframe_Smap_L_lab,rep("L_labiatus",nrow(dataframe_Smap_L_lab)))
colnames(for_all_L_lab)<-c("Smap","cause","index","effect")
for_all_L_lab<-cbind(for_all_L_lab,rep(paste(for_all_L_lab$effect,"-",for_all_L_lab$cause),nrow(for_all_L_lab)))
colnames(for_all_L_lab)<-c("Smap","cause","index","effect","effect_cause")

for_all_L_lem<-cbind(dataframe_Smap_L_lem,rep("L_lemairii",nrow(dataframe_Smap_L_lem)))
colnames(for_all_L_lem)<-c("Smap","cause","index","effect")
for_all_L_lem<-cbind(for_all_L_lem,rep(paste(for_all_L_lem$effect,"-",for_all_L_lem$cause),nrow(for_all_L_lem)))
colnames(for_all_L_lem)<-c("Smap","cause","index","effect","effect_cause")


#個別のggplot-------------
#凡例の整理2023/09/01編集
#凡例はSmap係数の平均値で高いものから上に配置

yearslabel <- c(rep(c(seq(1995,2013,1),NA),2))



Auldew_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_A_dew$cause))){
  Auldew_leg_order<-c(unique(dataframe_Smap_A_dew$cause)[i],mean(subset(dataframe_Smap_A_dew$Smap,dataframe_Smap_A_dew$cause==unique(dataframe_Smap_A_dew$cause)[i]),na.rm=T))
  Auldew_leg_orders<-rbind(Auldew_leg_orders,Auldew_leg_order)
  rm(Auldew_leg_order)
}

Auldew_leg_orders<-as.data.frame(Auldew_leg_orders)

colnames(Auldew_leg_orders)<-c("cause","mean")

Auldew_leg_orders$mean<-as.numeric(Auldew_leg_orders$mean)

Auldew_leg_orders<-Auldew_leg_orders[order(Auldew_leg_orders$mean,decreasing=T),]

for(i in 1:length(Auldew_leg_orders$cause)){
  Auldew_leg_orders$cause[i]<-sp_food_coltp0[Auldew_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_A_dew$cause)){
  dataframe_Smap_A_dew$cause[i]<-sp_food_coltp0[dataframe_Smap_A_dew$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_A_dew$cause<-factor(dataframe_Smap_A_dew$cause,levels=c(Auldew_leg_orders$cause))

Smap_A_dew<-ggplot(data=dataframe_Smap_A_dew, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Aul. dew")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),plot.background = element_rect(fill = "transparent",color = NA)  #ここを追加
  )+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_A_dew$Smap,na.rm=T)-0.375),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_A_dew$Smap,na.rm=T)-0.375),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_A_dew$Smap,na.rm=T),max(dataframe_Smap_A_dew$Smap,na.rm=T)),
                  clip = "off")


ggsave(file = "Smap_A_dew.pdf", plot = Smap_A_dew, dpi = 100, width = 10, height = 5)


Julorn_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_J_orn$cause))){
  Julorn_leg_order<-c(unique(dataframe_Smap_J_orn$cause)[i],mean(subset(dataframe_Smap_J_orn$Smap,dataframe_Smap_J_orn$cause==unique(dataframe_Smap_J_orn$cause)[i]),na.rm=T))
  Julorn_leg_orders<-rbind(Julorn_leg_orders,Julorn_leg_order)
  rm(Julorn_leg_order)
}

Julorn_leg_orders<-as.data.frame(Julorn_leg_orders)

colnames(Julorn_leg_orders)<-c("cause","mean")

Julorn_leg_orders$mean<-as.numeric(Julorn_leg_orders$mean)

Julorn_leg_orders<-Julorn_leg_orders[order(Julorn_leg_orders$mean,decreasing=T),]

for(i in 1:length(Julorn_leg_orders$cause)){
  Julorn_leg_orders$cause[i]<-sp_food_coltp0[Julorn_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_J_orn$cause)){
  dataframe_Smap_J_orn$cause[i]<-sp_food_coltp0[dataframe_Smap_J_orn$cause[i]==sp_food_coltp0$cause,]$re.namesp
}


Smap_J_orn<-ggplot(data=dataframe_Smap_J_orn, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Jul. orn")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_J_orn$Smap,na.rm=T)-1),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_J_orn$Smap,na.rm=T)-1),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_J_orn$Smap,na.rm=T),max(dataframe_Smap_J_orn$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_J_orn.pdf", plot = Smap_J_orn, dpi = 100, width = 10, height = 5)



Neofas_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_N_fas$cause))){
  Neofas_leg_order<-c(unique(dataframe_Smap_N_fas$cause)[i],mean(subset(dataframe_Smap_N_fas$Smap,dataframe_Smap_N_fas$cause==unique(dataframe_Smap_N_fas$cause)[i]),na.rm=T))
  Neofas_leg_orders<-rbind(Neofas_leg_orders,Neofas_leg_order)
  rm(Neofas_leg_order)
}

Neofas_leg_orders<-as.data.frame(Neofas_leg_orders)

colnames(Neofas_leg_orders)<-c("cause","mean")

Neofas_leg_orders$mean<-as.numeric(Neofas_leg_orders$mean)

Neofas_leg_orders<-Neofas_leg_orders[order(Neofas_leg_orders$mean,decreasing=T),]

for(i in 1:length(Neofas_leg_orders$cause)){
  Neofas_leg_orders$cause[i]<-sp_food_coltp0[Neofas_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_N_fas$cause)){
  dataframe_Smap_N_fas$cause[i]<-sp_food_coltp0[dataframe_Smap_N_fas$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_N_fas$cause<-factor(dataframe_Smap_N_fas$cause,levels=c(Neofas_leg_orders$cause))

Smap_N_fas<-ggplot(data=dataframe_Smap_N_fas, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Neo. fas")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_N_fas$Smap,na.rm=T)-0.55),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_N_fas$Smap,na.rm=T)-0.55),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_N_fas$Smap,na.rm=T),max(dataframe_Smap_N_fas$Smap,na.rm=T)),
                  clip = "off")


ggsave(file = "Smap_N_fas.pdf", plot = Smap_N_fas, dpi = 100, width = 10, height = 5)



Parspp_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_Par$cause))){
  Parspp_leg_order<-c(unique(dataframe_Smap_Par$cause)[i],mean(subset(dataframe_Smap_Par$Smap,dataframe_Smap_Par$cause==unique(dataframe_Smap_Par$cause)[i]),na.rm=T))
  Parspp_leg_orders<-rbind(Parspp_leg_orders,Parspp_leg_order)
  rm(Parspp_leg_order)
}

Parspp_leg_orders<-as.data.frame(Parspp_leg_orders)

colnames(Parspp_leg_orders)<-c("cause","mean")

Parspp_leg_orders$mean<-as.numeric(Parspp_leg_orders$mean)

Parspp_leg_orders<-Parspp_leg_orders[order(Parspp_leg_orders$mean,decreasing=T),]

for(i in 1:length(Parspp_leg_orders$cause)){
  Parspp_leg_orders$cause[i]<-sp_food_coltp0[Parspp_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_Par$cause)){
  dataframe_Smap_Par$cause[i]<-sp_food_coltp0[dataframe_Smap_Par$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_Par$cause<-factor(dataframe_Smap_Par$cause,levels=c(Parspp_leg_orders$cause))


Smap_Par<-ggplot(data=dataframe_Smap_Par, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Par. spp")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_Par$Smap,na.rm=T)-0.3),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_Par$Smap,na.rm=T)-0.3),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_Par$Smap,na.rm=T),max(dataframe_Smap_Par$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_Par.pdf", plot = Smap_Par, dpi = 100, width = 10, height = 5)



Tromoo_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_T_moo$cause))){
  Tromoo_leg_order<-c(unique(dataframe_Smap_T_moo$cause)[i],mean(subset(dataframe_Smap_T_moo$Smap,dataframe_Smap_T_moo$cause==unique(dataframe_Smap_T_moo$cause)[i]),na.rm=T))
  Tromoo_leg_orders<-rbind(Tromoo_leg_orders,Tromoo_leg_order)
  rm(Tromoo_leg_order)
}

Tromoo_leg_orders<-as.data.frame(Tromoo_leg_orders)

colnames(Tromoo_leg_orders)<-c("cause","mean")

Tromoo_leg_orders$mean<-as.numeric(Tromoo_leg_orders$mean)

Tromoo_leg_orders<-Tromoo_leg_orders[order(Tromoo_leg_orders$mean,decreasing=T),]

for(i in 1:length(Tromoo_leg_orders$cause)){
  Tromoo_leg_orders$cause[i]<-sp_food_coltp0[Tromoo_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_T_moo$cause)){
  dataframe_Smap_T_moo$cause[i]<-sp_food_coltp0[dataframe_Smap_T_moo$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_T_moo$cause<-factor(dataframe_Smap_T_moo$cause,levels=c(Tromoo_leg_orders$cause))





Smap_T_moo<-ggplot(data=dataframe_Smap_T_moo, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Tro. moo")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_T_moo$Smap,na.rm=T)-0.257),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_T_moo$Smap,na.rm=T)-0.257),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_T_moo$Smap,na.rm=T),max(dataframe_Smap_T_moo$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_T_moo.pdf", plot = Smap_T_moo, dpi = 100, width = 10, height = 5)






Intloo_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_I_loo$cause))){
  Intloo_leg_order<-c(unique(dataframe_Smap_I_loo$cause)[i],mean(subset(dataframe_Smap_I_loo$Smap,dataframe_Smap_I_loo$cause==unique(dataframe_Smap_I_loo$cause)[i]),na.rm=T))
  Intloo_leg_orders<-rbind(Intloo_leg_orders,Intloo_leg_order)
  rm(Intloo_leg_order)
}

Intloo_leg_orders<-as.data.frame(Intloo_leg_orders)

colnames(Intloo_leg_orders)<-c("cause","mean")

Intloo_leg_orders$mean<-as.numeric(Intloo_leg_orders$mean)

Intloo_leg_orders<-Intloo_leg_orders[order(Intloo_leg_orders$mean,decreasing=T),]

for(i in 1:length(Intloo_leg_orders$cause)){
  Intloo_leg_orders$cause[i]<-sp_food_coltp0[Intloo_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_I_loo$cause)){
  dataframe_Smap_I_loo$cause[i]<-sp_food_coltp0[dataframe_Smap_I_loo$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_I_loo$cause<-factor(dataframe_Smap_I_loo$cause,levels=c(Intloo_leg_orders$cause))

Smap_I_loo<-ggplot(data=dataframe_Smap_I_loo, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Int. loo")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_I_loo$Smap,na.rm=T)-0.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_I_loo$Smap,na.rm=T)-0.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_I_loo$Smap,na.rm=T),max(dataframe_Smap_I_loo$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_I_loo.pdf", plot = Smap_I_loo, dpi = 100, width = 10, height = 5)


Lamcol_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_L_cal$cause))){
  Lamcol_leg_order<-c(unique(dataframe_Smap_L_cal$cause)[i],mean(subset(dataframe_Smap_L_cal$Smap,dataframe_Smap_L_cal$cause==unique(dataframe_Smap_L_cal$cause)[i]),na.rm=T))
  Lamcol_leg_orders<-rbind(Lamcol_leg_orders,Lamcol_leg_order)
  rm(Lamcol_leg_order)
}

Lamcol_leg_orders<-as.data.frame(Lamcol_leg_orders)

colnames(Lamcol_leg_orders)<-c("cause","mean")

Lamcol_leg_orders$mean<-as.numeric(Lamcol_leg_orders$mean)

Lamcol_leg_orders<-Lamcol_leg_orders[order(Lamcol_leg_orders$mean,decreasing=T),]

for(i in 1:length(Lamcol_leg_orders$cause)){
  Lamcol_leg_orders$cause[i]<-sp_food_coltp0[Lamcol_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_L_cal$cause)){
  dataframe_Smap_L_cal$cause[i]<-sp_food_coltp0[dataframe_Smap_L_cal$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_L_cal$cause<-factor(dataframe_Smap_L_cal$cause,levels=c(Lamcol_leg_orders$cause))

Smap_L_cal<-ggplot(data=dataframe_Smap_L_cal, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Lam. cal")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_L_cal$Smap,na.rm=T)-0.7),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_L_cal$Smap,na.rm=T)-0.7),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_cal$Smap,na.rm=T),max(dataframe_Smap_L_cal$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_L_cal.pdf", plot = Smap_L_cal, dpi = 100, width = 10, height = 5)



Lepelo_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_L_elo$cause))){
  Lepelo_leg_order<-c(unique(dataframe_Smap_L_elo$cause)[i],mean(subset(dataframe_Smap_L_elo$Smap,dataframe_Smap_L_elo$cause==unique(dataframe_Smap_L_elo$cause)[i]),na.rm=T))
  Lepelo_leg_orders<-rbind(Lepelo_leg_orders,Lepelo_leg_order)
  rm(Lepelo_leg_order)
}

Lepelo_leg_orders<-as.data.frame(Lepelo_leg_orders)

colnames(Lepelo_leg_orders)<-c("cause","mean")

Lepelo_leg_orders$mean<-as.numeric(Lepelo_leg_orders$mean)

Lepelo_leg_orders<-Lepelo_leg_orders[order(Lepelo_leg_orders$mean,decreasing=T),]

for(i in 1:length(Lepelo_leg_orders$cause)){
  Lepelo_leg_orders$cause[i]<-sp_food_coltp0[Lepelo_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_L_elo$cause)){
  dataframe_Smap_L_elo$cause[i]<-sp_food_coltp0[dataframe_Smap_L_elo$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_L_elo$cause<-factor(dataframe_Smap_L_elo$cause,levels=c(Lepelo_leg_orders$cause))

Smap_L_elo<-ggplot(data=dataframe_Smap_L_elo, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Lep. elo")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_L_elo$Smap,na.rm=T)-1.63),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_L_elo$Smap,na.rm=T)-1.63),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_elo$Smap,na.rm=T),max(dataframe_Smap_L_elo$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_L_elo.pdf", plot = Smap_L_elo, dpi = 100, width = 10, height = 5)


#L_lemairii(eff)-A_dewindti(cau)


dataframe_Smap_L_lem$cause<-gsub(unique(dataframe_Smap_L_lem$cause),sp_food_coltp0[unique(dataframe_Smap_L_lem$cause)==sp_food_coltp0$cause,]$re.namesp,dataframe_Smap_L_lem$cause)

Smap_L_lem<-ggplot(data=dataframe_Smap_L_lem, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Lam. lem")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_L_lem$Smap,na.rm=T)-2.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_L_lem$Smap,na.rm=T)-2.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_lem$Smap,na.rm=T),max(dataframe_Smap_L_lem$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_L_lem.pdf", plot = Smap_L_lem, dpi = 100, width = 10, height = 5)


Neosex_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_N_sex$cause))){
  Neosex_leg_order<-c(unique(dataframe_Smap_N_sex$cause)[i],mean(subset(dataframe_Smap_N_sex$Smap,dataframe_Smap_N_sex$cause==unique(dataframe_Smap_N_sex$cause)[i]),na.rm=T))
  Neosex_leg_orders<-rbind(Neosex_leg_orders,Neosex_leg_order)
  rm(Neosex_leg_order)
}

Neosex_leg_orders<-as.data.frame(Neosex_leg_orders)

colnames(Neosex_leg_orders)<-c("cause","mean")

Neosex_leg_orders$mean<-as.numeric(Neosex_leg_orders$mean)

Neosex_leg_orders<-Neosex_leg_orders[order(Neosex_leg_orders$mean,decreasing=T),]

for(i in 1:length(Neosex_leg_orders$cause)){
  Neosex_leg_orders$cause[i]<-sp_food_coltp0[Neosex_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_N_sex$cause)){
  dataframe_Smap_N_sex$cause[i]<-sp_food_coltp0[dataframe_Smap_N_sex$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_N_sex$cause<-factor(dataframe_Smap_N_sex$cause,levels=c(Neosex_leg_orders$cause))

Smap_N_sex<-ggplot(data=dataframe_Smap_N_sex, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Neo. sex")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_N_sex$Smap,na.rm=T)-0.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_N_sex$Smap,na.rm=T)-0.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_N_sex$Smap,na.rm=T),max(dataframe_Smap_N_sex$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_N_sex.pdf", plot = Smap_N_sex, dpi = 100, width = 10, height = 5)



Ophven_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_O_ven$cause))){
  Ophven_leg_order<-c(unique(dataframe_Smap_O_ven$cause)[i],mean(subset(dataframe_Smap_O_ven$Smap,dataframe_Smap_O_ven$cause==unique(dataframe_Smap_O_ven$cause)[i]),na.rm=T))
  Ophven_leg_orders<-rbind(Ophven_leg_orders,Ophven_leg_order)
  rm(Ophven_leg_order)
}

Ophven_leg_orders<-as.data.frame(Ophven_leg_orders)

colnames(Ophven_leg_orders)<-c("cause","mean")

Ophven_leg_orders$mean<-as.numeric(Ophven_leg_orders$mean)

Ophven_leg_orders<-Ophven_leg_orders[order(Ophven_leg_orders$mean,decreasing=T),]

for(i in 1:length(Ophven_leg_orders$cause)){
  Ophven_leg_orders$cause[i]<-sp_food_coltp0[Ophven_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_O_ven$cause)){
  dataframe_Smap_O_ven$cause[i]<-sp_food_coltp0[dataframe_Smap_O_ven$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_O_ven$cause<-factor(dataframe_Smap_O_ven$cause,levels=c(Ophven_leg_orders$cause))

Smap_O_ven<-ggplot(data=dataframe_Smap_O_ven, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Oph. ven")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_O_ven$Smap,na.rm=T)-0.27),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_O_ven$Smap,na.rm=T)-0.27),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_O_ven$Smap,na.rm=T),max(dataframe_Smap_O_ven$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_O_ven.pdf", plot = Smap_O_ven, dpi = 100, width = 10, height = 5)



Permic_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_P_mic$cause))){
  Permic_leg_order<-c(unique(dataframe_Smap_P_mic$cause)[i],mean(subset(dataframe_Smap_P_mic$Smap,dataframe_Smap_P_mic$cause==unique(dataframe_Smap_P_mic$cause)[i]),na.rm=T))
  Permic_leg_orders<-rbind(Permic_leg_orders,Permic_leg_order)
  rm(Permic_leg_order)
}

Permic_leg_orders<-as.data.frame(Permic_leg_orders)

colnames(Permic_leg_orders)<-c("cause","mean")

Permic_leg_orders$mean<-as.numeric(Permic_leg_orders$mean)

Permic_leg_orders<-Permic_leg_orders[order(Permic_leg_orders$mean,decreasing=T),]

for(i in 1:length(Permic_leg_orders$cause)){
  Permic_leg_orders$cause[i]<-sp_food_coltp0[Permic_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_P_mic$cause)){
  dataframe_Smap_P_mic$cause[i]<-sp_food_coltp0[dataframe_Smap_P_mic$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_P_mic$cause<-factor(dataframe_Smap_P_mic$cause,levels=c(Permic_leg_orders$cause))

Smap_P_mic<-ggplot(data=dataframe_Smap_P_mic, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Per. mic")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_P_mic$Smap,na.rm=T)-0.29),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_P_mic$Smap,na.rm=T)-0.29),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_P_mic$Smap,na.rm=T),max(dataframe_Smap_P_mic$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_P_mic.pdf", plot = Smap_P_mic, dpi = 100, width = 10, height = 5)



Pettre_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_P_tre$cause))){
  Pettre_leg_order<-c(unique(dataframe_Smap_P_tre$cause)[i],mean(subset(dataframe_Smap_P_tre$Smap,dataframe_Smap_P_tre$cause==unique(dataframe_Smap_P_tre$cause)[i]),na.rm=T))
  Pettre_leg_orders<-rbind(Pettre_leg_orders,Pettre_leg_order)
  rm(Pettre_leg_order)
}

Pettre_leg_orders<-as.data.frame(Pettre_leg_orders)

colnames(Pettre_leg_orders)<-c("cause","mean")

Pettre_leg_orders$mean<-as.numeric(Pettre_leg_orders$mean)

Pettre_leg_orders<-Pettre_leg_orders[order(Pettre_leg_orders$mean,decreasing=T),]

for(i in 1:length(Pettre_leg_orders$cause)){
  Pettre_leg_orders$cause[i]<-sp_food_coltp0[Pettre_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_P_tre$cause)){
  dataframe_Smap_P_tre$cause[i]<-sp_food_coltp0[dataframe_Smap_P_tre$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_P_tre$cause<-factor(dataframe_Smap_P_tre$cause,levels=c(Pettre_leg_orders$cause))

Smap_P_tre<-ggplot(data=dataframe_Smap_P_tre, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Pet. tre")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_P_tre$Smap,na.rm=T)-0.18),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_P_tre$Smap,na.rm=T)-0.18),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_P_tre$Smap,na.rm=T),max(dataframe_Smap_P_tre$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_P_tre.pdf", plot = Smap_P_tre, dpi = 100, width = 10, height = 5)



Telvit_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_T_vit$cause))){
  Telvit_leg_order<-c(unique(dataframe_Smap_T_vit$cause)[i],mean(subset(dataframe_Smap_T_vit$Smap,dataframe_Smap_T_vit$cause==unique(dataframe_Smap_T_vit$cause)[i]),na.rm=T))
  Telvit_leg_orders<-rbind(Telvit_leg_orders,Telvit_leg_order)
  rm(Telvit_leg_order)
}

Telvit_leg_orders<-as.data.frame(Telvit_leg_orders)

colnames(Telvit_leg_orders)<-c("cause","mean")

Telvit_leg_orders$mean<-as.numeric(Telvit_leg_orders$mean)

Telvit_leg_orders<-Telvit_leg_orders[order(Telvit_leg_orders$mean,decreasing=T),]

for(i in 1:length(Telvit_leg_orders$cause)){
  Telvit_leg_orders$cause[i]<-sp_food_coltp0[Telvit_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_T_vit$cause)){
  dataframe_Smap_T_vit$cause[i]<-sp_food_coltp0[dataframe_Smap_T_vit$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_T_vit$cause<-factor(dataframe_Smap_T_vit$cause,levels=c(Telvit_leg_orders$cause))

Smap_T_vit<-ggplot(data=dataframe_Smap_T_vit, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Tel. vit")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_T_vit$Smap,na.rm=T)-0.23),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_T_vit$Smap,na.rm=T)-0.23),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_T_vit$Smap,na.rm=T),max(dataframe_Smap_T_vit$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_T_vit.pdf", plot = Smap_T_vit, dpi = 100, width = 10, height = 5)



Limdar_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_L_dar$cause))){
  Limdar_leg_order<-c(unique(dataframe_Smap_L_dar$cause)[i],mean(subset(dataframe_Smap_L_dar$Smap,dataframe_Smap_L_dar$cause==unique(dataframe_Smap_L_dar$cause)[i]),na.rm=T))
  Limdar_leg_orders<-rbind(Limdar_leg_orders,Limdar_leg_order)
  rm(Limdar_leg_order)
}

Limdar_leg_orders<-as.data.frame(Limdar_leg_orders)

colnames(Limdar_leg_orders)<-c("cause","mean")

Limdar_leg_orders$mean<-as.numeric(Limdar_leg_orders$mean)

Limdar_leg_orders<-Limdar_leg_orders[order(Limdar_leg_orders$mean,decreasing=T),]

for(i in 1:length(Limdar_leg_orders$cause)){
  Limdar_leg_orders$cause[i]<-sp_food_coltp0[Limdar_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_L_dar$cause)){
  dataframe_Smap_L_dar$cause[i]<-sp_food_coltp0[dataframe_Smap_L_dar$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_L_dar$cause<-factor(dataframe_Smap_L_dar$cause,levels=c(Limdar_leg_orders$cause))

Smap_L_dar<-ggplot(data=dataframe_Smap_L_dar, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  ggtitle("Lim. dar")+
  xlab("Year")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_L_dar$Smap,na.rm=T)-0.5),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_L_dar$Smap,na.rm=T)-0.5),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_dar$Smap,na.rm=T),max(dataframe_Smap_L_dar$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_L_dar.pdf", plot = Smap_L_dar, dpi = 100, width = 10, height = 5)



Lamtan_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_L_tan$cause))){
  Lamtan_leg_order<-c(unique(dataframe_Smap_L_tan$cause)[i],mean(subset(dataframe_Smap_L_tan$Smap,dataframe_Smap_L_tan$cause==unique(dataframe_Smap_L_tan$cause)[i]),na.rm=T))
  Lamtan_leg_orders<-rbind(Lamtan_leg_orders,Lamtan_leg_order)
  rm(Lamtan_leg_order)
}

Lamtan_leg_orders<-as.data.frame(Lamtan_leg_orders)

colnames(Lamtan_leg_orders)<-c("cause","mean")

Lamtan_leg_orders$mean<-as.numeric(Lamtan_leg_orders$mean)

Lamtan_leg_orders<-Lamtan_leg_orders[order(Lamtan_leg_orders$mean,decreasing=T),]

for(i in 1:length(Lamtan_leg_orders$cause)){
  Lamtan_leg_orders$cause[i]<-sp_food_coltp0[Lamtan_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_L_tan$cause)){
  dataframe_Smap_L_tan$cause[i]<-sp_food_coltp0[dataframe_Smap_L_tan$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_L_tan$cause<-factor(dataframe_Smap_L_tan$cause,levels=c(Lamtan_leg_orders$cause))

Smap_L_tan<-ggplot(data=dataframe_Smap_L_tan, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Lam. tan")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_L_tan$Smap,na.rm=T)-0.46),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_L_tan$Smap,na.rm=T)-0.46),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_tan$Smap,na.rm=T),max(dataframe_Smap_L_tan$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_L_tan.pdf", plot = Smap_L_tan, dpi = 100, width = 10, height = 5)



Lepatt_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_L_att$cause))){
  Lepatt_leg_order<-c(unique(dataframe_Smap_L_att$cause)[i],mean(subset(dataframe_Smap_L_att$Smap,dataframe_Smap_L_att$cause==unique(dataframe_Smap_L_att$cause)[i]),na.rm=T))
  Lepatt_leg_orders<-rbind(Lepatt_leg_orders,Lepatt_leg_order)
  rm(Lepatt_leg_order)
}

Lepatt_leg_orders<-as.data.frame(Lepatt_leg_orders)

colnames(Lepatt_leg_orders)<-c("cause","mean")

Lepatt_leg_orders$mean<-as.numeric(Lepatt_leg_orders$mean)

Lepatt_leg_orders<-Lepatt_leg_orders[order(Lepatt_leg_orders$mean,decreasing=T),]

for(i in 1:length(Lepatt_leg_orders$cause)){
  Lepatt_leg_orders$cause[i]<-sp_food_coltp0[Lepatt_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_L_att$cause)){
  dataframe_Smap_L_att$cause[i]<-sp_food_coltp0[dataframe_Smap_L_att$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_L_att$cause<-factor(dataframe_Smap_L_att$cause,levels=c(Lepatt_leg_orders$cause))

Smap_L_att<-ggplot(data=dataframe_Smap_L_att, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Lep .att")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_L_att$Smap,na.rm=T)-0.0237),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_L_att$Smap,na.rm=T)-0.0237),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_att$Smap,na.rm=T),max(dataframe_Smap_L_att$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_L_att.pdf", plot = Smap_L_att, dpi = 100, width = 10, height = 5)


#--L_labiatus(eff)対L_att(cas)
dataframe_Smap_L_lab$cause<-gsub(unique(dataframe_Smap_L_lab$cause),sp_food_coltp0[unique(dataframe_Smap_L_lab$cause)==sp_food_coltp0$cause,]$re.namesp,dataframe_Smap_L_lab$cause)

Smap_L_lab<-ggplot(data=dataframe_Smap_L_lab, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Lob.lab")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_L_lab$Smap,na.rm=T)-2.6),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_L_lab$Smap,na.rm=T)-2.6),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_lab$Smap,na.rm=T),max(dataframe_Smap_L_lab$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_L_lab.pdf", plot = Smap_L_lab, dpi = 100, width = 10, height = 5)



Xenpap_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_X_pap$cause))){
  Xenpap_leg_order<-c(unique(dataframe_Smap_X_pap$cause)[i],mean(subset(dataframe_Smap_X_pap$Smap,dataframe_Smap_X_pap$cause==unique(dataframe_Smap_X_pap$cause)[i]),na.rm=T))
  Xenpap_leg_orders<-rbind(Xenpap_leg_orders,Xenpap_leg_order)
  rm(Xenpap_leg_order)
}

Xenpap_leg_orders<-as.data.frame(Xenpap_leg_orders)

colnames(Xenpap_leg_orders)<-c("cause","mean")

Xenpap_leg_orders$mean<-as.numeric(Xenpap_leg_orders$mean)

Xenpap_leg_orders<-Xenpap_leg_orders[order(Xenpap_leg_orders$mean,decreasing=T),]

for(i in 1:length(Xenpap_leg_orders$cause)){
  Xenpap_leg_orders$cause[i]<-sp_food_coltp0[Xenpap_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_X_pap$cause)){
  dataframe_Smap_X_pap$cause[i]<-sp_food_coltp0[dataframe_Smap_X_pap$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_X_pap$cause<-factor(dataframe_Smap_X_pap$cause,levels=c(Xenpap_leg_orders$cause))



Smap_X_pap<-ggplot(data=dataframe_Smap_X_pap, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Xen. pap")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_X_pap$Smap,na.rm=T)-2.55),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_X_pap$Smap,na.rm=T)-2.55),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_X_pap$Smap,na.rm=T),max(dataframe_Smap_X_pap$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_X_pap.pdf", plot = Smap_X_pap, dpi = 100, width = 10, height = 5)




Varmoo_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_V_moo$cause))){
  Varmoo_leg_order<-c(unique(dataframe_Smap_V_moo$cause)[i],mean(subset(dataframe_Smap_V_moo$Smap,dataframe_Smap_V_moo$cause==unique(dataframe_Smap_V_moo$cause)[i]),na.rm=T))
  Varmoo_leg_orders<-rbind(Varmoo_leg_orders,Varmoo_leg_order)
  rm(Varmoo_leg_order)
}

Varmoo_leg_orders<-as.data.frame(Varmoo_leg_orders)

colnames(Varmoo_leg_orders)<-c("cause","mean")

Varmoo_leg_orders$mean<-as.numeric(Varmoo_leg_orders$mean)

Varmoo_leg_orders<-Varmoo_leg_orders[order(Varmoo_leg_orders$mean,decreasing=T),]

for(i in 1:length(Varmoo_leg_orders$cause)){
  Varmoo_leg_orders$cause[i]<-sp_food_coltp0[Varmoo_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_V_moo$cause)){
  dataframe_Smap_V_moo$cause[i]<-sp_food_coltp0[dataframe_Smap_V_moo$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_V_moo$cause<-factor(dataframe_Smap_V_moo$cause,levels=c(Varmoo_leg_orders$cause))





Smap_V_moo<-ggplot(data=dataframe_Smap_V_moo, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Var. moo")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_V_moo$Smap,na.rm=T)-0.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_V_moo$Smap,na.rm=T)-0.35),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_V_moo$Smap,na.rm=T),max(dataframe_Smap_V_moo$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_V_moo.pdf", plot = Smap_V_moo, dpi = 100, width = 10, height = 5)



Petpol_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_P_pol$cause))){
  Petpol_leg_order<-c(unique(dataframe_Smap_P_pol$cause)[i],mean(subset(dataframe_Smap_P_pol$Smap,dataframe_Smap_P_pol$cause==unique(dataframe_Smap_P_pol$cause)[i]),na.rm=T))
  Petpol_leg_orders<-rbind(Petpol_leg_orders,Petpol_leg_order)
  rm(Petpol_leg_order)
}

Petpol_leg_orders<-as.data.frame(Petpol_leg_orders)

colnames(Petpol_leg_orders)<-c("cause","mean")

Petpol_leg_orders$mean<-as.numeric(Petpol_leg_orders$mean)

Petpol_leg_orders<-Petpol_leg_orders[order(Petpol_leg_orders$mean,decreasing=T),]

for(i in 1:length(Petpol_leg_orders$cause)){
  Petpol_leg_orders$cause[i]<-sp_food_coltp0[Petpol_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_P_pol$cause)){
  dataframe_Smap_P_pol$cause[i]<-sp_food_coltp0[dataframe_Smap_P_pol$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_P_pol$cause<-factor(dataframe_Smap_P_pol$cause,levels=c(Petpol_leg_orders$cause))

Smap_P_pol<-ggplot(data=dataframe_Smap_P_pol, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Pet. pol")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_P_pol$Smap,na.rm=T)-0.41),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_P_pol$Smap,na.rm=T)-0.41),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_P_pol$Smap,na.rm=T),max(dataframe_Smap_P_pol$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_P_pol.pdf", plot = Smap_P_pol, dpi = 100, width = 10, height = 5)



Synmul_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_S_mul$cause))){
  Synmul_leg_order<-c(unique(dataframe_Smap_S_mul$cause)[i],mean(subset(dataframe_Smap_S_mul$Smap,dataframe_Smap_S_mul$cause==unique(dataframe_Smap_S_mul$cause)[i]),na.rm=T))
  Synmul_leg_orders<-rbind(Synmul_leg_orders,Synmul_leg_order)
  rm(Synmul_leg_order)
}

Synmul_leg_orders<-as.data.frame(Synmul_leg_orders)

colnames(Synmul_leg_orders)<-c("cause","mean")

Synmul_leg_orders$mean<-as.numeric(Synmul_leg_orders$mean)

Synmul_leg_orders<-Synmul_leg_orders[order(Synmul_leg_orders$mean,decreasing=T),]

for(i in 1:length(Synmul_leg_orders$cause)){
  Synmul_leg_orders$cause[i]<-sp_food_coltp0[Synmul_leg_orders$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

for(i in 1:length(dataframe_Smap_S_mul$cause)){
  dataframe_Smap_S_mul$cause[i]<-sp_food_coltp0[dataframe_Smap_S_mul$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_S_mul$cause<-factor(dataframe_Smap_S_mul$cause,levels=c(Synmul_leg_orders$cause))


Smap_S_mul<-ggplot(data=dataframe_Smap_S_mul, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("Syn. mul")+
  theme(plot.title=element_text(size=rel(2),colour="black"))+
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,40,1), labels = yearslabel)+
  theme_classic()+
  theme(plot.title = element_text(face = "italic"),text=element_text(family="Times"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(legend.text =element_text(family="Times",face = "italic"),
        plot.background = element_rect(fill = "transparent",color = NA))+
  annotate(geom = "text",
           x = 10, y = c(min(dataframe_Smap_S_mul$Smap,na.rm=T)-0.925),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(min(dataframe_Smap_S_mul$Smap,na.rm=T)-0.925),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_S_mul$Smap,na.rm=T),max(dataframe_Smap_S_mul$Smap,na.rm=T)),
                  clip = "off")

ggsave(file = "Smap_S_mul.pdf", plot = Smap_S_mul, dpi = 100, width = 10, height = 5)



library(dplyr)

forTable<-read.csv("int.csv",header=F)

dataset<-read.csv("Fig,Table/csv/CCM_list_all.csv",header=T)
swapped_dataset <- dataset %>%
  mutate(swapped_cause = effect, swapped_effect = cause)

# 元のデータフレームと交換したデータフレームを結合し、一致するレコードを抽出
matched_records <- dataset %>%
  inner_join(swapped_dataset, by = c("cause" = "swapped_cause", "effect" = "swapped_effect"))

positive_strength_records <- matched_records %>%
  filter(strength.x == "positive" & strength.y == "positive")

write.csv(positive_strength_records,"positive_strength_both.csv")

negative_strength_records <- matched_records %>%
  filter(strength.x == "negative" & strength.y == "negative")

write.csv(negative_strength_records,"negative_strength_both.csv")

positive_negative_strength_records <- matched_records %>%
  filter(strength.x == "positive" & strength.y == "negative")

write.csv(positive_negative_strength_records,"positive_negative_strength.csv")


negative_positive_strength_records <- matched_records %>%
  filter(strength.x == "negative" & strength.y == "positive")

write.csv(negative_positive_strength_records,"negative_positive_strength.csv")

alt_data_index <- as.numeric(dataset$X) %>%
  setdiff(as.numeric(matched_records$X.x))

alt_data<-dataset[alt_data_index,]

alt_data_positive<-subset(alt_data,alt_data$strength=="positive")

write.csv(alt_data_positive,"positive_strength_alt.csv")

alt_data_negative<-subset(alt_data,alt_data$strength=="negative")

write.csv(alt_data_negative,"negative_strength_alt.csv")







correlation_Hori$cause<-gsub("[[:blank:]]", "",correlation_Hori$cause)
correlation_Hori$effect<-gsub("[[:blank:]]", "",correlation_Hori$effect)


hori_foodweb_interaction$cause<-gsub("[[:blank:]]", "",hori_foodweb_interaction$cause)
hori_foodweb_interaction$effect<-gsub("[[:blank:]]", "",hori_foodweb_interaction$effect)



keynegative<-paste(negative_strength_records$cause,negative_strength_records$effect,sep="")
reverseKeynegative<-paste(negative_strength_records$effect,negative_strength_records$cause,sep="")
negative_strength_recordsWKey<-cbind(negative_strength_records,keynegative,reverseKeynegative)

colnames(negative_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")


negative_strength_recordsWKey<-as.data.frame(cbind(negative_strength_recordsWKey,NA))

colnames(negative_strength_recordsWKey)<-c(colnames(negative_strength_recordsWKey)[1:60],"match")

negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Predator-Prey",paste(negative_strength_recordsWKey$match,"Predator-Prey",sep=",")),negative_strength_recordsWKey$match)

negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Prey-Predator",paste(negative_strength_recordsWKey$match,"Prey-Predator",sep=",")),negative_strength_recordsWKey$match)


keyPositive<-paste(positive_strength_records$cause,positive_strength_records$effect,sep="")
reverseKeyPositive<-paste(positive_strength_records$effect,positive_strength_records$cause,sep="")
positive_strength_recordsWKey<-cbind(positive_strength_records,keyPositive,reverseKeyPositive)

colnames(positive_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")

positive_strength_recordsWKey<-as.data.frame(cbind(positive_strength_recordsWKey,NA))

colnames(positive_strength_recordsWKey)<-c(colnames(positive_strength_recordsWKey)[1:60],"match")

positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Predator-Prey",paste(positive_strength_recordsWKey$match,"Predator-Prey",sep=",")),positive_strength_recordsWKey$match)

positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Prey-Predator",paste(positive_strength_recordsWKey$match,"Prey-Predator",sep=",")),positive_strength_recordsWKey$match)


keynegative_positive<-paste(negative_positive_strength_records$cause,negative_positive_strength_records$effect,sep="")
reverseKeynegative_positive<-paste(negative_positive_strength_records$effect,negative_positive_strength_records$cause,sep="")
negative_positive_strength_recordsWKey<-cbind(negative_positive_strength_records,keynegative_positive,reverseKeynegative_positive)

colnames(negative_positive_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")



negative_positive_strength_recordsWKey<-as.data.frame(cbind(negative_positive_strength_recordsWKey,NA))

colnames(negative_positive_strength_recordsWKey)<-c(colnames(negative_positive_strength_recordsWKey)[1:60],"match")

negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                            ifelse(is.na(negative_positive_strength_recordsWKey$match),"Predator-Prey",paste(negative_positive_strength_recordsWKey$match,"Predator-Prey",sep=",")),negative_positive_strength_recordsWKey$match)

negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                            ifelse(is.na(negative_positive_strength_recordsWKey$match),"Prey-Predator",paste(negative_positive_strength_recordsWKey$match,"Prey-Predator",sep=",")),negative_positive_strength_recordsWKey$match)


keypositive_negative<-paste(positive_negative_strength_records$cause,positive_negative_strength_records$effect,sep="")
reverseKeypositive_negative<-paste(positive_negative_strength_records$effect,positive_negative_strength_records$cause,sep="")
positive_negative_strength_recordsWKey<-cbind(positive_negative_strength_records,keypositive_negative,reverseKeypositive_negative)

colnames(positive_negative_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")


positive_negative_strength_recordsWKey<-as.data.frame(cbind(positive_negative_strength_recordsWKey,NA))

colnames(positive_negative_strength_recordsWKey)<-c(colnames(positive_negative_strength_recordsWKey)[1:60],"match")

positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Predator-Prey",paste(positive_negative_strength_recordsWKey$match,"Predator-Prey",sep=",")),positive_negative_strength_recordsWKey$match)

positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Prey-Predator",paste(positive_negative_strength_recordsWKey$match,"Prey-Predator",sep=",")),positive_negative_strength_recordsWKey$match)


keypositive_negative<-paste(alt_data_negative$cause,alt_data_negative$effect,sep="")
reverseKeypositive_negative<-paste(alt_data_negative$effect,alt_data_negative$cause,sep="")
alt_data_negativeWKey<-cbind(alt_data_negative,keypositive_negative,reverseKeypositive_negative)

colnames(alt_data_negativeWKey)[c(30,31)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")

alt_data_negativeWKey<-as.data.frame(cbind(alt_data_negativeWKey,NA))

colnames(alt_data_negativeWKey)<-c(colnames(alt_data_negativeWKey)[1:31],"match")

alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_foodweb_interactionWKey$key,
                                                     ifelse(is.na(alt_data_negativeWKey$match),"Predator-Prey",paste(alt_data_negativeWKey$match,"Predator-Prey",sep=",")),alt_data_negativeWKey$match)

alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                                     ifelse(is.na(alt_data_negativeWKey$match),"Prey-Predator",paste(alt_data_negativeWKey$match,"Prey-Predator",sep=",")),alt_data_negativeWKey$match)




keypositive_alt_positive<-paste(alt_data_positive$cause,alt_data_positive$effect,sep="")
reverseKeypositive_alt_positive<-paste(alt_data_positive$effect,alt_data_positive$cause,sep="")
alt_data_positiveWKey<-cbind(alt_data_positive,keypositive_alt_positive,reverseKeypositive_alt_positive)

colnames(alt_data_positiveWKey)[c(30,31)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")


alt_data_positiveWKey<-as.data.frame(cbind(alt_data_positiveWKey,NA))

colnames(alt_data_positiveWKey)<-c(colnames(alt_data_positiveWKey)[1:31],"match")

alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_foodweb_interactionWKey$key,
                                    ifelse(is.na(alt_data_positiveWKey$match),"Predator-Prey",paste(alt_data_positiveWKey$match,"Predator-Prey",sep=",")),alt_data_positiveWKey$match)

alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                    ifelse(is.na(alt_data_positiveWKey$match),"Prey-Predator",paste(alt_data_positiveWKey$match,"Prey-Predator",sep=",")),alt_data_positiveWKey$match)


negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Aggressor-Victim",paste(negative_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),negative_strength_recordsWKey$match)

negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Victim-Aggressor",paste(negative_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),negative_strength_recordsWKey$match)

write.csv(negative_strength_recordsWKey,"negative_strength_recordsWKey.csv")


positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Aggressor-Victim",paste(positive_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),positive_strength_recordsWKey$match)

positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Victim-Aggressor",paste(positive_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),positive_strength_recordsWKey$match)

write.csv(positive_strength_recordsWKey,"positive_strength_recordsWKey.csv")



negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                            ifelse(is.na(negative_positive_strength_recordsWKey$match),"Aggressor-Victim",paste(negative_positive_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),negative_positive_strength_recordsWKey$match)

negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                            ifelse(is.na(negative_positive_strength_recordsWKey$match),"Victim-Aggressor",paste(negative_positive_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),negative_positive_strength_recordsWKey$match)

write.csv(negative_positive_strength_recordsWKey,"negative_positive_strength_recordsWKey.csv")


positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Aggressor-Victim",paste(positive_negative_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),positive_negative_strength_recordsWKey$match)

positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Victim-Aggressor",paste(positive_negative_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),positive_negative_strength_recordsWKey$match)


write.csv(positive_negative_strength_recordsWKey,"positive_negative_strength_recordsWKey.csv")


alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_Agg_interactionWKey$key,
                                    ifelse(is.na(alt_data_negativeWKey$match),"Aggressor-Victim",paste(alt_data_negativeWKey$match,"Aggressor-Victim",sep=",")),alt_data_negativeWKey$match)

alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                    ifelse(is.na(alt_data_negativeWKey$match),"Victim-Aggressor",paste(alt_data_negativeWKey$match,"Victim-Aggressor",sep=",")),alt_data_negativeWKey$match)

write.csv(alt_data_negativeWKey,"alt_data_negativeWKey.csv")



alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_Agg_interactionWKey$key,
                                                     ifelse(is.na(alt_data_positiveWKey$match),"Aggressor-Victim",paste(alt_data_positiveWKey$match,"Aggressor-Victim",sep=",")),alt_data_positiveWKey$match)

alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                                     ifelse(is.na(alt_data_positiveWKey$match),"Victim-Aggressor",paste(alt_data_positiveWKey$match,"Victim-Aggressor",sep=",")),alt_data_positiveWKey$match)

write.csv(alt_data_positiveWKey,"alt_data_positiveWKey.csv")




#TableS2
library(dplyr)

forTable<-read.csv("int.csv",header=F)

dataset<-read.csv("CCM_list_all.csv",header=T)
swapped_dataset <- dataset %>%
  mutate(swapped_cause = effect, swapped_effect = cause)

# 元のデータフレームと交換したデータフレームを結合し、一致するレコードを抽出
matched_records <- dataset %>%
  inner_join(swapped_dataset, by = c("cause" = "swapped_cause", "effect" = "swapped_effect"))

positive_strength_records <- matched_records %>%
  filter(strength.x == "positive" & strength.y == "positive")

write.csv(positive_strength_records,"positive_strength_both.csv")

negative_strength_records <- matched_records %>%
  filter(strength.x == "negative" & strength.y == "negative")

write.csv(negative_strength_records,"negative_strength_both.csv")

positive_negative_strength_records <- matched_records %>%
  filter(strength.x == "positive" & strength.y == "negative")

write.csv(positive_negative_strength_records,"positive_negative_strength.csv")


negative_positive_strength_records <- matched_records %>%
  filter(strength.x == "negative" & strength.y == "positive")

write.csv(negative_positive_strength_records,"negative_positive_strength.csv")

alt_data_index <- as.numeric(dataset$X) %>%
  setdiff(as.numeric(matched_records$X.x))

alt_data<-dataset[alt_data_index,]

alt_data_positive<-subset(alt_data,alt_data$strength=="positive")

write.csv(alt_data_positive,"positive_strength_alt.csv")

alt_data_negative<-subset(alt_data,alt_data$strength=="negative")

write.csv(alt_data_negative,"negative_strength_alt.csv")





#全てのcausalityと線形性、埋め込みなどについてのTable

correlation_Hori$cause<-gsub("[[:blank:]]", "",correlation_Hori$cause)
correlation_Hori$effect<-gsub("[[:blank:]]", "",correlation_Hori$effect)


hori_foodweb_interaction$cause<-gsub("[[:blank:]]", "",hori_foodweb_interaction$cause)
hori_foodweb_interaction$effect<-gsub("[[:blank:]]", "",hori_foodweb_interaction$effect)



keynegative<-paste(negative_strength_records$cause,negative_strength_records$effect,sep="")
reverseKeynegative<-paste(negative_strength_records$effect,negative_strength_records$cause,sep="")
negative_strength_recordsWKey<-cbind(negative_strength_records,keynegative,reverseKeynegative)

colnames(negative_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")


negative_strength_recordsWKey<-as.data.frame(cbind(negative_strength_recordsWKey,NA))

colnames(negative_strength_recordsWKey)<-c(colnames(negative_strength_recordsWKey)[1:60],"match")

negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Predator-Prey",paste(negative_strength_recordsWKey$match,"Predator-Prey",sep=",")),negative_strength_recordsWKey$match)

negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Prey-Predator",paste(negative_strength_recordsWKey$match,"Prey-Predator",sep=",")),negative_strength_recordsWKey$match)


keyPositive<-paste(positive_strength_records$cause,positive_strength_records$effect,sep="")
reverseKeyPositive<-paste(positive_strength_records$effect,positive_strength_records$cause,sep="")
positive_strength_recordsWKey<-cbind(positive_strength_records,keyPositive,reverseKeyPositive)

colnames(positive_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")

positive_strength_recordsWKey<-as.data.frame(cbind(positive_strength_recordsWKey,NA))

colnames(positive_strength_recordsWKey)<-c(colnames(positive_strength_recordsWKey)[1:60],"match")

positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Predator-Prey",paste(positive_strength_recordsWKey$match,"Predator-Prey",sep=",")),positive_strength_recordsWKey$match)

positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Prey-Predator",paste(positive_strength_recordsWKey$match,"Prey-Predator",sep=",")),positive_strength_recordsWKey$match)


keynegative_positive<-paste(negative_positive_strength_records$cause,negative_positive_strength_records$effect,sep="")
reverseKeynegative_positive<-paste(negative_positive_strength_records$effect,negative_positive_strength_records$cause,sep="")
negative_positive_strength_recordsWKey<-cbind(negative_positive_strength_records,keynegative_positive,reverseKeynegative_positive)

colnames(negative_positive_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")



negative_positive_strength_recordsWKey<-as.data.frame(cbind(negative_positive_strength_recordsWKey,NA))

colnames(negative_positive_strength_recordsWKey)<-c(colnames(negative_positive_strength_recordsWKey)[1:60],"match")

negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                                     ifelse(is.na(negative_positive_strength_recordsWKey$match),"Predator-Prey",paste(negative_positive_strength_recordsWKey$match,"Predator-Prey",sep=",")),negative_positive_strength_recordsWKey$match)

negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                                     ifelse(is.na(negative_positive_strength_recordsWKey$match),"Prey-Predator",paste(negative_positive_strength_recordsWKey$match,"Prey-Predator",sep=",")),negative_positive_strength_recordsWKey$match)


keypositive_negative<-paste(positive_negative_strength_records$cause,positive_negative_strength_records$effect,sep="")
reverseKeypositive_negative<-paste(positive_negative_strength_records$effect,positive_negative_strength_records$cause,sep="")
positive_negative_strength_recordsWKey<-cbind(positive_negative_strength_records,keypositive_negative,reverseKeypositive_negative)

colnames(positive_negative_strength_recordsWKey)[c(59,60)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")


positive_negative_strength_recordsWKey<-as.data.frame(cbind(positive_negative_strength_recordsWKey,NA))

colnames(positive_negative_strength_recordsWKey)<-c(colnames(positive_negative_strength_recordsWKey)[1:60],"match")

positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$key,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Predator-Prey",paste(positive_negative_strength_recordsWKey$match,"Predator-Prey",sep=",")),positive_negative_strength_recordsWKey$match)

positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Prey-Predator",paste(positive_negative_strength_recordsWKey$match,"Prey-Predator",sep=",")),positive_negative_strength_recordsWKey$match)


keypositive_negative<-paste(alt_data_negative$cause,alt_data_negative$effect,sep="")
reverseKeypositive_negative<-paste(alt_data_negative$effect,alt_data_negative$cause,sep="")
alt_data_negativeWKey<-cbind(alt_data_negative,keypositive_negative,reverseKeypositive_negative)

colnames(alt_data_negativeWKey)[c(30,31)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")

alt_data_negativeWKey<-as.data.frame(cbind(alt_data_negativeWKey,NA))

colnames(alt_data_negativeWKey)<-c(colnames(alt_data_negativeWKey)[1:31],"match")

alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_foodweb_interactionWKey$key,
                                    ifelse(is.na(alt_data_negativeWKey$match),"Predator-Prey",paste(alt_data_negativeWKey$match,"Predator-Prey",sep=",")),alt_data_negativeWKey$match)

alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                    ifelse(is.na(alt_data_negativeWKey$match),"Prey-Predator",paste(alt_data_negativeWKey$match,"Prey-Predator",sep=",")),alt_data_negativeWKey$match)




keypositive_alt_positive<-paste(alt_data_positive$cause,alt_data_positive$effect,sep="")
reverseKeypositive_alt_positive<-paste(alt_data_positive$effect,alt_data_positive$cause,sep="")
alt_data_positiveWKey<-cbind(alt_data_positive,keypositive_alt_positive,reverseKeypositive_alt_positive)

colnames(alt_data_positiveWKey)[c(30,31)]<-c("key","reverseKey")

hori_foodweb_Key<-paste(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect,sep="")
hori_foodweb_Reverse_Key<-paste(hori_foodweb_interaction$effect,hori_foodweb_interaction$cause,sep="")
hori_foodweb_interactionWKey<-cbind(hori_foodweb_interaction,hori_foodweb_Key,hori_foodweb_Reverse_Key)

colnames(hori_foodweb_interactionWKey)[c(5,6)]<-c("key","reverseKey")


alt_data_positiveWKey<-as.data.frame(cbind(alt_data_positiveWKey,NA))

colnames(alt_data_positiveWKey)<-c(colnames(alt_data_positiveWKey)[1:31],"match")

alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_foodweb_interactionWKey$key,
                                    ifelse(is.na(alt_data_positiveWKey$match),"Predator-Prey",paste(alt_data_positiveWKey$match,"Predator-Prey",sep=",")),alt_data_positiveWKey$match)

alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_foodweb_interactionWKey$reverseKey,
                                    ifelse(is.na(alt_data_positiveWKey$match),"Prey-Predator",paste(alt_data_positiveWKey$match,"Prey-Predator",sep=",")),alt_data_positiveWKey$match)


negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Aggressor-Victim",paste(negative_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),negative_strength_recordsWKey$match)

negative_strength_recordsWKey$match<-ifelse(negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                            ifelse(is.na(negative_strength_recordsWKey$match),"Victim-Aggressor",paste(negative_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),negative_strength_recordsWKey$match)

write.csv(negative_strength_recordsWKey,"negative_strength_recordsWKey.csv")


positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Aggressor-Victim",paste(positive_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),positive_strength_recordsWKey$match)

positive_strength_recordsWKey$match<-ifelse(positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                            ifelse(is.na(positive_strength_recordsWKey$match),"Victim-Aggressor",paste(positive_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),positive_strength_recordsWKey$match)

write.csv(positive_strength_recordsWKey,"positive_strength_recordsWKey.csv")



negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                                     ifelse(is.na(negative_positive_strength_recordsWKey$match),"Aggressor-Victim",paste(negative_positive_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),negative_positive_strength_recordsWKey$match)

negative_positive_strength_recordsWKey$match<-ifelse(negative_positive_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                                     ifelse(is.na(negative_positive_strength_recordsWKey$match),"Victim-Aggressor",paste(negative_positive_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),negative_positive_strength_recordsWKey$match)

write.csv(negative_positive_strength_recordsWKey,"negative_positive_strength_recordsWKey.csv")


positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$key,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Aggressor-Victim",paste(positive_negative_strength_recordsWKey$match,"Aggressor-Victim",sep=",")),positive_negative_strength_recordsWKey$match)

positive_negative_strength_recordsWKey$match<-ifelse(positive_negative_strength_recordsWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                                     ifelse(is.na(positive_negative_strength_recordsWKey$match),"Victim-Aggressor",paste(positive_negative_strength_recordsWKey$match,"Victim-Aggressor",sep=",")),positive_negative_strength_recordsWKey$match)


write.csv(positive_negative_strength_recordsWKey,"positive_negative_strength_recordsWKey.csv")


alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_Agg_interactionWKey$key,
                                    ifelse(is.na(alt_data_negativeWKey$match),"Aggressor-Victim",paste(alt_data_negativeWKey$match,"Aggressor-Victim",sep=",")),alt_data_negativeWKey$match)

alt_data_negativeWKey$match<-ifelse(alt_data_negativeWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                    ifelse(is.na(alt_data_negativeWKey$match),"Victim-Aggressor",paste(alt_data_negativeWKey$match,"Victim-Aggressor",sep=",")),alt_data_negativeWKey$match)

write.csv(alt_data_negativeWKey,"alt_data_negativeWKey.csv")



alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_Agg_interactionWKey$key,
                                    ifelse(is.na(alt_data_positiveWKey$match),"Aggressor-Victim",paste(alt_data_positiveWKey$match,"Aggressor-Victim",sep=",")),alt_data_positiveWKey$match)

alt_data_positiveWKey$match<-ifelse(alt_data_positiveWKey$key %in% hori_Agg_interactionWKey$reverseKey,
                                    ifelse(is.na(alt_data_positiveWKey$match),"Victim-Aggressor",paste(alt_data_positiveWKey$match,"Victim-Aggressor",sep=",")),alt_data_positiveWKey$match)

write.csv(alt_data_positiveWKey,"alt_data_positiveWKey.csv")




#FigS3






histList<-list()
for(i in 1 : length(unique(ForIgraph1$effect.x))){
  histList[[i]]<-ForIgraph1[ForIgraph1$cause.x==unique(ForIgraph1$effect.x)[i],]
}

histList[[23]]<-ForIgraph1[ForIgraph1$cause.x=="Gna. pfe",]

histList[[24]]<-ForIgraph1[ForIgraph1$cause.x=="Tel. tem",]


checkList<-NULL
for(i in 1 :length(colnames(zzz))){
  checkList[i]<-sp_food_coltp0[colnames(zzz)[i]==sp_food_coltp0$cause,]$re.namesp
}

foodEffectList <- list()

for (i in 1:length(histList)) {
  foodEffectList[[i]] <- numeric(nrow(histList[[i]]))
    for (j in 1:nrow(histList[[i]])) {
      foodEffectList[[i]][j] <- sp_food_coltp0[sp_food_coltp0$re.namesp == histList[[i]]$effect.x[j],]$foodhabit7
    }
}


histEachAllList<-list()
histAllList<-list()

for(i in 1:length(histList)){
  histEachAllList[[i]]<-cbind(histList[[i]],foodEffectList[[i]])
  colnames(histEachAllList[[i]])<-c("Strength","ratio","effect","cause","Smap_sum","Neg","Pos","CauseFoodHabit","EffectFoodHabit")
  histAllList<-rbind(histAllList,histEachAllList[[i]])
}
histAllList<-as.data.frame(histAllList[,c(-6,-7)])
histNegList<-subset(histAllList,histAllList$Strength=="Negative")
histPosList<-subset(histAllList,histAllList$Strength=="Positive")


histPosList_count <- dplyr::group_by(histPosList, cause, EffectFoodHabit) %>%
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")


histPosList_count_sum<-NULL
for(i in 1:length(unique(histPosList_count$cause))){
  sum<-sum(histPosList_count[unique(histPosList_count$cause)[i]==histPosList_count$cause,]$Count)
  name<-unique(histPosList_count$cause)[i]
  food<-sp_food_coltp0[name==sp_food_coltp0$re.namesp,]$foodhabit7
  each<-cbind(name,food,sum)
  histPosList_count_sum<-rbind(histPosList_count_sum,each)
}

histPosList_count_sum<-as.data.frame(histPosList_count_sum)

histPosList_count_sum$sum<-as.numeric(histPosList_count_sum$sum)

histPosList_count_sum<-histPosList_count_sum[order(histPosList_count_sum$sum),]

histPosList_count_level<-NULL
for(i in 1:length(unique(histPosList_count_sum$food))){
  histPosList_count_level<-rbind(histPosList_count_level,histPosList_count_sum[unique(histPosList_count_sum$food)[i]==histPosList_count_sum$food,])
}

histPosList_count$cause <- factor(histPosList_count$cause, levels = histPosList_count_level$name)

histPosList_count_col<-NULL
for(i in 1:nrow(histPosList_count)){
  histPosList_count_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==histPosList_count$EffectFoodHabit[i],]$col7)
}

histPosList_count<-cbind(histPosList_count,histPosList_count_col)

histPosList_count$Count<-as.numeric(histPosList_count$Count)

names(histPosList_count)<-c("cause","EffectFoodHabit","Count","col")

histPosList_count$col<-gsub("white","black",histPosList_count$col)

xlabColPos<-NULL
for(i in 1:length(histPosList_count_level$name)){
  xlabColPos[i]<-sp_food_coltp0[histPosList_count_level$name[i]==sp_food_coltp0$re.namesp,]$col7
}

xlabColPos<-gsub("white","black",xlabColPos)


histPosList_count$EffectFoodHabit <- factor(histPosList_count$EffectFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

ggplot(histPosList_count, aes(x = cause, y = Count, fill = EffectFoodHabit)) +
  geom_bar(stat = "identity") +
  labs(title = "The feeding habits of influencing species positively by each species",
       x = "Cause species",
       y = "Count of causes for each species ") +
      scale_y_continuous(limits = c(0, 11)) +
  theme(legend.position = "right",axis.text.x = element_text(angle = 45, hjust = 1,color=xlabColPos))+
  scale_fill_manual(values = setNames(histPosList_count$col, histPosList_count$EffectFoodHabit))+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))


ggsave("The_count_of_influencing_species_with_positive_causality_by_each_species.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)
#-------histgramについて(原因側の数で受けて色正のみ)










histNegList_count <- dplyr::group_by(histNegList, cause, EffectFoodHabit) %>%
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")



histNegList_count_sum<-NULL
for(i in 1:length(unique(histNegList_count$cause))){
  sum<-sum(histNegList_count[unique(histNegList_count$cause)[i]==histNegList_count$cause,]$Count)
  name<-unique(histNegList_count$cause)[i]
  food<-sp_food_coltp0[name==sp_food_coltp0$re.namesp,]$foodhabit7
  each<-cbind(name,food,sum)
  histNegList_count_sum<-rbind(histNegList_count_sum,each)
}

histNegList_count_sum<-as.data.frame(histNegList_count_sum)

histNegList_count_sum$sum<-as.numeric(histNegList_count_sum$sum)

histNegList_count_sum<-histNegList_count_sum[order(histNegList_count_sum$sum),]

histNegList_count_level<-NULL
for(i in 1:length(unique(histNegList_count_sum$food))){
  histNegList_count_level<-rbind(histNegList_count_level,histNegList_count_sum[unique(histNegList_count_sum$food)[i]==histNegList_count_sum$food,])
}

histNegList_count$cause <- factor(histNegList_count$cause, levels = histNegList_count_level$name)

histNegList_count_col<-NULL
for(i in 1:nrow(histNegList_count)){
  histNegList_count_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==histNegList_count$EffectFoodHabit[i],]$col7)
}

histNegList_count<-cbind(histNegList_count,histNegList_count_col)

histNegList_count$Count<-as.numeric(histNegList_count$Count)

names(histNegList_count)<-c("cause","EffectFoodHabit","Count","col")

histNegList_count$col<-gsub("white","black",histNegList_count$col)


xlabColNeg<-NULL
for(i in 1:length(histNegList_count_level$name)){
  xlabColNeg[i]<-sp_food_coltp0[histNegList_count_level$name[i]==sp_food_coltp0$re.namesp,]$col7
}

xlabColNeg<-gsub("white","black",xlabColNeg)

histNegList_count$EffectFoodHabit <- factor(histNegList_count$EffectFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))




bothdata_histPosList_Effect<-cbind(histPosList_count,rep("Positive",nrow(histPosList_count)))

colnames(bothdata_histPosList_Effect)<-c("cause","FoodHabitat","Count","Side")

bothdata_histNegList_Effect<-cbind(histNegList_count,rep("Negative",nrow(histNegList_count)))

colnames(bothdata_histNegList_Effect)<-c("cause","FoodHabitat","Count","Side")

bothdata_histNegList_Effect$Count<-bothdata_histNegList_Effect$Count*-1

bothdata_histList_count_Cause<-rbind(bothdata_histPosList_Effect,bothdata_histNegList_Effect)

bothdata_histList_count_Cause_col<-NULL
for(i in 1:nrow(bothdata_histList_count_Cause)){
  bothdata_histList_count_Cause_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==bothdata_histList_count_Cause$FoodHabitat[i],]$col7)
}

bothdata_histList_count_Cause<-cbind(bothdata_histList_count_Cause,bothdata_histList_count_Cause_col)

bothdata_histList_count_Cause$Count<-as.numeric(bothdata_histList_count_Cause$Count)

names(bothdata_histList_count_Cause)<-c("effect","EffectFoodHabit","Count","col","Side","col2")

bothdata_histList_count_Cause$col<-gsub("white","black",bothdata_histList_count_Cause$col)


#色の変更をすること!
xlabColCauseAll<-c("lightgreen","lightgreen","lightgreen","gray","gray","gray","gray","gray","gray","gray","gray","red","red","pink","pink","darkorange","darkorange","blueviolet","royalblue1","royalblue1","royalblue1","pink","red","darkorange")

bothdata_histList_count_Cause$EffectFoodHabit <- factor(bothdata_histList_count_Cause$EffectFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

FigS4a<-ggplot(bothdata_histList_count_Cause, aes(x = effect, y = Count, fill = EffectFoodHabit)) +
  geom_bar(stat = "identity") +
  labs(title = "(a) The feeding habits of influencing species by each species",
       x = "Cause species",
       y = "Count of effect for each species ") +
  scale_y_continuous(limits = c(-10, 10)) +
  guides(fill = "none") +
  theme(legend.position = "right",axis.text.x = element_text(angle = 90, hjust = 1,color=xlabColCauseAll))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_fill_manual(values = setNames(bothdata_histList_count_Cause$col, bothdata_histList_count_Cause$EffectFoodHabit),)+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))



ggsave("FigS3a.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)

#-------3/4histgramについて(原因側の色・正負を上下に分けたもの)

histListEffect<-list()
for(i in 1 : length(unique(ForIgraph1$effect.x))){
  histListEffect[[i]]<-ForIgraph1[ForIgraph1$effect.x==unique(ForIgraph1$effect.x)[i],]
}

#Gna. pfe,Tel. temは結果側には出ない

checkList<-NULL
for(i in 1 :length(colnames(zzz))){
  checkList[i]<-sp_food_coltp0[colnames(zzz)[i]==sp_food_coltp0$cause,]$re.namesp
}

foodCauseList <- list()

for (i in 1:length(histListEffect)) {
  foodCauseList[[i]] <- numeric(nrow(histListEffect[[i]]))
  for (j in 1:nrow(histListEffect[[i]])) {
    foodCauseList[[i]][j] <- sp_food_coltp0[sp_food_coltp0$re.namesp == histListEffect[[i]]$cause.x[j],]$foodhabit7
  }
}


histEachAllListEffect<-list()
histAllListEffect<-list()

for(i in 1:length(histListEffect)){
  histEachAllListEffect[[i]]<-cbind(histListEffect[[i]],foodCauseList[[i]])
  colnames(histEachAllListEffect[[i]])<-c("Strength","ratio","effect","cause","Smap_sum","Neg","Pos","CauseFoodHabit","EffectFoodHabit")
  histAllListEffect<-rbind(histAllListEffect,histEachAllListEffect[[i]])
}
histAllListEffect<-as.data.frame(histAllListEffect[,c(-6,-7)])

for(i in 1:nrow(histAllListEffect)){
  histAllListEffect$CauseFoodHabit[i]<- sp_food_coltp0[histAllListEffect$cause[i]==sp_food_coltp0$re.namesp,]$foodhabit7
}
  
histNegListEffect<-subset(histAllListEffect,histAllListEffect$Strength=="Negative")
histPosListEffect<-subset(histAllListEffect,histAllListEffect$Strength=="Positive")


histPosList_count_Effect <- dplyr::group_by(histPosListEffect, effect, CauseFoodHabit) %>%
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")



histPosList_count_effect_sum<-NULL
for(i in 1:length(unique(histPosList_count_Effect$effect))){
  sum<-sum(histPosList_count_Effect[unique(histPosList_count_Effect$effect)[i]==histPosList_count_Effect$effect,]$Count)
  name<-unique(histPosList_count_Effect$effect)[i]
  food<-sp_food_coltp0[name==sp_food_coltp0$re.namesp,]$foodhabit7
  each<-cbind(name,food,sum)
  histPosList_count_effect_sum<-rbind(histPosList_count_effect_sum,each)
}

histPosList_count_effect_sum<-as.data.frame(histPosList_count_effect_sum)

histPosList_count_effect_sum$sum<-as.numeric(histPosList_count_effect_sum$sum)

histPosList_count_effect_sum<-histPosList_count_effect_sum[order(histPosList_count_effect_sum$sum),]

histPosList_count_effect_level<-NULL
for(i in 1:length(unique(histPosList_count_effect_sum$food))){
  histPosList_count_effect_level<-rbind(histPosList_count_effect_level,histPosList_count_effect_sum[unique(histPosList_count_effect_sum$food)[i]==histPosList_count_effect_sum$food,])
}

histPosList_count_Effect$effect <- factor(histPosList_count_Effect$effect, levels = histPosList_count_effect_level$name)

histPosList_count_Effect_col<-NULL
for(i in 1:nrow(histPosList_count_Effect)){
  histPosList_count_Effect_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==histPosList_count_Effect$CauseFoodHabit[i],]$col7)
}

histPosList_count_Effect<-cbind(histPosList_count_Effect,histPosList_count_Effect_col)

histPosList_count_Effect$Count<-as.numeric(histPosList_count_Effect$Count)

names(histPosList_count_Effect)<-c("effect","CauseFoodHabit","Count","col")

histPosList_count_Effect$col<-gsub("white","black",histPosList_count_Effect$col)




xlabColPosEff<-NULL
for(i in 1:length(histPosList_count_effect_level$name)){
  xlabColPosEff[i]<-sp_food_coltp0[histPosList_count_effect_level$name[i]==sp_food_coltp0$re.namesp,]$col7
}

xlabColPosEff<-gsub("white","black",xlabColPosEff)

histPosList_count_Effect$CauseFoodHabit <- factor(histPosList_count_Effect$CauseFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))


ggplot(histPosList_count_Effect, aes(x = effect, y = Count, fill = CauseFoodHabit)) +
  geom_bar(stat = "identity") +
  labs(title = "The feeding habits of influenced species positively by each species",
       x = "Effect species",
       y = "Count of effect for each species ") +
  scale_y_continuous(limits = c(0, 15)) +
  theme(legend.position = "right",axis.text.x = element_text(angle = 45, hjust = 1,color=xlabColPosEff))+
  scale_fill_manual(values = setNames(histPosList_count_Effect$col, histPosList_count_Effect$CauseFoodHabit))+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))


ggsave("The_count_of_influenced_species_with_positive_causality_by_each_species.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)

#-------3/4histgramについて(結果側の色・正の因果について)




histNegList_count_Effect <- dplyr::group_by(histNegListEffect, effect, CauseFoodHabit) %>%
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")

histNegList_count_effect_sum<-NULL
for(i in 1:length(unique(histNegList_count_Effect$effect))){
  sum<-sum(histNegList_count_Effect[unique(histNegList_count_Effect$effect)[i]==histNegList_count_Effect$effect,]$Count)
  name<-unique(histNegList_count_Effect$effect)[i]
  food<-sp_food_coltp0[name==sp_food_coltp0$re.namesp,]$foodhabit7
  each<-cbind(name,food,sum)
  histNegList_count_effect_sum<-rbind(histNegList_count_effect_sum,each)
}

histNegList_count_effect_sum<-as.data.frame(histNegList_count_effect_sum)

histNegList_count_effect_sum$sum<-as.numeric(histNegList_count_effect_sum$sum)

histNegList_count_effect_sum<-histNegList_count_effect_sum[order(histNegList_count_effect_sum$sum),]

histNegList_count_effect_level<-NULL
for(i in 1:length(unique(histNegList_count_effect_sum$food))){
  histNegList_count_effect_level<-rbind(histNegList_count_effect_level,histNegList_count_effect_sum[unique(histNegList_count_effect_sum$food)[i]==histNegList_count_effect_sum$food,])
}

histNegList_count_Effect$effect <- factor(histNegList_count_Effect$effect, levels = histNegList_count_effect_level$name)

histNegList_count_Effect_col<-NULL
for(i in 1:nrow(histNegList_count_Effect)){
  histNegList_count_Effect_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==histNegList_count_Effect$CauseFoodHabit[i],]$col7)
}

histNegList_count_Effect<-cbind(histNegList_count_Effect,histNegList_count_Effect_col)

histNegList_count_Effect$Count<-as.numeric(histNegList_count_Effect$Count)

names(histNegList_count_Effect)<-c("effect","CauseFoodHabit","Count","col")

histNegList_count_Effect$col<-gsub("white","black",histNegList_count_Effect$col)

histNegList_count_Effect$CauseFoodHabit <- factor(histNegList_count_Effect$CauseFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))


bothdata_histPosList_count_Effect<-cbind(histPosList_count_Effect,rep("Positive",nrow(histPosList_count_Effect)))

colnames(bothdata_histPosList_count_Effect)<-c("effect","FoodHabitat","Count","Side")

bothdata_histNegList_count_Effect<-cbind(histNegList_count_Effect,rep("Negative",nrow(histNegList_count_Effect)))

colnames(bothdata_histNegList_count_Effect)<-c("effect","FoodHabitat","Count","Side")

bothdata_histNegList_count_Effect$Count<-bothdata_histNegList_count_Effect$Count*-1

bothdata_histList_count_Effect<-rbind(bothdata_histPosList_count_Effect,bothdata_histNegList_count_Effect)

bothdata_histList_count_Effect_col<-NULL
for(i in 1:nrow(bothdata_histList_count_Effect)){
  bothdata_histList_count_Effect_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==bothdata_histList_count_Effect$FoodHabitat[i],]$col7)
}

bothdata_histList_count_Effect<-cbind(bothdata_histList_count_Effect,bothdata_histList_count_Effect_col)

bothdata_histList_count_Effect$Count<-as.numeric(bothdata_histList_count_Effect$Count)

names(bothdata_histList_count_Effect)<-c("effect","FoodHabitat","Count","col","Side","col2")

bothdata_histList_count_Effect$col<-gsub("white","black",bothdata_histList_count_Effect$col)


xlabColEffectAll<-c("red","red","red","gray","gray","gray","gray","gray","gray","gray","gray","pink","pink","royalblue1","royalblue1","royalblue1","lightgreen","lightgreen","blueviolet","darkorange","darkorange","lightgreen")

bothdata_histList_count_Effect$FoodHabitat <- factor(bothdata_histList_count_Effect$FoodHabitat, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

#凡例はcauseFoodhabitat
FigS4b<-ggplot(bothdata_histList_count_Effect, aes(x = effect, y = Count, fill = FoodHabitat)) +
  geom_bar(stat = "identity") +
  labs(title = "(b) The feeding habits of influenced species by each species",
       x = "Effect species",
       y = "Count of effect for each species ") +
  scale_y_continuous(limits = c(-10, 10)) +
  guides(fill = "none") +
  theme(legend.position = "right",axis.text.x = element_text(angle = 90, hjust = 1,color=xlabColEffectAll))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_fill_manual(values = setNames(bothdata_histList_count_Effect$col, bothdata_histList_count_Effect$FoodHabitat))+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))
  














#FigS5


#全ての因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_All_all_digdis <- degree(igraphdatatp0ww8,mode = "all")

# 次数の頻度分布を計算
degree_freq <- table(degree_dist_All_all_digdis)
k <- as.numeric(names(degree_freq))
P_k <- as.numeric(degree_freq) / sum(degree_freq)

# 対数を取る
log_k <- log10(k)
log_P_k <- log10(P_k)

# 線形回帰を適用
fit <- lm(log_P_k ~ log_k)

# べき指数 gamma は -slope
model_summary <- summary(fit)

# 傾きとp値を抽出
slope <- model_summary$coefficients[2,1]
p_value <- model_summary$coefficients[2,4]

# プロット
plot_data <- data.frame(log_k = log_k, log_P_k = log_P_k)
powera<-ggplot(plot_data, aes(x = log_k, y = log_P_k)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black",lty=2) +
  labs(x = "log10(k)", y = "log10(P(k))")+
  annotate("text", x =0.5, y =-1.2, 
           label =paste("gamma = ",round(slope,digits = 2)) , hjust = 0, vjust = 1, size = 5)+
  ylim(min(plot_data$log_P_k),0)+
  xlim(0,max(plot_data$log_k))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  ggtitle("(a)")

ggsave("S5a.pdf",plot=power,width = 8.27, height = 5.69, units = "in")

# 結果の表示
print(summary(fit))
cat(sprintf("べき指数 γ: %.2f\n", gamma))

#ただし最小二乗法を用いた場合傾きは性も負もあり得るためその傾きの絶対値を取りマイナスをかけた値が冪乗法則のλに当たる
#大きな𝜆の値は、ネットワーク内のハブノードがさらに希少化し、ノードの次数のばらつきがより大きくなることを示唆します。つまり、ごく少数のノードが非常に多くのリンクを持ち、残りのノードはそれほど多くのリンクを持たない、より不均衡なネットワーク構造が予想されます。
#また、𝜆の値が大きい場合、ネットワーク内での情報や影響の伝播は、ごく少数の中心的なノードによって支配される可能性が高くなります。そのため、ネットワークのロバスト性や耐障害性が低下し、ハブノードの故障や攻撃に対する脆弱性が増すかもしれません。




#全ての因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_out_digdis <- degree(igraphdatatp0ww8,mode = "out")

# 次数の頻度分布を計算
degree_freq <- table(degree_dist_out_digdis)
k <- as.numeric(names(degree_freq))
P_k <- as.numeric(degree_freq) / sum(degree_freq)

# 対数を取る
log_k <- log10(k)
log_P_k <- log10(P_k)

# 線形回帰を適用
fit <- lm(log_P_k ~ log_k)

# べき指数 gamma は -slope
model_summary <- summary(fit)

# 傾きとp値を抽出
slope <- model_summary$coefficients[2,1]
p_value <- model_summary$coefficients[2,4]

# プロット
plot_data <- data.frame(log_k = log_k, log_P_k = log_P_k)
powerc<-ggplot(plot_data, aes(x = log_k, y = log_P_k)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black",lty=2) +
  labs(x = "log10(k)", y = "log10(P(k))")+
  annotate("text", x =0.5, y =-1.2, 
           label =paste("gamma = ",round(slope,digits = 2)) , hjust = 0, vjust = 1, size = 5)+
  ylim(min(plot_data$log_P_k),0)+
  xlim(0,max(plot_data$log_k))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  ggtitle("(c)")

ggsave("S5c.pdf",plot=power,width = 8.27, height = 5.69, units = "in")

# 結果の表示
print(summary(fit))
cat(sprintf("べき指数 γ: %.2f\n", gamma))




#全ての因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_in_digdis <- degree(igraphdatatp0ww8,mode = "in")

# 次数の頻度分布を計算
degree_freq <- table(degree_dist_in_digdis)
k <- as.numeric(names(degree_freq))
P_k <- as.numeric(degree_freq) / sum(degree_freq)

# 対数を取る
#1個目はInfなので除外
log_k <- log10(k)[-1]
log_P_k <- log10(P_k)[-1]

# 線形回帰を適用
fit <- lm(log_P_k ~ log_k)

# べき指数 gamma は -slope
model_summary <- summary(fit)

# 傾きとp値を抽出
slope <- model_summary$coefficients[2,1]
p_value <- model_summary$coefficients[2,4]

# プロット
plot_data <- data.frame(log_k = log_k, log_P_k = log_P_k)
powerb<-ggplot(plot_data, aes(x = log_k, y = log_P_k)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black",lty=2) +
  labs(x = "log10(k)", y = "log10(P(k))")+
  annotate("text", x =0.5, y =-1.2, 
           label =paste("gamma = ",round(slope,digits = 2)) , hjust = 0, vjust = 1, size = 5)+
  ylim(min(plot_data$log_P_k),0)+
  xlim(0,max(plot_data$log_k))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  ggtitle("(b)")

ggsave("S5b.pdf",plot=power,width = 8.27, height = 5.69, units = "in")

# 結果の表示
print(summary(fit))
cat(sprintf("べき指数 γ: %.2f\n", gamma))

top_plot<-plot_grid(powera,ncol=1)
bottom_plot<-plot_grid(powerb,powerc,ncol=2)
combined_plot<-plot_grid(top_plot,bottom_plot,ncol=1,rel_heights = c(1,1))
ggsave("FigS5.pdf",combined_plot,width = 8.27, height = 5.69, units = "in")

