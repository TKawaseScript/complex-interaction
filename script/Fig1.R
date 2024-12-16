#要確認あり(要確認で検索)

#cum_sum(abssum)が足し算して絶対値化したもの
#abs_meanが絶対値を取って平均値化したもの

#---------8/26 edge.arrow.size=0.5からedge.arrow.size=0.2に編集
#edge.curve=0.03で様子を見る
#edgeの太さを変換と矢尻のおきさを合わせたサイズにする
#popmean:20年間の観測個体数を東西に40年間に変換した後の平均個体数


################
###igraph描写###
################
library(caret)
library(tools)
library(dplyr)
library(igraph)
library(tidyverse)
library(plyr)
library(ggrepel)
library(tidygraph)
library(ggrepel)
library(ape)
library(nlme)

source("igraphplot2.R")

environment(plot.igraph2) <- asNamespace('igraph')
environment(igraph.Arrows2) <- asNamespace('igraph')

#データの読み込みと整形
#GLMbasedatatp0に因果関係リストとSmap係数などが格納されている
sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T,fileEncoding = "UTF-8")
strengthtp0<-read.csv("GLMbasedatatp0.csv",header=T)[,-c(1,9:11)]

colnames(strengthtp0)<-c("cause","effect","causepopmean","effectpopmean","causepopsd",
                         "effectpopsd","causehabitat","effecthabitat","smapmin","smapX1st",
                         "smapmedian","smapmean","smapX3rd","smapmax","strength","ratio")


intratp0<-NULL
intertp0<-NULL

for(i in 1:nrow(strengthtp0)){
  if(strengthtp0$cause[i]==strengthtp0$effect[i]){
    intratp0<-rbind(intratp0,strengthtp0[i,])
  }else{
    intertp0<-rbind(intertp0,strengthtp0[i,])
  }
}

write.csv(intertp0,"Interspecific_interaction.csv")


#原因側と結果側のまとめ
countcausetp0<-t(table(intertp0$cause))
counteffecttp0<-t(table(intertp0$effect))


forGray<-list()

for(i in 1:nrow(intertp0)){
  if(intertp0$strength[i]=="positive"){
    forGray[[i]]<-c(0,1)
  }else{
    forGray[[i]]<-c(1,0)
  }
}

forGray_data<-NULL
for(i in 1:length(forGray)){
  forGray_data<-rbind(forGray_data,forGray[[i]])
}

colnames(forGray_data)<-c("Neg","Pos")

igraph_allww8s<-cbind(intertp0,forGray_data)

igraph_posww8s<-subset(intertp0,intertp0$strength=="positive")

igraph_negww8s<-subset(intertp0,intertp0$strength=="negative")


#PosにあってNegにない種
nonEdgeNodeNeg<-setdiff(unique(c(igraph_posww8s$cause,igraph_posww8s$effect)),unique(c(igraph_negww8s$cause,igraph_negww8s$effect)))

#MegにあってPosにない種
nonEdgeNodePos<-setdiff(unique(c(igraph_negww8s$cause,igraph_negww8s$effect)),unique(c(igraph_posww8s$cause,igraph_posww8s$effect)))


igraph_posww8sadd<-rbind(igraph_posww8s,igraph_posww8s[c(1:2),])

igraph_posww8sadd$cause[c(length(igraph_posww8sadd$cause)-1,length(igraph_posww8sadd$cause))]<-nonEdgeNodePos

igraph_posww8sadd$effect[c(length(igraph_posww8sadd$effect)-1,length(igraph_posww8sadd$effect))]<-nonEdgeNodePos


igraphdatatp0posww8<-graph(t(cbind(igraph_posww8sadd$cause,igraph_posww8sadd$effect)))


SPCOLtp0posww8=NULL
for(i in 1:length(V(igraphdatatp0posww8))){
  SPCOLtp0posww8=c(SPCOLtp0posww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8)$name[i],]$col7))
}
SPCOLtp0posww8

SPFoodtp0posww8=NULL
for(i in 1:length(V(igraphdatatp0posww8))){
  SPFoodtp0posww8=c(SPFoodtp0posww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8)$name[i],]$foodhabit7))
}
SPFoodtp0posww8<-toTitleCase(SPFoodtp0posww8)

SPORDERtp0posww8=NULL
for(i in 1:length(V(igraphdatatp0posww8))){
  SPORDERtp0posww8=c(SPORDERtp0posww8,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8)$name[i],]$re.position)
}
SPORDERtp0posww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------
SPCOLtp0nameposww8=NULL
for(i in 1:length(V(igraphdatatp0posww8))){
  SPCOLtp0nameposww8=c(SPCOLtp0nameposww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8)$name[i],]$re.namesp))
}
SPCOLtp0nameposww8

SPCOLtp0popposww8=NULL
for(i in 1:length(V(igraphdatatp0posww8))){
  SPCOLtp0popposww8=c(SPCOLtp0popposww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8)$name[i],]$popmean))
}
SPCOLtp0popposww8<-as.numeric(SPCOLtp0popposww8)
SPCOLtp0popposww8

SPCOLtp0nameposww8<-gsub(" ","\n",SPCOLtp0nameposww8)

V(igraphdatatp0posww8)$name<-SPCOLtp0nameposww8

lay.crctp0posww8<-layout_in_circle(igraphdatatp0posww8,order=order(SPORDERtp0posww8))

plot(lay.crctp0posww8)

#色のためにratioを0から1の範囲にする
#変数の定義

#最小値
igraph_posww8s_ratio_min <- min(igraph_posww8sadd$ratio)-0.2

#最大値
igraph_posww8s_ratio_max <- max(igraph_posww8sadd$ratio)

#正規化
igraph_posww8sscale<-scale(igraph_posww8sadd$ratio, center = igraph_posww8s_ratio_min, scale = (igraph_posww8s_ratio_max - igraph_posww8s_ratio_min))

#Fig1a
pdf("Fig1a.pdf")
plot.igraph2(igraphdatatp0posww8,layout=lay.crctp0posww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=SPCOLtp0popposww8*0.2+3,
             edge.width=igraph_posww8sadd$smapmean,
             edge.arrow.size=igraph_posww8sadd$smapmean+0.3,
             vertex.label.color="black",
             vertex.color=alpha(SPCOLtp0posww8,0.5),
             vertex.label.dist=0,
             edge.color = rgb(0, 0, igraph_posww8sscale, alpha = igraph_posww8sadd$ratio)
)

dev.off()



igraph_negww8sadd<-rbind(igraph_negww8s,igraph_negww8s[1,])

igraph_negww8sadd$cause[length(igraph_negww8sadd$cause)]<-nonEdgeNodeNeg

igraph_negww8sadd$effect[length(igraph_negww8sadd$effect)]<-nonEdgeNodeNeg


igraphdatatp0negww8<-graph(t(cbind(igraph_negww8sadd$cause,igraph_negww8sadd$effect)))


SPCOLtp0negww8=NULL
for(i in 1:length(V(igraphdatatp0negww8))){
  SPCOLtp0negww8=c(SPCOLtp0negww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8)$name[i],]$col7))
}
SPCOLtp0negww8

SPFoodtp0negww8=NULL
for(i in 1:length(V(igraphdatatp0negww8))){
  SPFoodtp0negww8=c(SPFoodtp0negww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8)$name[i],]$foodhabit7))
}
SPFoodtp0negww8<-toTitleCase(SPFoodtp0negww8)

SPORDERtp0negww8=NULL
for(i in 1:length(V(igraphdatatp0negww8))){
  SPORDERtp0negww8=c(SPORDERtp0negww8,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8)$name[i],]$re.position)
}
SPORDERtp0negww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------
SPCOLtp0namenegww8=NULL
for(i in 1:length(V(igraphdatatp0negww8))){
  SPCOLtp0namenegww8=c(SPCOLtp0namenegww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8)$name[i],]$re.namesp))
}
SPCOLtp0namenegww8


SPCOLtp0popnegww8=NULL
for(i in 1:length(V(igraphdatatp0negww8))){
  SPCOLtp0popnegww8=c(SPCOLtp0popnegww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8)$name[i],]$popmean))
}
SPCOLtp0popnegww8<-as.numeric(SPCOLtp0popnegww8)
SPCOLtp0popnegww8

SPCOLtp0namenegww8<-gsub(" ","\n",SPCOLtp0namenegww8)

V(igraphdatatp0negww8)$name<-SPCOLtp0namenegww8

lay.crctp0negww8<-layout_in_circle(igraphdatatp0negww8,order=order(SPORDERtp0negww8))

plot(lay.crctp0negww8)

#色のためにratioを0から1の範囲にする
#変数の定義

#最小値
igraph_negww8s_ratio_min <- min(igraph_negww8s$ratio)-0.4

#最大値
igraph_negww8s_ratio_max <- max(igraph_negww8s$ratio)

#正規化
igraph_negww8sscale<-scale(igraph_negww8s$ratio, center = igraph_negww8s_ratio_min, scale = (igraph_negww8s_ratio_max - igraph_negww8s_ratio_min))




#Fig1b
pdf("Fig1b.pdf")
plot.igraph(
  igraphdatatp0negww8,
  layout = lay.crctp0negww8,
  vertex.frame.color = NA,
  edge.curved = 0.3,
  vertex.label.cex = 0.7,
  vertex.label.family = "Times",
  vertex.label.font = 4,
  vertex.size = SPCOLtp0popnegww8 * 0.2 + 3,
  edge.width = abs(igraph_negww8sadd$smapmean)+0.1,
  edge.arrow.size = abs(igraph_negww8sadd$smapmean)+0.3,
  vertex.label.color = "black",
  vertex.color = alpha(SPCOLtp0negww8, 0.5),
  vertex.label.dist = 0,
  edge.color = rgb((igraph_negww8sscale), 0, 0, alpha = igraph_negww8sadd$ratio)
)


dev.off()



hori_foodweb_interaction<-read.csv("Hori_Food_web.csv",header=T)

renameForfoodcau<-NULL
renameForfoodeff<-NULL
for(i in 1:length(igraph_allww8s$cause)){
  renameForfoodcau[i]<-sp_food_coltp0[igraph_allww8s$cause[i]==sp_food_coltp0$cause,]$re.namesp
  renameForfoodeff[i]<-sp_food_coltp0[igraph_allww8s$effect[i]==sp_food_coltp0$cause,]$re.namesp
}

nonPersistenceHorifood<-setdiff(unique(c(renameForfoodcau,renameForfoodeff)),
                                unique(c(hori_foodweb_interaction$cause,hori_foodweb_interaction$effect)))


nonPersistenceHorifoodFoodweb<-NULL
for(i in 1:length(nonPersistenceHorifood)){
  nonPersistenceHorifoodFoodweb[i]<-sp_food_coltp0[nonPersistenceHorifood[i]==sp_food_coltp0$re.namesp,]$foodhabit7
}
nonPersistenceHorifoodFoodweb

dataframHorifoodNonNode<-NULL
for(i in 1:length(nonPersistenceHorifood)){
  dataframHorifoodNonNode<-rbind(dataframHorifoodNonNode,c(nonPersistenceHorifood[i],nonPersistenceHorifood[i],nonPersistenceHorifoodFoodweb[i],0.0000001))
}

colnames(dataframHorifoodNonNode)<-colnames(hori_foodweb_interaction)

dataframeHorifoodNonNode<-rbind(hori_foodweb_interaction,dataframHorifoodNonNode)

hori_hoodweb_int_graph<-graph(t(cbind(dataframeHorifoodNonNode$cause,dataframeHorifoodNonNode$effect)))

Hori_graph_foodweb_col<-NULL
for(i in 1:length(V(hori_hoodweb_int_graph)$name)){
  Hori_graph_foodweb_col[i]<-sp_food_coltp0[sp_food_coltp0$re.namesp==V(hori_hoodweb_int_graph)$name[i],]$col7
}
Hori_graph_foodweb_col


Hori_graph_foodweb_pos<-NULL
for(i in 1:length(V(hori_hoodweb_int_graph)$name)){
  Hori_graph_foodweb_pos[i]<-sp_food_coltp0[sp_food_coltp0$re.namesp==V(hori_hoodweb_int_graph)$name[i],]$re.position
}
Hori_graph_foodweb_pos


Agg_food_fig_name<-gsub(". ",".\n",V(igraph::simplify(hori_hoodweb_int_graph))$name)

Agg_food_fig<-igraph::simplify(hori_hoodweb_int_graph)

V(Agg_food_fig)$name<-Agg_food_fig_name

lay.crctp0_Hori_foodweb<-layout_in_circle(hori_hoodweb_int_graph,order=order(Hori_graph_foodweb_pos))

#Fig1c
pdf("Fig1c.pdf")
plot.igraph2(Agg_food_fig,layout=lay.crctp0_Hori_foodweb,
     vertex.size=15,
     vertex.frame.color=NA,
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=as.numeric(dataframeHorifoodNonNode$ratio)*10,
     edge.arrow.size=as.numeric(dataframeHorifoodNonNode$ratio)*10,
     vertex.label.color="black",
     vertex.color=alpha(Hori_graph_foodweb_col,0.5),
     vertex.label.dist=0,
     edge.color="red"
     
)

dev.off()



correlation_Hori<-read.csv("Hori_aggressive_behaviors.csv")

renameForAggcau<-NULL
renameForAggeff<-NULL
for(i in 1:length(igraph_allww8s$cause)){
  renameForAggcau[i]<-sp_food_coltp0[igraph_allww8s$cause[i]==sp_food_coltp0$cause,]$re.namesp
  renameForAggeff[i]<-sp_food_coltp0[igraph_allww8s$effect[i]==sp_food_coltp0$cause,]$re.namesp
}

nonPersistenceHoriAgg<-setdiff(unique(c(renameForAggcau,renameForAggeff)),
                               unique(c(correlation_Hori$cause,correlation_Hori$effect)))


nonPersistenceHoriAggFoodweb<-NULL
for(i in 1:length(nonPersistenceHoriAgg)){
  nonPersistenceHoriAggFoodweb[i]<-sp_food_coltp0[nonPersistenceHoriAgg[i]==sp_food_coltp0$re.namesp,]$foodhabit7
}
nonPersistenceHoriAggFoodweb

dataframHoriAggNonNode<-NULL
for(i in 1:length(nonPersistenceHoriAgg)){
  dataframHoriAggNonNode<-rbind(dataframHoriAggNonNode,c(nonPersistenceHoriAgg[i],nonPersistenceHoriAgg[i],nonPersistenceHoriAggFoodweb[i],0.0000001))
}

colnames(dataframHoriAggNonNode)<-colnames(correlation_Hori)

dataframeHoriAggNonNode<-rbind(correlation_Hori,dataframHoriAggNonNode)

Hori_graph<-graph(t(cbind(dataframeHoriAggNonNode$cause,dataframeHoriAggNonNode$effect)))

Hori_graph_col<-NULL
for(i in 1:length(V(Hori_graph)$name)){
  Hori_graph_col[i]<-sp_food_coltp0[sp_food_coltp0$re.namesp==V(Hori_graph)$name[i],]$col7
}
Hori_graph_col



Hori_graph_pos<-NULL
for(i in 1:length(V(Hori_graph)$name)){
  Hori_graph_pos[i]<-sp_food_coltp0[sp_food_coltp0$re.namesp==V(Hori_graph)$name[i],]$re.position
}
Hori_graph_pos

lay.crctp0_Hori<-layout_in_circle(Hori_graph,order=order(Hori_graph_pos))



Agg_hori_fig<-igraph::simplify(Hori_graph)


V(Agg_hori_fig)$name<-gsub(". ",".\n",V(Agg_hori_fig)$name)

processHori <- preProcess(as.data.frame(correlation_Hori$ratio), method=c("range"))
processHori$rangeBounds<-c(0.8,1)
Agg_hori_fig_alpha <- predict(processHori, as.data.frame(correlation_Hori$ratio))

#Fig1d
pdf("Fig1d.pdf")
plot.igraph2(Agg_hori_fig,layout=lay.crctp0_Hori,
     vertex.size=13,
     vertex.frame.color=NA,
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=(correlation_Hori$ratio)+0.3,
     edge.arrow.size=(correlation_Hori$ratio)*0.1+0.5,
     vertex.label.color="black",
     vertex.color=alpha(Hori_graph_col,0.5),
     vertex.label.dist=0,
     edge.color=c(alpha("red",Agg_hori_fig_alpha$`correlation_Hori$ratio`)),
     rescale=FALSE
)


par(family="Times")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(SPFoodtp0posww8), col=alpha(unique(SPCOLtp0posww8),0.5),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)
dev.off()
