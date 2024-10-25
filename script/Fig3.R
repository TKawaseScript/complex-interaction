#透明度の設定どうするか？
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



igraphdatatp0ww8<-graph(t(cbind(igraph_allww8s$cause,igraph_allww8s$effect)))

igraphdatatp0ww8<-igraph::simplify(igraphdatatp0ww8)

SPCOLtp0ww8=NULL
for(i in 1:length(V(igraphdatatp0ww8))){
  SPCOLtp0ww8=c(SPCOLtp0ww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8)$name[i],]$col7))
}
SPCOLtp0ww8

SPFoodtp0ww8=NULL
for(i in 1:length(V(igraphdatatp0ww8))){
  SPFoodtp0ww8=c(SPFoodtp0ww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8)$name[i],]$foodhabit7))
}
SPFoodtp0ww8<-toTitleCase(SPFoodtp0ww8)

SPORDERtp0ww8=NULL
for(i in 1:length(V(igraphdatatp0ww8))){
  SPORDERtp0ww8=c(SPORDERtp0ww8,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8)$name[i],]$re.position)
}
SPORDERtp0ww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------
SPCOLtp0nameww8=NULL
for(i in 1:length(V(igraphdatatp0ww8))){
  SPCOLtp0nameww8=c(SPCOLtp0nameww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8)$name[i],]$re.namesp))
}
SPCOLtp0nameww8

SPSIZEtp0ww8=NULL
for(i in 1:length(V(igraphdatatp0ww8))){
  SPSIZEtp0ww8=c(SPSIZEtp0ww8,as.numeric(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8)$name[i],]$popmean))
}
SPSIZEtp0ww8

SPCOLtp0betww8<-betweenness(igraphdatatp0ww8)+0.01

V(igraphdatatp0ww8)$name<-gsub(" ","\n",SPCOLtp0nameww8)

lay.crctp0ww8<-layout_in_circle(igraphdatatp0ww8,order=order(SPORDERtp0ww8))

plot(lay.crctp0ww8)


#ratioの正規化
#最小値
igraph_ww8s_ratio_min <- min(igraph_allww8s$ratio)

#最大値
igraph_ww8s_ratio_max <- max(igraph_allww8s$ratio)

#正規化
igraph_ww8sscale<-scale(igraph_allww8s$ratio, center = igraph_ww8s_ratio_min, scale = (igraph_ww8s_ratio_max - igraph_ww8s_ratio_min))

#矢印の太さを指定するために-を+にする
igraph_ww8SmapMeanAbs<-abs(igraph_allww8s$smapmean)
#abs(smapMean)の正規化
#最小値
igraph_ww8s_mean_min <- min(igraph_ww8SmapMeanAbs)

#最大値
igraph_ww8s_mean_max <- max(igraph_ww8SmapMeanAbs)

#正規化
igraph_ww8sscale<-scale(igraph_ww8SmapMeanAbs, center = igraph_ww8s_mean_min, scale = (igraph_ww8s_mean_max - igraph_ww8s_mean_min))



pdf("Fig3.pdf")
par(oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),mfrow=c(1,2))

#Fig3a(population Size)
plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=SPSIZEtp0ww8*0.2,
             edge.width=igraph_ww8sscale,
             edge.arrow.width=igraph_ww8sscale,
             edge.arrow.size=0.2,
             vertex.label.color="black",
             vertex.color=SPCOLtp0ww8,
             vertex.label.dist=0,
             edge.color=rgb(igraph_allww8s$Neg,0,igraph_allww8s$Pos)
             
)


#Fig3b(betweenness Size)
plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=SPCOLtp0betww8*0.2,
             edge.width=igraph_ww8sscale,
             edge.arrow.width=igraph_ww8sscale,
             edge.arrow.size=0.2,
             vertex.label.color="black",
             vertex.color=SPCOLtp0ww8,
             vertex.label.dist=0,
             edge.color=rgb(igraph_allww8s$Neg,0,igraph_allww8s$Pos)
             
)



