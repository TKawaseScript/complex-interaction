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
library(tools)

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

SPCOLtp0betww8<-betweenness(igraphdatatp0ww8)

write.csv(SPCOLtp0betww8,"betweenness.csv")

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


V(igraphdatatp0ww8)$name<-gsub(" ","\n",SPCOLtp0nameww8)

lay.crctp0ww8<-layout_in_circle(igraphdatatp0ww8,order=order(SPORDERtp0ww8))

plot(lay.crctp0ww8)

scaleRatio<-(igraph_allww8s$ratio-min(igraph_allww8s$ratio))/(max(igraph_allww8s$ratio)-min(igraph_allww8s$ratio))

edgeColor <- mapply(function(x, alpha) {
  if (x < 0.5) {
    # 赤系
    r <- 1
    g <- x / 0.5
    b <- 0
  } else if (x == 1) {
    # 若干灰色を足す
    r <- 0.95
    g <- 0.95
    b <- 0.95
  } else {
    # 青系
    r <- (1 - x) / 0.5
    g <- (1 - x) / 0.5
    b <- 1
  }
  rgb(r, g, b, alpha = alpha)
}, scaleRatio, igraph_allww8s$ratio)

pdf("Fig3.pdf")

par(family = "Times")

#Fig3(betweenness Size)
plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=log(SPCOLtp0betww8+2)*5,
             edge.width=igraph_allww8s$smapmean+0.5,
             edge.arrow.width=igraph_allww8s$smapmean+0.5,
             vertex.label.color="black",
             vertex.color=alpha(SPCOLtp0ww8,0.5),
             vertex.label.dist=0,
             edge.color=rgb(igraph_allww8s$Neg*abs(igraph_allww8s$ratio),0,igraph_allww8s$Pos*abs(igraph_allww8s$ratio),alpha=igraph_allww8s$ratio)
             
)
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(SPFoodtp0ww8), col=unique(SPCOLtp0ww8),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)


dev.off()

pdf("Fig3_white.pdf")

par(family = "Times")

#Fig3(betweenness Size)
plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=log(SPCOLtp0betww8+2)*5,
             edge.width=igraph_allww8s$smapmean+0.5,
             edge.arrow.width=igraph_allww8s$smapmean+0.5,
             vertex.label.color="black",
             vertex.color=alpha(SPCOLtp0ww8,0.5),
             vertex.label.dist=0,
             edge.color=edgeColor
             
)
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(SPFoodtp0ww8), col=unique(SPCOLtp0ww8),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)


dev.off()

