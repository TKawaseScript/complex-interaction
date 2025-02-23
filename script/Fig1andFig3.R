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

write.csv(intertp0,"interspecific_interaction.csv")

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


SPFoodtp0posww8

pdf("Fig1a.pdf")

par(family = "Times")

plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=SPSIZEtp0ww8*0.2+3,
             edge.width=abs(igraph_allww8s$smapmean)+0.5,
             edge.arrow.width=abs(igraph_allww8s$smapmean)+0.5,
             vertex.label.color="black",
             vertex.color=alpha(SPCOLtp0ww8,0.5),
             vertex.label.dist=0,
             edge.color=rgb(igraph_allww8s$Neg*abs(igraph_allww8s$ratio),0,igraph_allww8s$Pos*abs(igraph_allww8s$ratio),alpha=(igraph_allww8s$ratio)*igraph_allww8s$Pos)
             
)
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(SPFoodtp0ww8), col=unique(SPCOLtp0ww8),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()


pdf("Fig1b.pdf")

par(family = "Times")

plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=SPSIZEtp0ww8*0.2+3,
             edge.width=abs(igraph_allww8s$smapmean)+0.5,
             edge.arrow.width=abs(igraph_allww8s$smapmean)+0.5,
             vertex.label.color="black",
             vertex.color=alpha(SPCOLtp0ww8,0.5),
             vertex.label.dist=0,
             edge.color=rgb(igraph_allww8s$Neg*abs(igraph_allww8s$ratio),0,igraph_allww8s$Pos*abs(igraph_allww8s$ratio),alpha=(igraph_allww8s$ratio)*igraph_allww8s$Neg)
             
)
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(SPFoodtp0ww8), col=unique(SPCOLtp0ww8),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

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
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(SPFoodtp0ww8), col=alpha(unique(SPCOLtp0ww8),0.5),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)
dev.off()





pdf("Fig3.pdf",width = 11.69,height = 8.27)

par(family = "Times")

#Fig3(betweenness Size)
plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
             vertex.frame.color=NA,
             edge.curved=0.3,
             vertex.label.cex=0.7,
             vertex.label.family="Times",
             vertex.label.font=4,
             vertex.size=sqrt(SPCOLtp0betww8+2)*2,
             edge.width=abs(igraph_allww8s$smapmean)+0.5,
             edge.arrow.width=abs(igraph_allww8s$smapmean)+0.5,
             vertex.label.color="black",
             vertex.color=alpha(SPCOLtp0ww8,0.5),
             vertex.label.dist=0,
             edge.color=rgb(igraph_allww8s$Neg*abs(igraph_allww8s$ratio),0,igraph_allww8s$Pos*abs(igraph_allww8s$ratio),alpha=igraph_allww8s$ratio)
             
)
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(SPFoodtp0ww8), col=unique(SPCOLtp0ww8),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)


dev.off()
