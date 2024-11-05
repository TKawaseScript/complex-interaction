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
strengthtp0<-read.csv("GLMbasedatatp0.csv",header=T)[,-1]


#8/26編集なぜかバックスラッシュが二重になる
#gsub("\\","",sp_food_coltp0$linerename)

strengthtp0<-strengthtp0[,c(-9:-11)]

colnames(strengthtp0)<-c("cause","effect","causepopmean","effectpopmean","causepopsd",
                         "effectpopsd","causehabitat","effecthabitat","smapmin","smapX1st",
                         "smapmedian","smapmean","smapX3rd","smapmax","strength")

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

write.csv(intertp0,"intertp0.csv")
t(countcausetp0)


#相関図を書くためのデータ化
igraphdatatp0<-graph(t(cbind(intertp0$cause,intertp0$effect)))

#nodeやedgeの色分け
#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],4]----------------------------------------------------------------

SPCOLtp0=NULL
for(i in 1:length(V(igraphdatatp0))){
  SPCOLtp0=c(SPCOLtp0,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0)$name[i],]$col7))
}
SPCOLtp0

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],8]----------------------------------------------------------------
SPORDERtp0=NULL
for(i in 1:length(V(igraphdatatp0))){
  SPORDERtp0=c(SPORDERtp0,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0)$name[i],]$re.position)
}
SPORDERtp0

EdgeCOL=NULL
for(i in 1:nrow(intertp0)){
  EdgeCOL <- c(EdgeCOL,as.character(intertp0$strength[i]))
}
EdgeCOL

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------

SPCOLtp0name=NULL
for(i in 1:length(V(igraphdatatp0))){
  SPCOLtp0name=c(SPCOLtp0name,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0)$name[i],]$re.namesp))
}
SPCOLtp0name




#判例のラベル
labels1<-unique(sp_food_coltp0$foodhabit7)

labels1[is.na(labels1)]<-"unclear"


#lay.crctp0<-c(3,32,5,
#              18,
#              38,26,14,30,
#              31,37,
#              2,4,22,19,33,
#              9,12,23,7,
#              10,11,
#              25,
#              1,13,16,17,
#              28,27,
#              6,
#              35,20,
#              24,15,34,21,29,36,
#              8)

#sort(testlay.crctp0)
#nodeの順番設定
lay.crctp0<-layout_in_circle(igraphdatatp0,order=order(SPORDERtp0))

frpostp0forcsv<-layout_in_circle(igraphdatatp0,order=order(SPORDERtp0))

sort(SPORDERtp0)
plot(lay.crctp0)

#因果関係(edge)の正負わけ
positivetp0<-NULL
negativetp0<-NULL

for(i in 1:nrow(intertp0)){
  if(intertp0[i,]$strength==c("positive")){
    positivetp0<-rbind(positivetp0,intertp0[i,])
  }else{
    negativetp0<-rbind(negativetp0,intertp0[i,])
  }
}

write.csv(negativetp0[,c(1,2)],"neglisttp=0.csv")
write.csv(positivetp0[,c(1,2)],"poslisttp=0.csv")

write.csv(table(positivetp0$cause),"positivecausetp0.csv")
write.csv(table(negativetp0$cause),"negativecausetp0.csv")

#相関図の正もしくは負だけのグラフデータ作成
igraphdatapositivetp0<-graph(t(cbind(positivetp0$cause,positivetp0$effect)))
igraphdatanegativetp0<-graph(t(cbind(negativetp0$cause,negativetp0$effect)))































#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],4]----------------------------------------------------------------

SPCOLtp0pos=NULL
for(i in 1:length(V(igraphdatapositivetp0))){
  SPCOLtp0pos=c(SPCOLtp0pos,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatapositivetp0)$name[i],]$col7))
}
SPCOLtp0pos


#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],8]----------------------------------------------------------------

SPORDERtp0pos=NULL
for(i in 1:length(V(igraphdatapositivetp0))){
  SPORDERtp0pos=c(SPORDERtp0pos,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatapositivetp0)$name[i],]$re.position)
}
SPORDERtp0pos


#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------

SPCOLtp0namepos=NULL
for(i in 1:length(V(igraphdatapositivetp0))){
  SPCOLtp0namepos=c(SPCOLtp0namepos,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatapositivetp0)$name[i],]$re.namesp))
}
SPCOLtp0namepos

#正についての相関図についての場所分け
lay.crctp0pos<-layout_in_circle(igraphdatapositivetp0,order=order(SPORDERtp0pos))

plot(lay.crctp0pos)

######################
###円形正の効果のみ###
######################
igraphdatapositivetp0cir<-igraphdatapositivetp0

V(igraphdatapositivetp0cir)$name<-gsub("_",". ",SPCOLtp0namepos)
V(igraphdatapositivetp0cir)$name<-gsub(" ","\n",SPCOLtp0namepos)



pdf("Correlation_diagram_tp0_positive.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot(igraphdatapositivetp0cir,layout=lay.crctp0pos,
     vertex.size=17,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=0.4,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0pos,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color="blue"
     
)
#title(main="positive interaction for interspecificity")
par(family="Times")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()




#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------

for(i in 1:length(V(igraphdatapositivetp0)$name)){
  V(igraphdatapositivetp0)$name[i]<-sp_food_coltp0[V(igraphdatapositivetp0)$name[i]==sp_food_coltp0$cause,]$re.namesp
}





#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],4]----------------------------------------------------------------

SPCOLtp0neg=NULL
for(i in 1:length(V(igraphdatanegativetp0))){
  SPCOLtp0neg=c(SPCOLtp0neg,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],]$col7))
}
SPCOLtp0neg



#要編集(plotの場所について)-----------------------------------------------------


#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],8]----------------------------------------------------------------

SPORDERtp0neg=NULL

for(i in 1:length(V(igraphdatanegativetp0))){
  SPORDERtp0neg=c(SPORDERtp0neg,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],]$re.position)
}
SPORDERtp0neg

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------

SPCOLtp0nameneg=NULL
for(i in 1:length(V(igraphdatanegativetp0))){
  SPCOLtp0nameneg=c(SPCOLtp0nameneg,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],]$re.namesp))
}
SPCOLtp0nameneg


lay.crctp0neg<-layout_in_circle(igraphdatanegativetp0,order=order(SPORDERtp0neg))

plot(lay.crctp0neg)


SPCOLtp0nodepopneg=vector()
for(i in 1:length(V(igraphdatanegativetp0)$name)){
  SPCOLtp0nodepopneg[i]<-popmean[which(rownames(popmean)==V(igraphdatanegativetp0)$name[i]),]
}
SPCOLtp0nodepopneg



#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------
SPCOLtp0noderenameneg=vector()
for(i in 1:length(V(igraphdatanegativetp0)$name)){
  SPCOLtp0noderenameneg[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],]$re.namesp
}
SPCOLtp0noderenameneg

V(igraphdatanegativetp0)$name<-gsub(" ","\n",SPCOLtp0noderenameneg)

V(igraphdatanegativetp0)$name<-gsub(" ","\n",SPCOLtp0nameneg)


#######################
### 円形負の効果のみ###
#######################



pdf("Correlation_diagram_tp0_negative.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot(igraphdatanegativetp0,layout=lay.crctp0neg,
     vertex.size=17,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=0.4,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0neg,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color="red"
)
#title(main="negative interaction for interspecificity")
par(family="Times")

legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()

#####################
### 円形全相互作用###
#####################

pdf("Correlation_diagram_tp0.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))


#SPCOLtp0nodesize=NULL
#for(i in 1:length(V(igraphdatatp0))){
#SPCOLtp0nodesize=c(SPCOLtp0nodesize,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0)$name[i],12])
#}
#SPCOLtp0nodesize


plot(igraphdatatp0,layout=lay.crctp0,
     vertex.size=17,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=0.4,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0,
     vertex.label.dist=0,
     
     edge.color=as.character(factor(EdgeCOL,labels = c("#ff00007F", "#0000ff7F")))
)
#title(main="negative interaction for interspecificity")

par(family="Times")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

legend("bottomleft",legend=c("negative","positive"),col=c("Red", "Blue"),lty=1,bty="n")


dev.off()




#########################################
### 相関図のnodeを平均個体数で重み付け###
#########################################
library(dplyr)

popmeandataframetp0<-data.frame("cause"=rownames(popmean),"mean"=popmean)

#2023/08/11編集多分sp_foodcoltp0の読み込みができていない？-----------------------------------------------------------------

plotrenamestp0<-data.frame("cause"=sp_food_coltp0$cause,"renames.n"=renames.n)

plotmeanpoptp01<-left_join(sp_food_coltp0,popmeandataframetp0,by="cause")

plotmeanpoptp02<-left_join(plotmeanpoptp01,plotrenamestp0,by="cause")


#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],15]----------------------------------------------------------------
popmeanrenameplottp0<-NULL
for(i in 1:length(V(igraphdatatp0)$name)){
  popmeanrenameplottp0<-c(popmeanrenameplottp0,plotmeanpoptp02[plotmeanpoptp02$cause==V(igraphdatatp0)$name[i],]$popmean)
}
popmeanrenameplottp0

popmeanrenameplotpostp0<-NULL
for(i in 1:length(V(igraphdatapositivetp0)$name)){
  popmeanrenameplotpostp0<-c(popmeanrenameplotpostp0,plotmeanpoptp02[plotmeanpoptp02$re.namesp==V(igraphdatapositivetp0)$name[i],]$popmean)
}
popmeanrenameplotpostp0


popmeanrenameplotnegtp0<-NULL
for(i in 1:length(V(igraphdatanegativetp0)$name)){
  popmeanrenameplotnegtp0<-c(popmeanrenameplotnegtp0,plotmeanpoptp02[plotmeanpoptp02$renames.n==V(igraphdatanegativetp0)$name[i],]$popmean)
}
popmeanrenameplotnegtp0



#V(igraphdatapositivetp0)$name<-gsub("_",". ",V(igraphdatapositivetp0)$name)
#V(igraphdatapositivetp0)$name<-gsub(" ","\n",V(igraphdatapositivetp0)$name)

#############################################
### 正の相関図のnodeを平均個体数で重み付け###
#############################################

V(igraphdatatp0)$name<-SPCOLtp0name
V(igraphdatatp0)$name<-gsub(" ","\n",SPCOLtp0name)

renames.n<-gsub(" ","\n",sp_food_coltp0$re.namesp)
#nodeの大きさが個体数の平均ものもの場所3枚とおも同じ

pdf("Correlation_diagram_tp0_positive_mean.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot(igraphdatapositivetp0cir,layout=lay.crctp0pos,
     vertex.size=log(popmeanrenameplotpostp0)+abs(min(log(popmeanrenameplotpostp0)))+5,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=0.4,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0pos,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color="#0000ff7F"
)
#title(main="positive interaction for interspecificity")
par(family="Times")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()



#############################################
### 負の相関図のnodeを平均個体数で重み付け###
#############################################

pdf("Correlation_diagram_tp0_negative_mean.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot(igraphdatanegativetp0,layout=lay.crctp0neg,
     vertex.size=log(popmeanrenameplotnegtp0)+abs(min(log(popmeanrenameplotnegtp0)))+5,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=0.4,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0neg,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color="#ff00007F"
)
#title(main="negative interaction for interspecificity")

legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()



###################################################
### 全相互作用相関図のnodeを平均個体数で重み付け###
###################################################


plot(igraphdatatp0,layout=lay.crctp0,
     vertex.size=log(popmeanrenameplottp0)+abs(min(log(popmeanrenameplottp0)))+5,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=0.4,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=as.character(factor(EdgeCOL,labels = c("#ff00007F", "#0000ff7F")))
     #as.numeric(factor(EdgeCOL))
)
#title(main="negative interaction for interspecificity")


legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

legend("bottomleft",legend=c("negative","positive"),col=c("Red", "Blue"),lty=1,bty="n")


dev.off()


#############################
### 全相互作用相関図のnode###
#############################

pdf("Correlation_diagram_tp0.pdf")


plot(igraphdatatp0,layout=lay.crctp0,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=0.4,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=as.character(factor(EdgeCOL,labels = c("#ff00007F", "#0000ff7F")))
     #as.numeric(factor(EdgeCOL))
)
#title(main="negative interaction for interspecificity")


legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

legend("bottomleft",legend=c("negative","positive"),col=c("Red", "Blue"),lty=1,bty="n")


dev.off()




#因果関係の正負の割合
strengthposnum<-subset(GLMbasedatatp0$strength,GLMbasedatatp0$strength=="positive")
strengthnegnum<-subset(GLMbasedatatp0$strength,GLMbasedatatp0$strength=="negative")

length(strengthposnum)/length(GLMbasedatatp0$strength)
length(strengthnegnum)/length(GLMbasedatatp0$strength)







pdf("exam_Smap.pdf")
for(i in 1:length(spsmaptp0)){
  for(j in 1:ncol(spsmaptp0[[i]][[1]])){
    plot(spsmaptp0[[i]][[1]][,j],type="l",ylab="S-map Coefficietns",xlab="Time index")
    abline(h=0)
    title(main=paste("Effect:",names(spsmaptp0)[i],"Cause:",colnames(spsmaptp0[[i]][[1]])[j]))
    abline(v=19)
  }
}

dev.off()

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



#------Smap係数描画のためのデータ整形 2023/07/23編集

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


library(gridExtra)
FigS2<-grid.arrange(Smap_A_dew,
                      Smap_I_loo,
                      Smap_J_orn,
                      Smap_L_att,
                      Smap_L_cal,
                      Smap_L_dar,
                      Smap_L_elo,
                      Smap_L_lab,
                      Smap_L_lem,
                      Smap_L_tan,
                      Smap_N_fas,
                      Smap_N_sex,
                      Smap_O_ven,
                      Smap_P_mic,
                      Smap_P_pol,
                      Smap_P_tre,
                      Smap_Par,
                      Smap_S_mul,
                      Smap_T_moo,
                      Smap_T_vit,
                      Smap_V_moo,
                      Smap_X_pap
                    ,ncol=2
)

ggsave('FigS2.pdf',FigS2, width=15, height =8, units = "cm", dpi=200)

#それぞれの種に対するSmap係数(cause)の正負の数(TRUE=Positive,FALSE=Negative) 編集:2023/07/31
TF_list_all<-list()
for(i in 1:length(SmapInterOnly)){
  TF_list_all[[i]]<-SmapInterOnly[[i]]>0
}

TF_list_all_summary<-list()
for(i in 1:length(TF_list_all)){
  TF_list_all_summary[[i]]<-summary(TF_list_all[[i]])
}

names(TF_list_all_summary)<-names(SmapInterOnly)

#$XXXXが受けて側の種,列名が因果関係の原因側の種
write.csv(capture.output(TF_list_all_summary),"TF_list_all_summary.csv")

#------Smap係数の平均値とその政府判定 2023/08/02編集
retioCount<-function(x){if(sum(x>0)/38>0.5){
  return(sum(x>0)/38)
}else{
  return(sum(x<0)/38)
}
}

judgePNmean<-function(x){if(mean(x)>0){
  return("Positive")
}else{
  return("Negative")
}
}

forEdgeGrade<-function(x){
  X<-retioCount(x)
  Y<-judgePNmean(x)
  Z<-rbind(Y,X)
  return(Z)
}


Smap_TF_list<-list()

for(i in 1:7){
Smap_TF_list[[i]]<-apply(SmapInterOnly[[i]],2,forEdgeGrade)
}

summary(SmapInterOnly[[8]]>0)
Smap_TF_list[[8]]<-rbind("Positive",sum(SmapInterOnly[[8]]>0)/38)


for(i in 9:16){
  Smap_TF_list[[i]]<-apply(SmapInterOnly[[i]],2,forEdgeGrade)
}

#--L_labiatus(eff)対L_att(cas)
summary(SmapInterOnly[[17]]>0)
Smap_TF_list[[17]]<-rbind("Positive",sum(SmapInterOnly[[17]]>0)/38)


summary(SmapInterOnly[[18]]>0)
Smap_TF_list[[18]]<-rbind("Positive",sum(SmapInterOnly[[18]]>0)/38)


for(i in 19:21){
  Smap_TF_list[[i]]<-apply(SmapInterOnly[[i]],2,forEdgeGrade)
}

summary(SmapInterOnly[[22]]>0)
Smap_TF_list[[22]]<-rbind("Positive",sum(SmapInterOnly[[22]]>0)/38)


for(i in 23:length(SmapInterOnly)){
  Smap_TF_list[[i]]<-apply(SmapInterOnly[[i]],2,forEdgeGrade)
}



names(Smap_TF_list)<-names(SmapInterOnly)


#nullのところが一列のみ
for(i in 1:length(SmapInterOnly)){
  print(ncol(SmapInterOnly[[i]]))
}






















#編集点2023/07/28---------------------------------------
#途中でrownameがないもの(cause-effectが1対1のもの)がいるためforが途中で止まる？


Smap_TF_list_t<-list()

for(i in 1:length(Smap_TF_list)){
Smap_TF_list_t[[i]]<-t(Smap_TF_list[[i]])
names(Smap_TF_list_t)[i]<-names(Smap_TF_list)[i]
}


Smap_TF_list_t_for_col<-NULL
for(i in 1:length(Smap_TF_list_t)){
  Smap_TF_list_t_for_col<-rbind(Smap_TF_list_t_for_col,Smap_TF_list_t[[i]])
}

listtp0[listtp0$effect=="L_labiatus",]

#cause側を挿入
#L_lemairii(eff)-A_dewindti(cau)

#L_lemairii(eff)-A_dewindti(cau)
rownames(Smap_TF_list_t_for_col)[73]<-"A_dewindti"
#L_labiatus(eff)-L_attenuatus(cau)
rownames(Smap_TF_list_t_for_col)[121]<-"L_attenuatus"



Smap_TF_list_t_bind<-NULL
eff<-NULL

for(i in 1:length(Smap_TF_list_t)){
  eff<-c(eff,rep(names(Smap_TF_list_t)[i],nrow(Smap_TF_list_t[[i]])))
  Smap_TF_list_t_bind<-rbind(Smap_TF_list_t_bind,Smap_TF_list_t[[i]])
  
}

rownames(Smap_TF_list_t_bind)[73]<-"A_dewindti"
rownames(Smap_TF_list_t_bind)[121]<-"L_attenuatus"

effList<-paste(eff,rownames(Smap_TF_list_t_for_col),sep="_")




Smap_TF_list_t_wKey<-as.data.frame(cbind(Smap_TF_list_t_for_col,effList,eff,rownames(Smap_TF_list_t_for_col)))

Smap_TF_list_t_wKey$V2<-as.numeric(Smap_TF_list_t_wKey$V2)

rm(eff)

colnames(Smap_TF_list_t_wKey)<-c("Strength","ratio","effect_cause","effect","cause")

forGray<-list()

for(i in 1:nrow(Smap_TF_list_t_wKey)){
if(Smap_TF_list_t_wKey$Strength[i]=="Positive"){
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



igraph_allww8s<-cbind(Smap_TF_list_t_wKeys,forGray_data)



igraph_allww8<-igraph_allww8s[-c(6,7)]

write.csv(igraph_allww8,"igraph_allww8.csv")

colnames(igraph_allww8)<-c("Strength","ratio","effect_cause","effect","cause","Smap_cum_sum","Smap_abs_mean","Neg","Pos")

igraphdatatp0ww8<-graph(t(cbind(igraph_allww8$cause,igraph_allww8$effect)))


#nodeやedgeの色分け
#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],4]----------------------------------------------------------------
SPCOLtp0ww8=NULL
for(i in 1:length(V(igraphdatatp0ww8))){
  SPCOLtp0ww8=c(SPCOLtp0ww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8)$name[i],]$col7))
}
SPCOLtp0ww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],8]----------------------------------------------------------------
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

SPCOLtp0nameww8<-gsub(" ","\n",SPCOLtp0nameww8)

V(igraphdatatp0ww8)$name<-SPCOLtp0nameww8

lay.crctp0ww8<-layout_in_circle(igraphdatatp0ww8,order=order(SPORDERtp0ww8))



#ww8(edgeの色の重みづけ:ratio,矢印の太さ:log(Smap_cum_sum))は3枚とも場所同じ
#log変換ではなく比率なり割り算で行う?





















#library("DescTools")

#for (i in 1:nrow(forGray_data)){
  #RgbToCmy(rgb((forGray_data[,1]*igraph_allww8$ratio)[i],0,(forGray_data[,2]*igraph_allww8$ratio)[i],alpha=0.5))
#}

library(caret)
process <- preProcess(as.data.frame(igraph_allww8$Smap_cum_sum), method=c("range"))
process$rangeBounds<-c(0.1,1)
diagram_tp0_ww8_cum_sum_alpha <- predict(process, as.data.frame(igraph_allww8$Smap_cum_sum))

pdf("Correlation_diagram_tp0_ww8_cum_sum.pdf")


plot(igraphdatatp0ww8,layout=lay.crctp0ww8,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     #Smap_cum_sumの中身sumしてabsに変更
     edge.width=(log(igraph_allww8$Smap_cum_sum)+abs(log(igraph_allww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.size=0.3,
     vertex.label.color="black",
     vertex.color=SPCOLtp0ww8,
     vertex.label.dist=0,
     vertex.size=log(SPSIZEtp0ww8)*5,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",128
     #        "cyan","blue","pink")
     #igraph_allww8$Smap_cum_sum)を1~0に変換したいrgb
     #edge.color=rgb((forGray_data[,1]*igraph_allww8$Smap_cum_sum),0,(forGray_data[,2]*igraph_allww8$Smap_cum_sum))
     edge.color=rgb((forGray_data[,1]*igraph_allww8$ratio),0,(forGray_data[,2]*igraph_allww8$ratio),alpha=igraph_allww8$ratio)
     #as.numeric(factor(EdgeCOL))
)
#title(main="negative interaction for interspecificity")


legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)


dev.off()


pdf("Correlation_diagram_tp0_ww8_cum_sum_edge.pdf")


plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     #Smap_cum_sumの中身sumしてabsに変更
     edge.width=(log(igraph_allww8$Smap_cum_sum)+abs(log(igraph_allww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.width=(log(igraph_allww8$Smap_cum_sum)+abs(log(igraph_allww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.size=0.3,
     vertex.label.color="black",
     vertex.color=SPCOLtp0ww8,
     vertex.label.dist=0,
     vertex.size=log(SPSIZEtp0ww8)*5,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",128
     #        "cyan","blue","pink")
     #igraph_allww8$Smap_cum_sum)を1~0に変換したいrgb
     #edge.color=rgb((forGray_data[,1]*igraph_allww8$Smap_cum_sum),0,(forGray_data[,2]*igraph_allww8$Smap_cum_sum))
     edge.color=rgb((forGray_data[,1]*igraph_allww8$ratio),0,(forGray_data[,2]*igraph_allww8$ratio),alpha=igraph_allww8$ratio)
     #as.numeric(factor(EdgeCOL))
)
#title(main="negative interaction for interspecificity")


legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)


dev.off()

#ww8(edgeの色の重みづけ:ratio,矢印の太さ:Smap_abs_mean)は3枚とも場所同じ

igraph_posww8<-subset(igraph_allww8,igraph_allww8$Strength=="Positive")

igraphdatapositivetp0cirww8<-graph(t(cbind(igraph_posww8$cause,igraph_posww8$effect)))






#nodeやedgeの色分け
#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],4]----------------------------------------------------------------
SPCOLtp0posww8=NULL
for(i in 1:length(V(igraphdatapositivetp0cirww8))){
  SPCOLtp0posww8=c(SPCOLtp0posww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatapositivetp0cirww8)$name[i],]$col7))
}
SPCOLtp0posww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],8]----------------------------------------------------------------
SPORDERtp0posww8=NULL
for(i in 1:length(V(igraphdatapositivetp0cirww8))){
  SPORDERtp0posww8=c(SPORDERtp0posww8,sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatapositivetp0cirww8)$name[i],]$re.position)
}
SPORDERtp0posww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------
SPCOLtp0nameposww8=NULL
for(i in 1:length(V(igraphdatapositivetp0cirww8))){
  SPCOLtp0nameposww8=c(SPCOLtp0nameposww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatapositivetp0cirww8)$name[i],]$re.namesp))
}
SPCOLtp0nameposww8


SPSIZEtp0nameposww8=NULL
for(i in 1:length(V(igraphdatapositivetp0cirww8))){
  SPSIZEtp0nameposww8=c(SPSIZEtp0nameposww8,as.numeric(sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatapositivetp0cirww8)$name[i],]$popmean))
}
SPSIZEtp0nameposww8

lay.crctp0posww8<-layout_in_circle(igraphdatapositivetp0cirww8,order=order(SPORDERtp0posww8))

igraphPosName<-NULL
for(i in 1:length(V(igraphdatapositivetp0cirww8)$name)){
  igraphPosName[i]<-sp_food_coltp0[V(igraphdatapositivetp0cirww8)$name[i]==sp_food_coltp0$cause,]$re.namesp
}


V(igraphdatapositivetp0cirww8)$name<-gsub(" ","\n",igraphPosName)

#ww8(edgeの色の重みづけ:ratio,矢印の太さ:log(Smap_cum_sum))は3枚とも場所同じ

pdf("Correlation_diagram_tp0_positive_ww8_cum_sum.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot(igraphdatapositivetp0cirww8,layout=lay.crctp0posww8,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     vertex.size=log(SPSIZEtp0nameposww8)*5,
     edge.width=log(igraph_posww8$Smap_cum_sum),
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0posww8,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=rgb(0,0,(igraph_posww8$ratio),alpha =igraph_posww8$ratio)
     
)
#title(main="positive interaction for interspecificity")
par(family="Times")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()


pdf("Correlation_diagram_tp0_positive_ww8_cum_sum_edge.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot.igraph2(igraphdatapositivetp0cirww8,layout=lay.crctp0posww8,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     vertex.size=log(SPSIZEtp0nameposww8)*5,
     edge.width=(log(igraph_posww8$Smap_cum_sum)+abs(log(igraph_posww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.width=(log(igraph_posww8$Smap_cum_sum)+abs(log(igraph_posww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0posww8,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=rgb(0,0,(igraph_posww8$ratio),alpha =igraph_posww8$ratio)
     
)
#title(main="positive interaction for interspecificity")
par(family="Times")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()


#ww8(edgeの色の重みづけ:ratio,矢印の太さ:log(Smap_cum_mean))は3枚とも場所同じ



igraph_negww8<-subset(igraph_allww8,igraph_allww8$Strength=="Negative")

igraphdatanegativetp0cirww8<-graph(t(cbind(igraph_negww8$cause,igraph_negww8$effect)))




rename<-gsub("\n", "", V(igraphdatanegativetp0cirww8)$name)
#nodeやedgeの色分け
#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],4]----------------------------------------------------------------
SPCOLtp0negww8=NULL
for(i in 1:length(rename)){
  SPCOLtp0negww8=c(SPCOLtp0negww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==rename[i],]$col7))
}
SPCOLtp0negww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],8]----------------------------------------------------------------
SPORDERtp0negww8=NULL
for(i in 1:length(rename)){
  SPORDERtp0negww8=c(SPORDERtp0negww8,sp_food_coltp0[sp_food_coltp0$cause==rename[i],]$re.position)
}
SPORDERtp0negww8

#2023/08/31加筆-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatanegativetp0)$name[i],13]----------------------------------------------------------------
SPCOLtp0namenegww8=NULL
for(i in 1:length(V(igraphdatanegativetp0cirww8))){
  SPCOLtp0namenegww8=c(SPCOLtp0namenegww8,as.character(sp_food_coltp0[sp_food_coltp0$cause==rename[i],]$re.namesp))
}
SPCOLtp0namenegww8


SPSIZEtp0namenegww8=NULL
for(i in 1:length(V(igraphdatanegativetp0cirww8))){
  SPSIZEtp0namenegww8=c(SPSIZEtp0namenegww8,as.numeric(sp_food_coltp0[sp_food_coltp0$cause==rename[i],]$popmean))
}
SPSIZEtp0namenegww8

lay.crctp0negww8<-layout_in_circle(igraphdatanegativetp0cirww8,order=order(SPORDERtp0negww8))



igraphNegName<-NULL
for(i in 1:length(V(igraphdatanegativetp0cirww8)$name)){
  igraphNegName[i]<-sp_food_coltp0[rename[i]==sp_food_coltp0$cause,]$re.namesp
}


V(igraphdatanegativetp0cirww8)$name<-gsub(" ","\n",igraphNegName)



#ww8(edgeの色の重みづけ:ratio,矢印の太さ:log(Smap_cum_sum))は3枚とも場所同じ

pdf("Correlation_diagram_tp0_negative_ww8_cum_sum.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))
#2023/08/07lay.crctp0posww8になっていたのでlay.crctp0negww8に変更
plot(igraphdatanegativetp0cirww8,layout=lay.crctp0negww8,
     vertex.size=log(SPSIZEtp0namenegww8)*5,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=log(igraph_negww8$Smap_cum_sum),
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0negww8,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=rgb(igraph_negww8$ratio,0,0,alpha =igraph_negww8$ratio)
)
#title(main="negative interaction for interspecificity")
par(family="Times")

legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()



pdf("Correlation_diagram_tp0_negative_ww8_cum_sum_edge.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))
#2023/08/07lay.crctp0posww8になっていたのでlay.crctp0negww8に変更
plot(igraphdatanegativetp0cirww8,layout=lay.crctp0negww8,
     vertex.size=log(SPSIZEtp0namenegww8)*5,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=(log(igraph_negww8$Smap_cum_sum)+abs(log(igraph_negww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.width=(log(igraph_negww8$Smap_cum_sum)+abs(log(igraph_negww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=SPCOLtp0negww8,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=rgb(igraph_negww8$ratio,0,0,alpha =igraph_negww8$ratio)
)
#title(main="negative interaction for interspecificity")
par(family="Times")

legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()




#ww8(edgeの色の重みづけ:ratio,矢印の太さ:log(Smap_abs_mean))は3枚とも場所同じ


write.csv(dataframeabssumkeystoneindex,"dataframeabssumkeystoneindex.csv")

keystoneabssumgraph <- graph(t(cbind(dataframeabssumkeystoneindex[,1], dataframeabssumkeystoneindex[,2])))

positionkeystoneabssum<-NULL
for(i in 1:length(V(keystoneabssumgraph)$name)){
  positionkeystoneabssum[i]<-sp_food_coltp0[V(keystoneabssumgraph)$name[i]==sp_food_coltp0$cause,]$re.position
}


positionkeystoneabssumcircle<-layout_in_circle(keystoneabssumgraph,order=order(positionkeystoneabssum))

colkeystoneabssum<-NULL
for(i in 1:length(V(keystoneabssumgraph)$name)){
  colkeystoneabssum[i]<-sp_food_coltp0[V(keystoneabssumgraph)$name[i]==sp_food_coltp0$cause,]$col7
}

for(i in 1:length(V(keystoneabssumgraph)$name)){
  V(keystoneabssumgraph)$name[i]<-sp_food_coltp0[V(keystoneabssumgraph)$name[i]==sp_food_coltp0$cause,]$re.namesp
}

V(keystoneabssumgraph)$name<-gsub(" ","\n",V(keystoneabssumgraph)$name)

keystoneCorrelationcolpositiveSign<-gsub("Positive",1,dataframeabssumkeystoneindex$sign)
keystoneCorrelationcolpositiveSign<-as.numeric(gsub("Negative",0,keystoneCorrelationcolpositiveSign))
keystoneCorrelationcolnegativeSign<-gsub("Negative",1,dataframeabssumkeystoneindex$sign)
keystoneCorrelationcolnegativeSign<-as.numeric(gsub("Positive",0,keystoneCorrelationcolnegativeSign))

#幅に関してはlogをとっているので太さの差が微差になるのはわかるが透明度がイマイチ謎である





#2023/09/19編集

# sp_food_coltp0から引用するように編集予定

correlation_Hori<-read.csv("Hori_aggressive_behaviors.csv")

renameForAggcau<-NULL
renameForAggeff<-NULL
for(i in 1:length(igraph_allww8$cause)){
  renameForAggcau[i]<-sp_food_coltp0[igraph_allww8$cause[i]==sp_food_coltp0$cause,]$re.namesp
  renameForAggeff[i]<-sp_food_coltp0[igraph_allww8$effect[i]==sp_food_coltp0$cause,]$re.namesp
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

library(caret)
processHori <- preProcess(as.data.frame(correlation_Hori$ratio), method=c("range"))
processHori$rangeBounds<-c(0.8,1)
Agg_hori_fig_alpha <- predict(processHori, as.data.frame(correlation_Hori$ratio))



pdf("Hori_correlation_map_aggressive_behavior.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot(Agg_hori_fig,layout=lay.crctp0_Hori,
     vertex.size=13,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=(correlation_Hori$ratio)*2,
     edge.arrow.size=0.1,
     vertex.label.color="black",
     vertex.color=Hori_graph_col,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=c(alpha("red",Agg_hori_fig_alpha$`correlation_Hori$ratio`))
     
)
#title(main="positive interaction for interspecificity")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(sp_food_coltp0$foodhabit7),col=unique(sp_food_coltp0$col7),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()

hori_foodweb_interaction<-read.csv("Hori_Food_web.csv",header=T)

renameForfoodcau<-NULL
renameForfoodeff<-NULL
for(i in 1:length(igraph_allww8$cause)){
  renameForfoodcau[i]<-sp_food_coltp0[igraph_allww8$cause[i]==sp_food_coltp0$cause,]$re.namesp
  renameForfoodeff[i]<-sp_food_coltp0[igraph_allww8$effect[i]==sp_food_coltp0$cause,]$re.namesp
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

#要確認
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






#orderがうまく機能していない？
lay.crctp0_Hori_foodweb<-layout_in_circle(hori_hoodweb_int_graph,order=order(Hori_graph_foodweb_pos))



#8/22矢印の太さの変化がわかりにくい

pdf("Hori_correlation_map_Food_web.pdf")

par(oma=c(0,0,0,0),mar=c(0,3,5,3),mgp=c(0,0,0),mfrow=c(1,1))

plot(Agg_food_fig,layout=lay.crctp0_Hori_foodweb,
     vertex.size=15,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     #全部0.05なので統一
     edge.width=as.numeric(dataframeHorifoodNonNode$ratio)*5,
     edge.arrow.size=0.1,
     vertex.label.color="black",
     vertex.color=Hori_graph_foodweb_col,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     #全部0.05なので統一
     edge.color="red"
     
)
#title(main="positive interaction for interspecificity")
legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(sp_food_coltp0$foodhabit7),col=unique(sp_food_coltp0$col7),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()





pdf("Fig2.pdf")
#Correlation_diagram_tp0_positive_ww8_cum_sumとCorrelation_diagram_tp0_negative_ww8_cum_sumを横に並べる

par(mar=c(0,1,3,0),oma=c(0,0,0,0),mfrow=c(2,2))

plot.igraph2(igraphdatapositivetp0cirww8,layout=lay.crctp0posww8,
     vertex.frame.color="white",
     edge.curved=0.1,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     vertex.size=log(SPSIZEtp0nameposww8)*5,
     edge.width=log(igraph_posww8$Smap_cum_sum),
     edge.arrow.size=(log(igraph_posww8$Smap_cum_sum)-min(log(igraph_posww8$Smap_cum_sum))+1)*0.1,
     vertex.label.color="black",
     vertex.color=SPCOLtp0posww8,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=rgb(0,0,(igraph_posww8$ratio),alpha =igraph_posww8$ratio)
     
)
text(x=-1,y=1,labels="(a)",adj=c(1,1),col="black",cex=2,family="Times")

#legend("top",legend=unique(sp_food_coltp0$foodhabit7)[-5],col=unique(sp_food_coltp0$col7)[-5],pch=16,ncol=3,cex=1,box.lwd =NA)


plot.igraph2(igraphdatanegativetp0cirww8,layout=lay.crctp0negww8,
     vertex.size=log(SPSIZEtp0namenegww8)*5,
     vertex.frame.color="white",
     edge.curved=0.1,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=log(igraph_negww8$Smap_cum_sum),
     edge.arrow.size=(log(igraph_negww8$Smap_cum_sum)-min(log(igraph_negww8$Smap_cum_sum))+1)*0.1,
     vertex.label.color="black",
     vertex.color=SPCOLtp0negww8,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color=rgb(igraph_negww8$ratio,0,0,alpha =igraph_negww8$ratio)
)
text(x=-1,y=1,labels="(b)",adj=c(1,1),col="black",cex=2,family="Times")

#title(main="negative interaction for interspecificity")
#par(family="Times")

#legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=labels,col=cols,pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

plot.igraph2(Agg_food_fig,layout=lay.crctp0_Hori_foodweb,
     vertex.size=18,
     vertex.frame.color="white",
     edge.curved=0.1,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     #全部0.05なので統一
     edge.width=as.numeric(dataframeHorifoodNonNode$ratio)*5+0.1,
     edge.arrow.size=as.numeric(dataframeHorifoodNonNode$ratio)*5+0.1,
     vertex.label.color="black",
     vertex.color=Hori_graph_foodweb_col,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     #全部0.05なので統一
     edge.color="red"
     
)
text(x=-1,y=1,labels="(c)",adj=c(1,1),col="black",cex=2,family="Times")




plot(Agg_hori_fig,layout=lay.crctp0_Hori,
     vertex.size=15,
     vertex.frame.color=rgb(0,0,0,alpha=0),
     edge.curved=0.1,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=(correlation_Hori$ratio)*2,
     edge.arrow.size=(log(correlation_Hori$ratio)-min(log(correlation_Hori$ratio))+1)-0.5,
     vertex.label.color="black",
     vertex.color=Hori_graph_col,
     vertex.label.dist=0,
     #c("orange","green","red","green","red",
     #              "white","yellow","skyblue","yellow","yellow4",
     #             "yellow4","yellow","orange","pink","cyan",
     #            "orange","orange","violet","green","grey",
     #           "cyan","green","yellow","cyan","palegoldenrod",
     #          "pink","darkgreen","darkgreen","cyan","pink",
     #         "blue","red","green","cyan","grey",
     #        "cyan","blue","pink")
     edge.color="red"
     
)
text(x=-1,y=1,labels="(d)",adj=c(1,1),col="black",cex=2,family="Times")

legend(x=par()$usr[1]-0.1,y=par()$usr[4]+0.3,legend=unique(sp_food_coltp0$foodhabit7),col=unique(sp_food_coltp0$col7),pch=16,ncol=3,cex=0.6,box.lwd =NA,xpd=T)

dev.off()



















#中心媒介性

igraphdatatp0ww8_tbl<-as_tbl_graph(graph(t(cbind(igraph_allww8$cause,igraph_allww8$effect))), direted = T)

graph_bet_foodweb_col<-NULL
for(i in 1:length(V(igraphdatatp0ww8_tbl)$name)){
  graph_bet_foodweb_col[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8_tbl)$name[i],]$col7
}
graph_bet_foodweb_col

graph_bet_foodweb_pos<-NULL
for(i in 1:length(V(igraphdatatp0ww8_tbl)$name)){
  graph_bet_foodweb_pos[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8_tbl)$name[i],]$re.position
}
graph_bet_foodweb_pos


graph_bet_foodweb_name<-NULL
for(i in 1:length(V(igraphdatatp0ww8_tbl)$name)){
  graph_bet_foodweb_name[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0ww8_tbl)$name[i],]$re.name
}
graph_bet_foodweb_name

graph_bet_foodweb_name<-gsub(". ",".\n",graph_bet_foodweb_name)

V(igraphdatatp0ww8_tbl)$name<-graph_bet_foodweb_name

lay.crctp0igraphdatatp0ww8_tbl<-layout_in_circle(igraphdatatp0ww8_tbl,order=order(graph_bet_foodweb_pos))

lay.crctp0igraphdatatp0ww8_tbl_w8<-betweenness(graph(t(cbind(igraph_allww8$cause,igraph_allww8$effect))),directed = T)

#指数の計算の結果0のものはinFになってしまうため0.1を入れておく
lay.crctp0igraphdatatp0ww8_tbl_w8_log<-lay.crctp0igraphdatatp0ww8_tbl_w8

lay.crctp0igraphdatatp0ww8_tbl_w8_log[is.infinite(lay.crctp0igraphdatatp0ww8_tbl_w8_log)]<-0.1

popmeantbl<-NULL
popmeantblcol<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0ww8_tbl_w8))){
  popmeantbl[i]<- sp_food_coltp0[names(lay.crctp0igraphdatatp0ww8_tbl_w8)[i]==sp_food_coltp0$cause,]$popmean
  popmeantblcol[i]<- sp_food_coltp0[names(lay.crctp0igraphdatatp0ww8_tbl_w8)[i]==sp_food_coltp0$cause,]$col7
}

renameTbl<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0ww8_tbl_w8))){
  renameTbl[i]<-sp_food_coltp0[names(lay.crctp0igraphdatatp0ww8_tbl_w8)[i]==sp_food_coltp0$cause,]$bindre.namesp
}

foodTbl<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0ww8_tbl_w8))){
  foodTbl[i]<-sp_food_coltp0[names(lay.crctp0igraphdatatp0ww8_tbl_w8)[i]==sp_food_coltp0$cause,]$foodhabit7
}


corDatapoptbl<-cbind(renameTbl,lay.crctp0igraphdatatp0ww8_tbl_w8,popmeantbl,foodTbl,popmeantblcol)

colnames(corDatapoptbl)<-c("name","weight","popmean","foodHabitat","col")

corDatapoptbl<-as.data.frame(corDatapoptbl)

corDatapoptbl$weight<-as.numeric(corDatapoptbl$weight)

corDatapoptbl$popmean<-as.numeric(corDatapoptbl$popmean)

corpoptbl<-cor.test(corDatapoptbl$weight,corDatapoptbl$popmean)

corDatapoptbl$foodHabitat <- factor(corDatapoptbl$foodHabitat, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

pdf("between_popmean_and_betweeness_weight.pdf", width = 10, height = 10)



ggplot(data = corDatapoptbl, aes(x = popmean, y = weight, color = foodHabitat, label = name)) +
  geom_point() +
  geom_text_repel() +
  geom_jitter(size = 2, width = 0.1, height = 0.1) +
  annotate("text", x = max(corDatapoptbl$popmean), y = min(corDatapoptbl$weight),
           label = paste("Correlation:", round(corpoptbl$estimate, 2),
                         "\nP-value:", signif(corpoptbl$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black") +
  scale_color_manual(values = c("grazer" = "lightgreen", "browser" = "darkorange", "omnivore" = "gray", "shrimp-eater" = "pink", "fry-feeder" = "royalblue1", "piscivore" = "red", "scale-eater" = "blueviolet")) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, colour = "black")) +
  ylim(-5, 200) +
  xlim(-5, 200)

dev.off()



pdf("betweeness_for_all_causality.pdf")

par(mar=c(0,0,1,0))

plot(igraphdatatp0ww8_tbl,
     #layout=lay.crctp0igraphdatatp0ww8_tbl,
     vertex.frame.color=rgb(1,1,1,alpha=0),
     layout=lay.crctp0igraphdatatp0ww8_tbl,
     edge.curved=0.1,
     vertex.label.cex=0.7,
     vertex.label.family="Arial",
     vertex.label.font=4,
     edge.width=log(igraph_allww8$Smap_cum_sum),
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=graph_bet_foodweb_col,
     vertex.label.dist=0,
     vertex.size=lay.crctp0igraphdatatp0ww8_tbl_w8_log*0.4,
     edge.color=rgb(
       igraph_allww8$Neg * igraph_allww8$ratio,
       0,
       igraph_allww8$Pos * igraph_allww8$ratio,
       alpha = 0.5
     )
     )
legend("top",legend=unique(sp_food_coltp0$foodhabit7)[-5],col=unique(sp_food_coltp0$col7)[-5],pch=16,ncol=3,cex=1,box.lwd =NA)


dev.off()



pdf("Fig2.pdf")

par(mar=c(0,0,1,0))

plot.igraph2(igraphdatatp0ww8_tbl,
     #layout=lay.crctp0igraphdatatp0ww8_tbl,
     vertex.frame.color="white",layout=lay.crctp0igraphdatatp0ww8_tbl,
     edge.curved=0.2,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=(log(igraph_allww8$Smap_cum_sum)+abs(log(igraph_allww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.width=(log(igraph_allww8$Smap_cum_sum)+abs(log(igraph_allww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=graph_bet_foodweb_col,
     vertex.label.dist=0,
     vertex.size=lay.crctp0igraphdatatp0ww8_tbl_w8_log*0.2,
     edge.color=rgb(
       igraph_allww8$Neg * igraph_allww8$ratio,
       0,
       igraph_allww8$Pos * igraph_allww8$ratio,
       alpha = 0.5
     )
)

legend("top",legend=unique(sp_food_coltp0$foodhabit7)[-5],col=unique(sp_food_coltp0$col7)[-5],pch=16,ncol=4,cex=1,box.lwd =NA)

dev.off()

degree_distribution <- degree_distribution(igraphdatatp0ww8_tbl)
non_zero_data <- degree_distribution[degree_distribution > 0]

# 対数-対数プロットを作成する
plot(non_zero_data, main="Degree Distribution (log-log)", xlab="log(Degree)", ylab="log(Frequency)", log="xy")

# 対数-対数プロット上での直線的な傾向を確認する
# 例えば、直線的な部分を取り出し、その直線の傾きを計算することができます

# 指数の計算（例示）
lm_result <- lm(log(non_zero_data) ~ log(seq_along(non_zero_data)))
exponent <- coef(lm_result)[2]

a = 1+(1/(exponent))

igraphdatatp0negww8_tbl<-as_tbl_graph(graph(t(cbind(igraph_negww8$cause,igraph_negww8$effect))), direted = T)

graph_bet_foodweb_col_neg<-NULL
for(i in 1:length(V(igraphdatatp0negww8_tbl)$name)){
  graph_bet_foodweb_col_neg[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8_tbl)$name[i],]$col7
}
graph_bet_foodweb_col_neg

graph_bet_foodweb_pos_neg<-NULL
for(i in 1:length(V(igraphdatatp0negww8_tbl)$name)){
  graph_bet_foodweb_pos_neg[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8_tbl)$name[i],]$re.position
}
graph_bet_foodweb_pos_neg


graph_bet_foodweb_name_neg<-NULL
for(i in 1:length(V(igraphdatatp0negww8_tbl)$name)){
  graph_bet_foodweb_name_neg[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0negww8_tbl)$name[i],]$re.name
}
graph_bet_foodweb_name_neg

graph_bet_foodweb_name_neg<-gsub(". ",".\n",graph_bet_foodweb_name_neg)

V(igraphdatatp0negww8_tbl)$name<-graph_bet_foodweb_name_neg

lay.crctp0igraphdatatp0negww8_tbl<-layout_in_circle(igraphdatatp0negww8_tbl,order=order(graph_bet_foodweb_pos_neg))


lay.crctp0igraphdatatp0negww8_tbl_w8<-betweenness(graph(t(cbind(igraph_negww8$cause,igraph_negww8$effect))),directed = T)

#指数の計算の結果0のものはinFになってしまうため0.1を入れておく
lay.crctp0igraphdatatp0negww8_tbl_w8_log<-sqrt(lay.crctp0igraphdatatp0negww8_tbl_w8)

lay.crctp0igraphdatatp0negww8_tbl_w8_log[is.infinite(lay.crctp0igraphdatatp0negww8_tbl_w8_log)]<-0.1

popmeantblneg<-NULL
popmeantblcolneg<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0negww8_tbl_w8))){
  popmeantblneg[i]<- sp_food_coltp0[names(lay.crctp0igraphdatatp0negww8_tbl_w8)[i]==sp_food_coltp0$cause,]$popmean
  popmeantblcolneg[i]<- sp_food_coltp0[names(lay.crctp0igraphdatatp0negww8_tbl_w8)[i]==sp_food_coltp0$cause,]$col7
}

renameTblneg<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0negww8_tbl_w8))){
  renameTblneg[i]<-sp_food_coltp0[names(lay.crctp0igraphdatatp0negww8_tbl_w8)[i]==sp_food_coltp0$cause,]$bindre.namesp
}

foodTblneg<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0negww8_tbl_w8))){
  foodTblneg[i]<-sp_food_coltp0[names(lay.crctp0igraphdatatp0negww8_tbl_w8)[i]==sp_food_coltp0$cause,]$foodhabit7
}


corDatapoptblneg<-cbind(renameTblneg,lay.crctp0igraphdatatp0negww8_tbl_w8,popmeantblneg,foodTblneg,popmeantblcolneg)

colnames(corDatapoptblneg)<-c("name","weight","popmean","foodHabitat","col")

corDatapoptblneg<-as.data.frame(corDatapoptblneg)

corDatapoptblneg$weight<-as.numeric(corDatapoptblneg$weight)

corDatapoptblneg$popmean<-as.numeric(corDatapoptblneg$popmean)

corpoptblneg<-cor.test(corDatapoptblneg$weight,corDatapoptblneg$popmean)

corDatapoptblneg$foodHabitat <- factor(corDatapoptblneg$foodHabitat, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

pdf("between_popmean_neg_and_betweeness_weight.pdf", width = 10, height = 10)

ggplot(data=corDatapoptblneg,aes(x=popmean,y=weight,color=foodHabitat,label =name ))+
  geom_point()+
  geom_text_repel()+
  geom_jitter(size = 2, width = 0.1,height = 0.1)+
  annotate("text", x = max(corDatapoptblneg$popmean), y = min(corDatapoptblneg$weight),
           label = paste("Correlation:", round(corpoptblneg$estimate, 2),
                         "\nP-value:", signif(corpoptblneg$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black")+
  scale_color_manual(values = c("grazer" = "lightgreen", "browser" = "darkorange", "omnivore" = "gray", "shrimp-eater" = "pink", "fry-feeder" = "royalblue1", "piscivore" = "red", "scale-eater" = "blueviolet")) +
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))+
  ylim(-5,200)+
  xlim(-5,200)

dev.off()


pdf("betweeness_for_negative_causality.pdf", width = 10, height = 10)

plot(igraphdatatp0negww8_tbl,
     main='betweeness for negative causality',layout=lay.crctp0igraphdatatp0negww8_tbl,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=log(igraph_negww8$Smap_cum_sum),
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=graph_bet_foodweb_col_neg,
     vertex.label.dist=0,
     vertex.size=lay.crctp0igraphdatatp0negww8_tbl_w8_log*3,
     edge.color=rgb(
       igraph_negww8$Neg * igraph_negww8$ratio,
       0,
       0,
       alpha = 0.5
     )
)

dev.off()


pdf("betweeness_for_negative_causality_edge.pdf", width = 10, height = 10)

plot.igraph2(igraphdatatp0negww8_tbl,
     main='betweeness for negative causality',layout=lay.crctp0igraphdatatp0negww8_tbl,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=(log(igraph_negww8$Smap_cum_sum)+abs(log(igraph_negww8$Smap_cum_sum))+1)*0.3,
     edge.width=log(igraph_negww8$Smap_cum_sum),
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=graph_bet_foodweb_col_neg,
     vertex.label.dist=0,
     vertex.size=lay.crctp0igraphdatatp0negww8_tbl_w8_log*3,
     edge.color=rgb(
       igraph_negww8$Neg * igraph_negww8$ratio,
       0,
       0,
       alpha = 0.5
     )
)

dev.off()



igraphdatatp0posww8_tbl<-as_tbl_graph(graph(t(cbind(igraph_posww8$cause,igraph_posww8$effect))), direted = T)

graph_bet_foodweb_col_pos<-NULL
for(i in 1:length(V(igraphdatatp0posww8_tbl)$name)){
  graph_bet_foodweb_col_pos[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8_tbl)$name[i],]$col7
}
graph_bet_foodweb_col_pos

graph_bet_foodweb_pos_pos<-NULL
for(i in 1:length(V(igraphdatatp0posww8_tbl)$name)){
  graph_bet_foodweb_pos_pos[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8_tbl)$name[i],]$re.position
}
graph_bet_foodweb_pos_pos


graph_bet_foodweb_name_pos<-NULL
for(i in 1:length(V(igraphdatatp0posww8_tbl)$name)){
  graph_bet_foodweb_name_pos[i]<-sp_food_coltp0[sp_food_coltp0$cause==V(igraphdatatp0posww8_tbl)$name[i],]$re.name
}
graph_bet_foodweb_name_pos

graph_bet_foodweb_name_pos<-gsub(". ",".\n",graph_bet_foodweb_name_pos)

V(igraphdatatp0posww8_tbl)$name<-graph_bet_foodweb_name_pos

lay.crctp0igraphdatatp0posww8_tbl<-layout_in_circle(igraphdatatp0posww8_tbl,order=order(graph_bet_foodweb_pos_pos))


lay.crctp0igraphdatatp0posww8_tbl_w8<-betweenness(graph(t(cbind(igraph_posww8$cause,igraph_posww8$effect))),directed = T)

#指数の計算の結果0のものはinFになってしまうため0.1を入れておく
lay.crctp0igraphdatatp0posww8_tbl_w8_log<-sqrt(lay.crctp0igraphdatatp0posww8_tbl_w8)

lay.crctp0igraphdatatp0posww8_tbl_w8_log[is.infinite(lay.crctp0igraphdatatp0posww8_tbl_w8_log)]<-0.1

popmeantblpos<-NULL
popmeantblcolpos<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0posww8_tbl_w8))){
  popmeantblpos[i]<- sp_food_coltp0[names(lay.crctp0igraphdatatp0posww8_tbl_w8)[i]==sp_food_coltp0$cause,]$popmean
  popmeantblcolpos[i]<- sp_food_coltp0[names(lay.crctp0igraphdatatp0posww8_tbl_w8)[i]==sp_food_coltp0$cause,]$col7
}

renameTblpos<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0posww8_tbl_w8))){
  renameTblpos[i]<-sp_food_coltp0[names(lay.crctp0igraphdatatp0posww8_tbl_w8)[i]==sp_food_coltp0$cause,]$bindre.namesp
}

foodTblpos<-NULL
for(i in 1:length(names(lay.crctp0igraphdatatp0posww8_tbl_w8))){
  foodTblpos[i]<-sp_food_coltp0[names(lay.crctp0igraphdatatp0posww8_tbl_w8)[i]==sp_food_coltp0$cause,]$foodhabit7
}


corDatapoptblpos<-cbind(renameTblpos,lay.crctp0igraphdatatp0posww8_tbl_w8,popmeantblpos,foodTblpos,popmeantblcolpos)

colnames(corDatapoptblpos)<-c("name","weight","popmean","foodHabitat","col")

corDatapoptblpos<-as.data.frame(corDatapoptblpos)

corDatapoptblpos$weight<-as.numeric(corDatapoptblpos$weight)

corDatapoptblpos$popmean<-as.numeric(corDatapoptblpos$popmean)

corpoptblpos<-cor.test(corDatapoptblpos$weight,corDatapoptblpos$popmean)

corDatapoptblpos$foodHabitat <- factor(corDatapoptblpos$foodHabitat, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))


pdf("between_popmean_pos_and_betweeness_weight.pdf", width = 10, height = 10)

ggplot(data=corDatapoptblpos,aes(x=popmean,y=weight,color=foodHabitat,label =name ))+
  geom_point()+
  geom_text_repel()+
  geom_jitter(size = 2, width = 0.1,height = 0.1)+
  annotate("text", x = max(corDatapoptblpos$popmean), y = min(corDatapoptblpos$weight+15),
           label = paste("Correlation:", round(corpoptblpos$estimate, 2),
                         "\nP-value:", signif(corpoptblpos$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black")+
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))+
  ylim(-5,200)+
  xlim(-5,200)+
  scale_color_manual(values = c("grazer" = "lightgreen", "browser" = "darkorange", "omnivore" = "gray", "shrimp-eater" = "pink", "fry-feeder" = "royalblue1", "piscivore" = "red", "scale-eater" = "blueviolet")) 

dev.off()

pdf("betweeness_for_positive_causality.pdf", width = 10, height = 10)


plot(igraphdatatp0posww8_tbl,
     main='betweeness for positive causality',layout=lay.crctp0igraphdatatp0posww8_tbl,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=log(igraph_posww8$Smap_cum_sum),
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=graph_bet_foodweb_col_pos,
     vertex.label.dist=0,
     vertex.size=lay.crctp0igraphdatatp0posww8_tbl_w8_log*3,
     edge.color=rgb(
       0,
       0,
       igraph_posww8$Pos * igraph_posww8$ratio,
       alpha = 0.5
     )
)

dev.off()


pdf("betweeness_for_positive_causality_edge.pdf", width = 10, height = 10)

plot.igraph2(igraphdatatp0posww8_tbl,
     main='betweeness for positive causality',layout=lay.crctp0igraphdatatp0posww8_tbl,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     edge.width=(log(igraph_posww8$Smap_cum_sum)+abs(log(igraph_posww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.width=(log(igraph_posww8$Smap_cum_sum)+abs(log(igraph_posww8$Smap_cum_sum))+1)*0.3,
     edge.arrow.size=0.2,
     vertex.label.color="black",
     vertex.color=graph_bet_foodweb_col_pos,
     vertex.label.dist=0,
     vertex.size=lay.crctp0igraphdatatp0posww8_tbl_w8_log*3,
     edge.color=rgb(
       0,
       0,
       igraph_posww8$Pos * igraph_posww8$ratio,
       alpha = 0.5
     )
)

dev.off()

#-------3/4histgramについて




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

library(dplyr)

histAllList_count <- dplyr::group_by(histAllList, cause, EffectFoodHabit) %>%
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")

histAllList_count_sum<-NULL
for(i in 1:length(unique(histAllList_count$cause))){
  sum<-sum(histAllList_count[unique(histAllList_count$cause)[i]==histAllList_count$cause,]$Count)
  name<-unique(histAllList_count$cause)[i]
  food<-sp_food_coltp0[name==sp_food_coltp0$re.namesp,]$foodhabit7
  each<-cbind(name,food,sum)
  histAllList_count_sum<-rbind(histAllList_count_sum,each)
}

histAllList_count_sum<-as.data.frame(histAllList_count_sum)

histAllList_count_sum$sum<-as.numeric(histAllList_count_sum$sum)

histAllList_count_sum<-histAllList_count_sum[order(histAllList_count_sum$sum),]


order_food<-unique(sp_food_coltp0[sort(sp_food_coltp0$re.position,decreasing = F,index=T)$ix,]$foodhabit7)

histAllList_count_level<-NULL
for(i in 1:length(unique(histAllList_count_sum$food))){
  histAllList_count_level<-rbind(histAllList_count_level,histAllList_count_sum[unique(histAllList_count_sum$food)[i]==histAllList_count_sum$food,])
}

histAllList_count_food_level<-NULL
for(i in 1:length(order_food)){
  part<-subset(histAllList_count_level,histAllList_count_level$food==order_food[i])
  histAllList_count_food_level<-rbind(histAllList_count_food_level,part)
}

histAllList_count$cause <- factor(histAllList_count$cause, levels = histAllList_count_level$name)

xlabCol<-NULL
for(i in 1:length(histAllList_count_level$name)){
  xlabCol[i]<-sp_food_coltp0[histAllList_count_level$name[i]==sp_food_coltp0$re.namesp,]$col7
}

xlabCol<-gsub("white","black",xlabCol)

histAllList_count_col<-NULL
for(i in 1:nrow(histAllList_count)){
  histAllList_count_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==histAllList_count$EffectFoodHabit[i],]$col7)
}

histAllList_count<-cbind(histAllList_count,histAllList_count_col)

histAllList_count$Count<-as.numeric(histAllList_count$Count)

names(histAllList_count)<-c("cause","EffectFoodHabit","Count","col")

histAllList_count$col<-gsub("white","black",histAllList_count$col)

histAllList_count$EffectFoodHabit <- factor(histAllList_count$EffectFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

ggplot(histAllList_count, aes(x = cause, y = Count, fill = EffectFoodHabit)) +
  geom_bar(stat = "identity") +
  labs(title = "The feeding habits of influencing species by each species",
       x = "Cause species",
       y = "Count of causes for each species ") +
  scale_y_continuous(limits = c(0, 11)) +
  theme(legend.position = "right",axis.text.x = element_text(angle = 45, hjust = 1,color =xlabCol ))+
  scale_fill_manual(values = setNames(histAllList_count$col, histAllList_count$EffectFoodHabit))+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))


ggsave("The_count_of_influencing_species_causality_by_each_species.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)

#--------(原因側の数で受けて色全て)



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
#-------3/4histgramについて(原因側の数で受けて色正のみ)










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

ggplot(histNegList_count, aes(x = cause, y = Count, fill = EffectFoodHabit)) +
  geom_bar(stat = "identity") +
  labs(title = "The feeding habits of influencing species negatively by each species",
       x = "Cause species",
       y = "Count of causes for each species ") +
  scale_y_continuous(limits = c(0, 11)) +
  theme(legend.position = "right",axis.text.x = element_text(angle = 45, hjust = 1,color=xlabColNeg))+
  scale_fill_manual(values = setNames(histNegList_count$col, histNegList_count$EffectFoodHabit))+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))


ggsave("The_count_of_influencing_species_with_negative_causality_by_each_species.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)
#-------3/4histgramについて(原因側の色・数負のみ)





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



histAllList_count_Effect <- dplyr::group_by(histAllListEffect, effect, CauseFoodHabit) %>%
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")


histAllList_count_effect_sum<-NULL
for(i in 1:length(unique(histAllList_count_Effect$effect))){
  sum<-sum(histAllList_count_Effect[unique(histAllList_count_Effect$effect)[i]==histAllList_count_Effect$effect,]$Count)
  name<-unique(histAllList_count_Effect$effect)[i]
  food<-sp_food_coltp0[name==sp_food_coltp0$re.namesp,]$foodhabit7
  each<-cbind(name,food,sum)
  histAllList_count_effect_sum<-rbind(histAllList_count_effect_sum,each)
}

histAllList_count_effect_sum<-as.data.frame(histAllList_count_effect_sum)

histAllList_count_effect_sum$sum<-as.numeric(histAllList_count_effect_sum$sum)

histAllList_count_effect_sum<-histAllList_count_effect_sum[order(histAllList_count_effect_sum$sum),]

histAllList_count_effect_level<-NULL
for(i in 1:length(unique(histAllList_count_effect_sum$food))){
  histAllList_count_effect_level<-rbind(histAllList_count_effect_level,histAllList_count_effect_sum[unique(histAllList_count_effect_sum$food)[i]==histAllList_count_effect_sum$food,])
}

histAllList_count_Effect$effect <- factor(histAllList_count_Effect$effect, levels = histAllList_count_effect_level$name)

histAllList_count_Effect_col<-NULL
for(i in 1:nrow(histAllList_count_Effect)){
  histAllList_count_Effect_col[i]<-unique(sp_food_coltp0[sp_food_coltp0$foodhabit7==histAllList_count_Effect$CauseFoodHabit[i],]$col7)
}

histAllList_count_Effect<-cbind(histAllList_count_Effect,histAllList_count_Effect_col)

histAllList_count_Effect$Count<-as.numeric(histAllList_count_Effect$Count)

names(histAllList_count_Effect)<-c("effect","CauseFoodHabit","Count","col")

histAllList_count_Effect$col<-gsub("white","black",histAllList_count_Effect$col)



xlabColEff<-NULL
for(i in 1:length(histAllList_count_effect_level$name)){
  xlabColEff[i]<-sp_food_coltp0[histAllList_count_effect_level$name[i]==sp_food_coltp0$re.namesp,]$col7
}

xlabColEff<-gsub("white","black",xlabColEff)

histAllList_count_Effect$CauseFoodHabit <- factor(histAllList_count_Effect$CauseFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))


ggplot(histAllList_count_Effect, aes(x = effect, y = Count, fill = CauseFoodHabit)) +
  geom_bar(stat = "identity") +
  labs(title = "The feeding habits of influenced species by each species",
       x = "Effect species",
       y = "Count of effect for each species ") +
  scale_y_continuous(limits = c(0, 15)) +
  theme(legend.position = "right",axis.text.x = element_text(angle = 45, hjust = 1,color = xlabColEff))+
  scale_fill_manual(values = setNames(histAllList_count_Effect$col, histAllList_count_Effect$CauseFoodHabit))+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))


ggsave("The_count_of_influenced_species_causality_by_each_species.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)

#-------3/4histgramについて(結果側の色・全ての因果について)

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



xlabColNegEff<-NULL
for(i in 1:length(histNegList_count_effect_level$name)){
  xlabColNegEff[i]<-sp_food_coltp0[histNegList_count_effect_level$name[i]==sp_food_coltp0$re.namesp,]$col7
}

xlabColNegEff<-gsub("white","black",xlabColNegEff)

histNegList_count_Effect$CauseFoodHabit <- factor(histNegList_count_Effect$CauseFoodHabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))


ggplot(histNegList_count_Effect, aes(x = effect, y = Count, fill = CauseFoodHabit)) +
  geom_bar(stat = "identity") +
  labs(title = "The feeding habits of influenced species negatively by each species",
       x = "Effect species",
       y = "Count of effect for each species ") +
  scale_y_continuous(limits = c(0, 15)) +
  theme(legend.position = "right",axis.text.x = element_text(angle = 45, hjust = 1,color=xlabColNegEff))+
  scale_fill_manual(values = setNames(histNegList_count_Effect$col, histNegList_count_Effect$CauseFoodHabit))+
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))


ggsave("The_count_of_influenced_species_with_negative_causality_by_each_species.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)

#-------3/4histgramについて(結果側の色・負の因果について)


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
  


ggsave("FigS3b.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)

pdf("FigS4.pdf",width=11.69,height=8.27)
grid.arrange(FigS4a,FigS4b,ncol=2)
dev.off()

#-------3/4histgramについて(結果側の色・全ての因果に関して正負を上下に分けたもの)


corCauseData<-full_join(histNegList_count_sum,histPosList_count_sum,by="name")


corCauseData<-data.frame("name"=corCauseData$name,"food"=corCauseData$food.x,"Negative"=as.numeric(corCauseData$sum.x),"Positive"=as.numeric(corCauseData$sum.y))

corCauseData$Negative[is.na(corCauseData$Negative)]<-0

corCauseData$Positive[is.na(corCauseData$Positive)]<-0

for(i in 1:nrow(corCauseData)){
  corCauseData$food[i]<-sp_food_coltp0[corCauseData$name[i]==sp_food_coltp0$re.namesp,]$foodhabit7
}

#Hap. micとCya. sppがcause,effectともになし

corCauseData<-rbind(corCauseData,c("Hap. mic","omnivore",0,0))

corCauseData<-rbind(corCauseData,c("Cya. spp","omnivore",0,0))

corCuaseDataframeCol<-NULL
for(i in 1:nrow(corCauseData)){
  corCuaseDataframeCol[i]<-sp_food_coltp0[corCauseData$name[i]==sp_food_coltp0$re.namesp,]$col7
}

corCauseData<-cbind(corCauseData,corCuaseDataframeCol)

corTestCauseData<-cor.test(as.numeric(corCauseData$Negative),as.numeric(corCauseData$Positive))

corCauseData$Negative<-as.numeric(corCauseData$Negative)

corCauseData$Positive<-as.numeric(corCauseData$Positive)

corCauseData$food <- factor(corCauseData$food, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corCauseData$name<-gsub(" ","",corCauseData$name)

pglscorCauseData <- gls(Negative ~ Positive, correlation=corBrownian(phy = anoleTree),
                        data = corCauseData, method = "ML")
summary(pglscorCauseData)

options(ggrepel.max.overlaps = Inf)
CauseNegativeCausePositive<-ggplot(corCauseData,aes(x=Positive,y=Negative,colour=food,label=name))+
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = name), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "")+
  annotate("text", x=0, y=14, label= "(j)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(corTestCauseData$estimate, 2),
                         "\nP-value:", signif(corTestCauseData$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times")+
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "white", linetype = "solid"),panel.grid.minor = element_blank()) +
  scale_color_manual(values = setNames(corCauseData$corCuaseDataframeCol, corCauseData$food))+
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))

ggsave("The_count_of_influenced_species_with_negative_cause_and_positive_cause.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)






corEffectData<-full_join(histNegList_count_effect_sum,histPosList_count_effect_sum,by="name")


corEffectData<-data.frame("name"=corEffectData$name,"food"=corEffectData$food.x,"Negative"=as.numeric(corEffectData$sum.x),"Positive"=as.numeric(corEffectData$sum.y))

corEffectData$Negative[is.na(corEffectData$Negative)]<-0

corEffectData$Positive[is.na(corEffectData$Positive)]<-0

for(i in 1:nrow(corEffectData)){
  corEffectData$food[i]<-sp_food_coltp0[corEffectData$name[i]==sp_food_coltp0$re.namesp,]$foodhabit7
}

#Hap. micとCya. sppがEffect,effectともになし

corEffectData<-rbind(corEffectData,c("Hap. mic","omnivore",0,0))

corEffectData<-rbind(corEffectData,c("Cya. spp","omnivore",0,0))

corCuaseDataframeCol<-NULL
for(i in 1:nrow(corEffectData)){
  corCuaseDataframeCol[i]<-sp_food_coltp0[corEffectData$name[i]==sp_food_coltp0$re.namesp,]$col7
}

corEffectData<-cbind(corEffectData,corCuaseDataframeCol)

corEffectData$corCuaseDataframeCol<-gsub("white","black",corEffectData$corCuaseDataframeCol)


corEffectData<-rbind(corEffectData,c("Gna. pfe","shrimp-eater",0,0,"pink"))
corEffectData<-rbind(corEffectData,c("Tel. tem","browser",0,0,"darkorange"))

corEffectData$Negative<-as.numeric(corEffectData$Negative)

corEffectData$Positive<-as.numeric(corEffectData$Positive)


corTestEffectData<-cor.test(as.numeric(corEffectData$Negative),as.numeric(corEffectData$Positive))


corEffectData$food <- factor(corEffectData$food, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corEffectData$name<-gsub(" ","",corEffectData$name)

pglscorCauseData <- gls(Negative ~ Positive, correlation=corBrownian(phy = anoleTree),
                        data = corEffectData, method = "ML")
summary(pglscorCauseData)


options(ggrepel.max.overlaps = Inf)
EffectNegativeEffectPositive<-ggplot(corEffectData, aes(x = Positive, y = Negative, colour = food, label = name)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = name), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(k)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(corTestEffectData$estimate, 2),
                         "\nP-value:", signif(corTestEffectData$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times") +
  theme(panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_blank(),
        legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = "black")) +
  
  scale_color_manual(values = setNames(corEffectData$corCuaseDataframeCol, corEffectData$food),guide=FALSE)+
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  labs(color = "Food habitat")



ggsave("The_count_of_influenced_species_with_negative_effect_and_positive_effect.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)


degree_dist_ALL <- igraph::degree(igraphdatatp0ww8,mode = "all")

names(degree_dist_ALL)<-gsub("\n","",names(degree_dist_ALL))

degree_dist_ALL_col<-NULL
for(i in 1:length(names(degree_dist_ALL))){
  degree_dist_ALL_col[i]<-sp_food_coltp0[names(degree_dist_ALL)[i]==sp_food_coltp0$bindre.namesp,]$col7
}


degree_dist_ALL_food<-NULL
for(i in 1:length(names(degree_dist_ALL))){
  degree_dist_ALL_food[i]<-sp_food_coltp0[names(degree_dist_ALL)[i]==sp_food_coltp0$bindre.namesp,]$foodhabit7
}





degree_dist_All <- igraph::degree(igraphdatatp0ww8,mode = "out")

names(degree_dist_All)<-gsub("\n","",names(degree_dist_All))


degree_dist_ALL_col<-gsub("white","black",degree_dist_ALL_col)

degree_dist_ALL_col_data<-cbind(names(degree_dist_ALL),tibble(degree = degree_dist_ALL),degree_dist_ALL_col,degree_dist_ALL_food)

degree_dist_ALL_col_data$degree<-as.numeric(degree_dist_ALL_col_data$degree)
#Hap. micとCya. sppがcause,effectともになし
degree_dist_ALL_col_data<-rbind(degree_dist_ALL_col_data,c("Hap.mic",0,"gray","omnivore"))

degree_dist_ALL_col_data<-rbind(degree_dist_ALL_col_data,c("Cya.spp",0,"gray","omnivore"))

colnames(degree_dist_ALL_col_data)<-c("spname","degree","col","food")

degree_dist_ALL_col_data$degree<-as.numeric(degree_dist_ALL_col_data$degree)

ggplot(data = degree_dist_ALL_col_data, aes(x = degree, fill = food)) +
  geom_bar() +
  scale_fill_manual(values = setNames(degree_dist_ALL_col_data$col, degree_dist_ALL_col_data$food)) +
  ggtitle("degree distribution of all causality") +
  ylim(c(0, 8)) +
  scale_x_continuous(breaks = seq(1, 26, by = 1)) +
  theme(legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, colour = "black"))


ggsave("degree_dist_ALL.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)




degree_dist_All_col<-NULL
for(i in 1:length(names(degree_dist_All))){
  degree_dist_All_col[i]<-sp_food_coltp0[names(degree_dist_All)[i]==sp_food_coltp0$bindre.namesp,]$col7
}

degree_dist_All_food<-NULL
for(i in 1:length(names(degree_dist_All))){
  degree_dist_All_food[i]<-sp_food_coltp0[names(degree_dist_All)[i]==sp_food_coltp0$bindre.namesp,]$foodhabit7
}

degree_dist_All_col<-gsub("white","black",degree_dist_All_col)

degree_dist_All_col_data<-cbind(names(degree_dist_All),tibble(degree = degree_dist_All),degree_dist_All_col,degree_dist_All_food)

#Hap. micとCya. sppがcause,effectともになし
degree_dist_All_col_data<-rbind(degree_dist_All_col_data,c("Hap.mic",0,"gray","omnivore"))

degree_dist_All_col_data<-rbind(degree_dist_All_col_data,c("Cya.spp",0,"gray","omnivore"))

degree_dist_All_col_data$degree<-as.numeric(degree_dist_All_col_data$degree)

names(degree_dist_All_col_data)<-c("name","degree","col","food")

out_all<-ggplot(data = degree_dist_All_col_data, aes(x = degree, fill = food)) +
  geom_histogram(binwidth=1) +
  scale_fill_manual(values = setNames(degree_dist_All_col_data$col, degree_dist_All_col_data$food)) +
  ggtitle("(a) All causality for causatives") +
  ylim(c(0, 8)) +
  theme(legend.position = "none", 
        legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, colour = "black"))+
  labs(y="The count of causative relationship")
  

#過分散なのでpoisonではなくglm.nbを参照
summary(glm(degree~food,data=degree_dist_All_col_data,family="poisson"))
summary(glm.nb(degree~food,data=degree_dist_All_col_data))
summary(glm.nb(degree~pop_mean_out,data=degree_dist_All_col_data))


pop_mean_out<-NULL
for(i in 1:nrow(degree_dist_All_col_data)){
  pop_mean_out[i]<-sp_food_coltp0[degree_dist_All_col_data$name[i]==sp_food_coltp0$bindre.namesp,]$popmean
}

degree_dist_All_col_data<-cbind(degree_dist_All_col_data,pop_mean_out)

summary(glm.nb(degree~pop_mean_out,data=degree_dist_All_col_data))


degree_dist_negative <- igraph::degree(igraphdatanegativetp0cirww8,mode = "out")


names(degree_dist_negative)<-gsub("\n","",names(degree_dist_negative))


degree_dist_negative_col<-NULL
for(i in 1:length(names(degree_dist_negative))){
  degree_dist_negative_col[i]<-sp_food_coltp0[names(degree_dist_negative)[i]==sp_food_coltp0$bindre.namesp,]$col7
}

degree_dist_negative_food<-NULL
for(i in 1:length(names(degree_dist_negative))){
  degree_dist_negative_food[i]<-sp_food_coltp0[names(degree_dist_negative)[i]==sp_food_coltp0$bindre.namesp,]$foodhabit7
}

degree_dist_negative_col<-gsub("white","black",degree_dist_negative_col)

degree_dist_negative_col_data<-cbind(names(degree_dist_negative),tibble(degree = degree_dist_negative),degree_dist_negative_col,degree_dist_negative_food)

#Hap. micとCya. sppがcause,effectともになし
degree_dist_negative_col_data<-rbind(degree_dist_negative_col_data,c("Hap.mic",0,"gray","omnivore"))

degree_dist_negative_col_data<-rbind(degree_dist_negative_col_data,c("Cya.spp",0,"gray","omnivore"))

degree_dist_negative_col_data<-rbind(degree_dist_negative_col_data,c("Lam.lem",0,"red","piscivore"))


degree_dist_negative_col_data$degree<-as.numeric(degree_dist_negative_col_data$degree)

names(degree_dist_negative_col_data)<-c("name","degree","col","food")


out_neg<-ggplot(data = degree_dist_negative_col_data, aes(x = degree,fill=food))+
  geom_histogram(binwidth=1) +
  ggtitle("(c) Negative causality for causatives")+
  scale_fill_manual(values = setNames(degree_dist_negative_col_data$col, degree_dist_negative_col_data$food))+
  ylim(c(0,8))+
  theme(legend.position = "none",legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))+
  scale_x_continuous(breaks = seq(0, max(degree_dist_negative_col_data$degree), by = 2))







degree_dist_positive <- igraph::degree(igraphdatapositivetp0cirww8,mode = "out")

names(degree_dist_positive)<-gsub("\n","",names(degree_dist_positive))


degree_dist_positive_col<-NULL
for(i in 1:length(names(degree_dist_positive))){
  degree_dist_positive_col[i]<-sp_food_coltp0[names(degree_dist_positive)[i]==sp_food_coltp0$bindre.namesp,]$col7
}

degree_dist_positive_food<-NULL
for(i in 1:length(names(degree_dist_positive))){
  degree_dist_positive_food[i]<-sp_food_coltp0[names(degree_dist_positive)[i]==sp_food_coltp0$bindre.namesp,]$foodhabit7
}

degree_dist_positive_col<-gsub("white","black",degree_dist_positive_col)

degree_dist_positive_col_data<-cbind(names(degree_dist_positive),tibble(degree = degree_dist_positive),degree_dist_positive_col,degree_dist_positive_food)


#Hap. micとCya. sppがcause,effectともになし
degree_dist_positive_col_data<-rbind(degree_dist_positive_col_data,c("Hap.mic",0,"gray","omnivore"))

degree_dist_positive_col_data<-rbind(degree_dist_positive_col_data,c("Cya.spp",0,"gray","omnivore"))

degree_dist_positive_col_data<-rbind(degree_dist_positive_col_data,c("Tel.tem",0,"darkorange","browser"))

degree_dist_positive_col_data$degree<-as.numeric(degree_dist_positive_col_data$degree)

names(degree_dist_positive_col_data)<-c("name","degree","col","food")


out_pos<-ggplot(data = degree_dist_positive_col_data, aes(x = degree,fill=food))+
  geom_histogram(binwidth=1) +
  ggtitle("(b) Positive causality for causatives")+
  scale_fill_manual(values = setNames(degree_dist_positive_col_data$col, degree_dist_positive_col_data$food))+
  ylim(c(0,8))+
  theme(legend.position = "none",legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))+
  scale_x_continuous(breaks = seq(0, max(degree_dist_positive_col_data$degree), by = 2))


pdf("Fig3.pdf", width = 10, height = 8)
plot_grid(out_all, plot_grid(out_pos,out_neg, labels=c(1:2),align = "h"),labels=c(1:2),ncol=1)
dev.off()



degree_dist_All_in <- igraph::degree(igraphdatatp0ww8,mode = "in")

names(degree_dist_All_in)<-gsub("\n","",names(degree_dist_All_in))


degree_dist_All_in_col<-NULL
for(i in 1:length(names(degree_dist_All_in))){
  degree_dist_All_in_col[i]<-sp_food_coltp0[names(degree_dist_All_in)[i]==sp_food_coltp0$bindre.namesp,]$col7
}

degree_dist_All_in_food<-NULL
for(i in 1:length(names(degree_dist_All_in))){
  degree_dist_All_in_food[i]<-sp_food_coltp0[names(degree_dist_All_in)[i]==sp_food_coltp0$bindre.namesp,]$foodhabit7
}

degree_dist_All_in_col<-gsub("white","black",degree_dist_All_in_col)

degree_dist_All_in_col_data<-cbind(names(degree_dist_All_in),tibble(degree = degree_dist_All_in),degree_dist_All_in_col,degree_dist_All_in_food)

#Hap. micとCya. sppがcause,effectともになし
degree_dist_All_in_col_data<-rbind(degree_dist_All_in_col_data,c("Hap.mic",0,"gray","omnivore"))

degree_dist_All_in_col_data<-rbind(degree_dist_All_in_col_data,c("Cya.spp",0,"gray","omnivore"))

degree_dist_All_in_col_data$degree<-as.numeric(degree_dist_All_in_col_data$degree)

names(degree_dist_All_in_col_data)<-c("name","degree","col","food")

in_all<-ggplot(data = degree_dist_All_in_col_data, aes(x = degree,fill=food))+
  geom_histogram(binwidth=1) +
  scale_fill_manual(values = setNames(degree_dist_All_in_col_data$col, degree_dist_All_in_col_data$food))+
  ggtitle("(a) All causality for recipients")+
  ylim(c(0,8))+
  theme(legend.position = "none",
        legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))

summary(glm(degree~food,data=degree_dist_All_in_col_data,family="quasipoisson"))
summary(glm.nb(degree~food,data=degree_dist_All_in_col_data))

pop_mean_in<-NULL
for(i in 1:nrow(degree_dist_All_in_col_data)){
  pop_mean_in[i]<-sp_food_coltp0[degree_dist_All_in_col_data$name[i]==sp_food_coltp0$bindre.namesp,]$popmean
}

degree_dist_All_in_col_data<-cbind(degree_dist_All_in_col_data,pop_mean_in)

summary(glm.nb(degree~pop_mean_in,data=degree_dist_All_in_col_data))


degree_dist_negative <- igraph::degree(igraphdatanegativetp0cirww8,mode = "in")


names(degree_dist_negative)<-gsub("\n","",names(degree_dist_negative))


degree_dist_All_col_data_all<-degree_dist_All_col_data

degree_dist_All_col_data_all$degree<-
  degree_dist_All_col_data$degree+degree_dist_All_in_col_data$degree

degree_dist_All_col_data$name==degree_dist_All_in_col_data$name

#summary((glm.nb(degree~food,data=degree_dist_All_col_data_all)))
summary((glm.nb(degree~pop_mean_out,data=degree_dist_All_col_data_all)))


degree_dist_negative_col<-NULL
for(i in 1:length(names(degree_dist_negative))){
  degree_dist_negative_col[i]<-sp_food_coltp0[names(degree_dist_negative)[i]==sp_food_coltp0$bindre.namesp,]$col7
}

degree_dist_negative_food<-NULL
for(i in 1:length(names(degree_dist_negative))){
  degree_dist_negative_food[i]<-sp_food_coltp0[names(degree_dist_negative)[i]==sp_food_coltp0$bindre.namesp,]$foodhabit7
}

degree_dist_negative_col<-gsub("white","black",degree_dist_negative_col)

degree_dist_negative_col_data<-cbind(names(degree_dist_negative),tibble(degree = degree_dist_negative),degree_dist_negative_col,degree_dist_negative_food)

#Hap. micとCya. sppがcause,effectともになし
degree_dist_negative_col_data<-rbind(degree_dist_negative_col_data,c("Hap.mic",0,"gray","omnivore"))

degree_dist_negative_col_data<-rbind(degree_dist_negative_col_data,c("Cya.spp",0,"gray","omnivore"))

degree_dist_negative_col_data<-rbind(degree_dist_negative_col_data,c("Lam.lem",0,"red","piscivore"))


degree_dist_negative_col_data$degree<-as.numeric(degree_dist_negative_col_data$degree)

names(degree_dist_negative_col_data)<-c("name","degree","col","food")


in_neg<-ggplot(data = degree_dist_negative_col_data, aes(x = degree,fill=food))+
  geom_histogram(binwidth=1) +
  ggtitle("(c) Negative causality for recipients")+
  scale_fill_manual(values = setNames(degree_dist_negative_col_data$col, degree_dist_negative_col_data$food))+
  ylim(c(0,8))+
  theme(legend.position = "none",legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))+
  scale_x_continuous(breaks = seq(0, max(degree_dist_negative_col_data$degree), by = 2))






degree_dist_positive <- igraph::degree(igraphdatapositivetp0cirww8,mode = "in")

names(degree_dist_positive)<-gsub("\n","",names(degree_dist_positive))


degree_dist_positive_col<-NULL
for(i in 1:length(names(degree_dist_positive))){
  degree_dist_positive_col[i]<-sp_food_coltp0[names(degree_dist_positive)[i]==sp_food_coltp0$bindre.namesp,]$col7
}

degree_dist_positive_food<-NULL
for(i in 1:length(names(degree_dist_positive))){
  degree_dist_positive_food[i]<-sp_food_coltp0[names(degree_dist_positive)[i]==sp_food_coltp0$bindre.namesp,]$foodhabit7
}

degree_dist_positive_col<-gsub("white","black",degree_dist_positive_col)

degree_dist_positive_col_data<-cbind(names(degree_dist_positive),tibble(degree = degree_dist_positive),degree_dist_positive_col,degree_dist_positive_food)


#Hap. micとCya. sppがcause,effectともになし
degree_dist_positive_col_data<-rbind(degree_dist_positive_col_data,c("Hap.mic",0,"gray","omnivore"))

degree_dist_positive_col_data<-rbind(degree_dist_positive_col_data,c("Cya.spp",0,"gray","omnivore"))

degree_dist_positive_col_data<-rbind(degree_dist_positive_col_data,c("Tel.tem",0,"darkorange","browser"))


degree_dist_positive_col_data$degree<-as.numeric(degree_dist_positive_col_data$degree)

names(degree_dist_positive_col_data)<-c("name","degree","col","food")

in_pos<-ggplot(data = degree_dist_positive_col_data, aes(x = degree,fill=food))+
  geom_histogram(binwidth=1) +
  ggtitle("(b) Positive causality for recipients")+
  scale_fill_manual(values = setNames(degree_dist_positive_col_data$col, degree_dist_positive_col_data$food))+
  ylim(c(0,8))+
  theme(legend.position = "none",legend.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA, colour = "black"))+
  scale_x_continuous(breaks = seq(0, max(degree_dist_negative_col_data$degree), by = 2))


pdf("Fig4.pdf", width = 10, height = 8)
plot_grid(in_all, plot_grid(in_pos,in_neg, labels=c(1:2),align = "h"),labels=c(1:2),ncol=1)
dev.off()









#隣接行列
as_adj_list(igraphdatatp0ww8)
as_adjacency_matrix(igraphdatatp0ww8)












#相関関係の図の作成(原点0から描画 NAを0にして図に書く=>0を加えて偏相関係数をみる？)

cause_counts <- histAllList %>% dplyr::group_by(cause) %>% dplyr::summarise(count = n())
effect_counts <- histAllList %>% dplyr::group_by(effect) %>% dplyr::summarise(count = n())

resultAll<-left_join(cause_counts,effect_counts,by=c("cause" = "effect"))

resultAllCol<-vector()
for(i in 1:nrow(resultAll)){
  resultAllCol[i]<-sp_food_coltp0[resultAll$cause[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

resultAllFood<-vector()
for(i in 1:nrow(resultAll)){
  resultAllFood[i]<-sp_food_coltp0[resultAll$cause[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}


corResultAll<-as.data.frame(cbind(resultAll,resultAllCol,resultAllFood))
#Hap. micとCya. sppがcause,effectともになし
setdiff(checkList,unique(ForIgraph1$effect.x))
setdiff(checkList,unique(ForIgraph1$cause.x))
corResultAll<-rbind(corResultAll,c("Hap. mic",0,0,"gray","omnivore"),c("Cya. spp",0,0,"gray","omnivore"))

corResultAll$count.x<-as.numeric(corResultAll$count.x)
corResultAll$count.y<-as.numeric(corResultAll$count.y)

corResultAll[is.na(corResultAll)]<-0

cor_test_result <- cor.test(corResultAll$count.x, corResultAll$count.y)

legend_data_All <- data.frame(
  resultAllFood = unique(corResultAll$resultAllFood),
  resultAllCol = unique(corResultAll$resultAllCol)
)

corResultAll$resultAllFood <- factor(corResultAll$resultAllFood, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corResultAll$cause<-gsub(" ","",corResultAll$cause)

pglscorResultAll <- gls(count.y ~ count.x, correlation=corBrownian(phy = anoleTree),
                        data = corResultAll, method = "ML")
summary(pglscorResultAll)

options(ggrepel.max.overlaps = Inf)
CauseAllEffectAll<-ggplot(corResultAll, aes(x = count.x, y = count.y, color =resultAllFood , label = cause)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = cause), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(i)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_result$estimate, 2),
                         "\nP-value:", signif(cor_test_result$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times") +
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))+
  scale_color_manual(values = setNames(corResultAll$resultAllCol, corResultAll$resultAllFood),guide=FALSE) +
  coord_fixed(ratio = 1)

ggsave("All_causal_relationships_by_food_habit_include00.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)


cause_Pos_counts <- histPosList %>% dplyr::group_by(cause) %>% dplyr::summarise(count = n())
effect_Pos_counts <- histPosList %>% dplyr::group_by(effect) %>% dplyr::summarise(count = n())

resultPos<-left_join(cause_Pos_counts,effect_Pos_counts,by=c("cause" = "effect"))

resultPosCol<-vector()
for(i in 1:nrow(resultPos)){
  resultPosCol[i]<-sp_food_coltp0[resultPos$cause[i]==sp_food_coltp0$re.namesp,][1,]$col7
}


resultPosFood<-vector()
for(i in 1:nrow(resultPos)){
  resultPosFood[i]<-sp_food_coltp0[resultPos$cause[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}

corResultPos<-as.data.frame(cbind(resultPos,resultPosCol,resultPosFood))

corResultPos<-rbind(corResultPos,c("Hap. mic",0,0,"gray","omnivore"),c("Cya. spp",0,0,"gray","omnivore"))

corResultPos$count.x<-as.numeric(corResultPos$count.x)
corResultPos$count.y<-as.numeric(corResultPos$count.y)

corResultPos[is.na(corResultPos)]<-0

legend_data_Pos <- data.frame(
  resultPosFood = unique(corResultPos$resultPosFood),
  resultPosCol = unique(corResultPos$resultPosCol)
)


corResultPos<-rbind(corResultPos,c("Lep. elo",0,0,"red","piscivore"))
corResultPos<-rbind(corResultPos,c("Lob. lab",0,0,"pink","shrimp-eater"))
corResultPos<-rbind(corResultPos,c("Tel. tem",0,0,"darkorange","browser"))

corResultPos$count.x<-as.numeric(corResultPos$count.x)
corResultPos$count.y<-as.numeric(corResultPos$count.y)

cor_test_resultPos <- cor.test(corResultPos$count.x, corResultPos$count.y)

corResultPos$resultPosFood <- factor(corResultPos$resultPosFood, levels = unique(corResultPos$resultPosFood)[-8])

corResultPos$cause<-gsub(" ","",corResultPos$cause)

pglscorResultAll <- gls(count.y ~ count.x, correlation=corBrownian(phy = anoleTree),
                        data = corResultPos, method = "ML")
summary(pglscorResultAll)

options(ggrepel.max.overlaps=Inf)
PositiveCausePositiveEffect<-ggplot(corResultPos, aes(x = count.x, y = count.y, colour = resultPosFood, label = cause)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = cause), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(a)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_resultPos$estimate, 2),
                         "\nP-value:", signif(cor_test_resultPos$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times") +
  scale_color_manual(values = setNames(corResultPos$resultPosCol, corResultPos$resultPosFood),guide=FALSE)+
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = "black"))







ggsave("Positive_causal_relationships_by_food_habit_include00.pdf",width =10, height = 10, units = "in", device = cairo_pdf)


corEffectAllCausePos<-left_join(effect_counts,cause_Pos_counts,by=c("effect"="cause"))
colnames(corEffectAllCausePos)<-c("spname","effect","cause")
corEffectAllCausePos$cause[is.na(corEffectAllCausePos$cause)]<-0

resultEffectAllCausePosFood<-vector()
for(i in 1:nrow(corEffectAllCausePos)){
  resultEffectAllCausePosFood[i]<-sp_food_coltp0[corEffectAllCausePos$spname[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}

resultEffectAllCausePosCol<-vector()
for(i in 1:nrow(corEffectAllCausePos)){
  resultEffectAllCausePosCol[i]<-sp_food_coltp0[corEffectAllCausePos$spname[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

corEffectAllCausePos<-cbind(corEffectAllCausePos,resultEffectAllCausePosFood,resultEffectAllCausePosCol)
colnames(corEffectAllCausePos)<-c("spname","effect","cause","foodhabit","col")

corEffectAllCausePos<-rbind(corEffectAllCausePos,c("Gna. pfe",0,0,"shrimp-eater","pink"))
corEffectAllCausePos<-rbind(corEffectAllCausePos,c("Tel. tem",0,0,"browser","darkorange"))
corEffectAllCausePos<-rbind(corEffectAllCausePos,c("Hap. mic",0,0,"omnivore","gray"))
corEffectAllCausePos<-rbind(corEffectAllCausePos,c("Cya. spp",0,0,"omnivore","gray"))

corEffectAllCausePos$effect<-as.numeric(corEffectAllCausePos$effect)
corEffectAllCausePos$cause<-as.numeric(corEffectAllCausePos$cause)

cor_test_resultCausePosEffetAll<-cor.test(corEffectAllCausePos$cause,corEffectAllCausePos$effect)

corEffectAllCausePos$foodhabit <- factor(corEffectAllCausePos$food, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corEffectAllCausePos$spname<-gsub(" ","",corEffectAllCausePos$spname)

pglscorEffectAllCausePos <- gls(effect ~ cause, correlation=corBrownian(phy = anoleTree),
                                 data = corEffectAllCausePos, method = "ML")
summary(pglscorEffectAllCausePos)


options(ggrepel.max.overlaps = Inf)
PositiveCauseAllEffect<-ggplot(corEffectAllCausePos, aes(x = cause, y = effect, colour = foodhabit, label = spname)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = spname), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(g)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_resultCausePosEffetAll$estimate, 2),
                         "\nP-value:", signif(cor_test_resultCausePosEffetAll$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times" ) +
  scale_color_manual(values = setNames(corEffectAllCausePos$col, corEffectAllCausePos$foodhabit),guide=FALSE) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = "black"))





cause_Neg_counts <- histNegList %>% dplyr::group_by(cause) %>% dplyr::summarise(count = n())
effect_Neg_counts <- histNegList %>% dplyr::group_by(effect) %>% dplyr::summarise(count = n())

resultNeg<-left_join(cause_Neg_counts,effect_Neg_counts,by=c("cause" = "effect"))

resultNegCol<-vector()
for(i in 1:nrow(resultNeg)){
  resultNegCol[i]<-sp_food_coltp0[resultNeg$cause[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

resultNegFood<-vector()
for(i in 1:nrow(resultNeg)){
  resultNegFood[i]<-sp_food_coltp0[resultNeg$cause[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}

corResultNeg<-as.data.frame(cbind(resultNeg,resultNegCol,resultNegFood))

corResultNeg<-rbind(corResultNeg,c("Hap. mic",0,0,"gray","omnivore"),c("Cya. spp",0,0,"gray","omnivore"))

corResultNeg$count.x<-as.numeric(corResultNeg$count.x)
corResultNeg$count.y<-as.numeric(corResultNeg$count.y)

corResultNeg[is.na(corResultNeg)]<-0

legend_data_Neg <- data.frame(
  resultNegFood = unique(corResultNeg$resultNegFood),
  resultNegCol = unique(corResultNeg$resultNegCol)
)

corResultNeg<-rbind(corResultNeg,c("Lam. lem",0,0,"red","piscivore"))
corResultNeg<-rbind(corResultNeg,c("Par. spp",0,0,"gray","omnivore"))

corResultNeg$count.x<-as.numeric(corResultNeg$count.x)
corResultNeg$count.y<-as.numeric(corResultNeg$count.y)

cor_test_resultNeg <- cor.test(corResultNeg$count.x, corResultNeg$count.y)



corResultNeg$resultNegFood <- factor(corResultNeg$resultNegFood, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corResultNeg$cause<-gsub(" ","",corResultNeg$cause)

pglscorCausePosEffectNegs <- gls(count.y ~ count.x, correlation=corBrownian(phy = anoleTree),
                                 data = corResultNeg, method = "ML")
summary(pglscorCausePosEffectNegs)

options(ggrepel.max.overlaps = Inf)
NegativeCauseNegativeEffect<-ggplot(corResultNeg, aes(x = count.x, y = count.y, colour = resultNegFood, label = cause)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = cause), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(e)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_resultNeg$estimate, 2),
                         "\nP-value:", signif(cor_test_resultNeg$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times")+
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = setNames(corResultNeg$resultNegCol, corResultNeg$resultNegFood),guide=FALSE) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  labs(color = "Food Type")+
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))



ggsave("Negative_causal_relationships_by_food_habit_include00.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)



corResultCauseNegEffectPos<-left_join(cause_Neg_counts,effect_Pos_counts,by=c("cause"="effect"))

corResultCauseNegEffectPos<-rbind(corResultCauseNegEffectPos,c("Hap. mic",0,0),c("Cya. spp",0,0))

corResultCauseNegEffectPos$count.x<-as.numeric(corResultCauseNegEffectPos$count.x)
corResultCauseNegEffectPos$count.y<-as.numeric(corResultCauseNegEffectPos$count.y)

corResultCauseNegEffectPos[is.na(corResultCauseNegEffectPos)]<-0

resultCauseNegEffectPosCol<-vector()
for(i in 1:nrow(corResultCauseNegEffectPos)){
  resultCauseNegEffectPosCol[i]<-sp_food_coltp0[corResultCauseNegEffectPos$cause[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

resultCauseNegEffectPosFood<-vector()
for(i in 1:nrow(corResultCauseNegEffectPos)){
  resultCauseNegEffectPosFood[i]<-sp_food_coltp0[corResultCauseNegEffectPos$cause[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}

corResultCauseNegEffectPos<-as.data.frame(cbind(corResultCauseNegEffectPos,resultCauseNegEffectPosCol,resultCauseNegEffectPosFood))

corResultCauseNegEffectPos[is.na(corResultCauseNegEffectPos)]<-0

legend_data_NegPos <- data.frame(
  resultNegPosFood = unique(corResultCauseNegEffectPos$resultCauseNegEffectPosFood),
  resultNegPosCol = unique(corResultCauseNegEffectPos$resultCauseNegEffectPosCol)
)



corResultCauseNegEffectPos<-rbind(corResultCauseNegEffectPos,c("Lam. lem",0,0,"red","piscivore"))
corResultCauseNegEffectPos<-rbind(corResultCauseNegEffectPos,c("Par. spp",0,0,"gray","omnivore"))

names(corResultCauseNegEffectPos)<-c("cause","count.x","count.y","col","food")

corResultCauseNegEffectPos$count.x<-as.numeric(corResultCauseNegEffectPos$count.x)
corResultCauseNegEffectPos$count.y<-as.numeric(corResultCauseNegEffectPos$count.y)

cor_test_result_CauseNegEffectPos<-cor.test(corResultCauseNegEffectPos$count.x, corResultCauseNegEffectPos$count.y)


corResultCauseNegEffectPos$food <- factor(corResultCauseNegEffectPos$food, levels = unique(corResultCauseNegEffectPos$food))

corResultCauseNegEffectPos$cause<-gsub(" ","",corResultCauseNegEffectPos$cause)

pglscorResultNegPos <- gls(count.y ~ count.x, correlation=corBrownian(phy = anoleTree),
                        data = corResultCauseNegEffectPos, method = "ML")

summary(pglscorResultNegPos)

options(ggrepel.max.overlaps = Inf)
NegativeCausePositiveEffect<-ggplot(corResultCauseNegEffectPos, aes(x = count.x, y = count.y, colour = food, label = cause)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = cause), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x ="",
       y = "") +
  annotate("text", x=0, y=14, label= "(b)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_result_CauseNegEffectPos$estimate, 2),
                         "\nP-value:", signif(cor_test_result_CauseNegEffectPos$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times")+
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = setNames(corResultCauseNegEffectPos$col, corResultCauseNegEffectPos$food),guide=FALSE) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  labs(color = "Food Type")+
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))



ggsave("Causal_relationships_between_cause_negatively_relation_and_effect_positively_relation_by_food_habit_include00.pdf",width =10, height = 10, units = "in", device = cairo_pdf)



corEffectAllCauseNeg<-left_join(effect_counts,cause_Neg_counts,by=c("effect"="cause"))
colnames(corEffectAllCauseNeg)<-c("spname","effect","cause")
corEffectAllCauseNeg$cause[is.na(corEffectAllCauseNeg$cause)]<-0

resultEffectAllCausNegFood<-vector()
for(i in 1:nrow(corEffectAllCauseNeg)){
  resultEffectAllCausNegFood[i]<-sp_food_coltp0[corEffectAllCauseNeg$spname[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}

resultEffectAllCauseNegCol<-vector()
for(i in 1:nrow(corEffectAllCauseNeg)){
  resultEffectAllCauseNegCol[i]<-sp_food_coltp0[corEffectAllCauseNeg$spname[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

corEffectAllCauseNeg<-cbind(corEffectAllCauseNeg,resultEffectAllCausNegFood,resultEffectAllCauseNegCol)
colnames(corEffectAllCauseNeg)<-c("spname","effect","cause","foodhabit","col")

corEffectAllCauseNeg<-rbind(corEffectAllCauseNeg,c("Gna. pfe",0,0,"shrimp-eater","pink"))
corEffectAllCauseNeg<-rbind(corEffectAllCauseNeg,c("Tel. tem",0,0,"browser","darkorange"))
corEffectAllCauseNeg<-rbind(corEffectAllCauseNeg,c("Hap. mic",0,0,"omnivore","gray"))
corEffectAllCauseNeg<-rbind(corEffectAllCauseNeg,c("Cya. spp",0,0,"omnivore","gray"))

corEffectAllCauseNeg$cause<-as.numeric(corEffectAllCauseNeg$cause)
corEffectAllCauseNeg$effect<-as.numeric(corEffectAllCauseNeg$effect)

cor_test_resultCauseNegEffetAll<-cor.test(corEffectAllCauseNeg$cause,corEffectAllCauseNeg$effect)


corEffectAllCauseNeg$foodhabit <- factor(corEffectAllCauseNeg$foodhabit, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corEffectAllCauseNeg$spname<-gsub(" ","",corEffectAllCauseNeg$spname)

pglscorEffectAllCauseNeg <- gls(effect ~ cause, correlation=corBrownian(phy = anoleTree),
                                data = corEffectAllCauseNeg, method = "ML")
summary(pglscorEffectAllCauseNeg)

options(ggrepel.max.overlaps = Inf)
NegativeCauseAllEffect<-ggplot(corEffectAllCauseNeg, aes(x = cause, y = effect, colour = foodhabit, label = spname)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = spname), box.padding = 0.5, point.padding = 1, segment.color = "black", family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(h)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_resultCauseNegEffetAll$estimate, 2),
                         "\nP-value:", signif(cor_test_resultCauseNegEffetAll$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black", family="Times" ) +
  scale_color_manual(values = setNames(corEffectAllCauseNeg$col, corEffectAllCauseNeg$foodhabit),guide=FALSE) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = "black"))


corResultCausePosEffectNeg<-left_join(cause_Pos_counts,effect_Neg_counts,by=c("cause"="effect"))

corResultCausePosEffectNeg<-rbind(corResultCausePosEffectNeg,c("Hap. mic",0,0),c("Cya. spp",0,0))

corResultCausePosEffectNeg$count.x<-as.numeric(corResultCausePosEffectNeg$count.x)
corResultCausePosEffectNeg$count.y<-as.numeric(corResultCausePosEffectNeg$count.y)

corResultCausePosEffectNeg[is.na(corResultCausePosEffectNeg)]<-0

resultCausePosEffectNegCol<-vector()
for(i in 1:nrow(corResultCausePosEffectNeg)){
  resultCausePosEffectNegCol[i]<-sp_food_coltp0[corResultCausePosEffectNeg$cause[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

resultCausePosEffectNegFood<-vector()
for(i in 1:nrow(corResultCausePosEffectNeg)){
  resultCausePosEffectNegFood[i]<-sp_food_coltp0[corResultCausePosEffectNeg$cause[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}

legend_data_NegPos <- data.frame(
  resultPosNegFood = unique(corResultCausePosEffectNeg$resultCausePosEffectNegFood),
  resultPosNegCol = unique(corResultCausePosEffectNeg$resultCausePosEffectNegCol)
)
corResultCausePosEffectNeg<-as.data.frame(cbind(corResultCausePosEffectNeg,resultCausePosEffectNegCol,resultCausePosEffectNegFood))


corResultCausePosEffectNeg<-rbind(corResultCausePosEffectNeg,c("Lep. elo",0,0,"red","piscivore"))
corResultCausePosEffectNeg<-rbind(corResultCausePosEffectNeg,c("Lob. lab",0,0,"pink","shrimp-eater"))
corResultCausePosEffectNeg<-rbind(corResultCausePosEffectNeg,c("Tel. tem",0,0,"darkorange","browser"))

corResultCausePosEffectNeg$count.x<-as.numeric(corResultCausePosEffectNeg$count.x)
corResultCausePosEffectNeg$count.y<-as.numeric(corResultCausePosEffectNeg$count.y)


cor_test_result_CausePosEffectNeg<-cor.test(corResultCausePosEffectNeg$count.x, corResultCausePosEffectNeg$count.y)

corResultCausePosEffectNeg$resultCausePosEffectNegFood <- factor(corResultCausePosEffectNeg$resultCausePosEffectNegFood, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corResultCausePosEffectNeg$cause<-gsub(" ","",corResultCausePosEffectNeg$cause)

pglscorCausePosEffectNegs <- gls(count.y ~ count.x, correlation=corBrownian(phy = anoleTree),
                                data = corResultCausePosEffectNeg, method = "ML")
summary(pglscorCausePosEffectNegs)

options(ggrepel.max.overlaps = Inf)
PositiveCauseNegativeEffect<-ggplot(corResultCausePosEffectNeg, aes(x = count.x, y = count.y, colour = resultCausePosEffectNegFood, label = cause)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = cause), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(d)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_result_CausePosEffectNeg$estimate, 2),
                         "\nP-value:", signif(cor_test_result_CausePosEffectNeg$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times")+
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = setNames(corResultCausePosEffectNeg$resultCausePosEffectNegCol, corResultCausePosEffectNeg$resultCausePosEffectNegFood),guide=FALSE) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  labs(color = "Food Type")+
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))



ggsave("Causal_relationships_between_cause_positively_relation_and_effect_negatively_relation_by_food_habit_include00.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)


cause_All_counts <- histAllList %>% dplyr::group_by(cause) %>% dplyr::summarise(count = n())

corResultCauseAllEffectNeg<-left_join(cause_All_counts,effect_Neg_counts,by=c("cause"="effect"))

corResultCauseAllEffectNeg<-rbind(corResultCauseAllEffectNeg,c("Hap. mic",0,0),c("Cya. spp",0,0))

corResultCauseAllEffectNeg$count.x<-as.numeric(corResultCauseAllEffectNeg$count.x)
corResultCauseAllEffectNeg$count.y<-as.numeric(corResultCauseAllEffectNeg$count.y)

corResultCauseAllEffectNeg[is.na(corResultCauseAllEffectNeg)]<-0

corResultCauseAllEffectNeg$count.x<-as.numeric(corResultCauseAllEffectNeg$count.x)
corResultCauseAllEffectNeg$count.y<-as.numeric(corResultCauseAllEffectNeg$count.y)


cor_test_result_CauseAllEffectNeg<-cor.test(corResultCauseAllEffectNeg$count.x, corResultCauseAllEffectNeg$count.y)


resultCauseAllEffectNegCol<-vector()
for(i in 1:nrow(corResultCauseAllEffectNeg)){
  resultCauseAllEffectNegCol[i]<-sp_food_coltp0[corResultCauseAllEffectNeg$cause[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

resultCauseAllEffectNegFood<-vector()
for(i in 1:nrow(corResultCauseAllEffectNeg)){
  resultCauseAllEffectNegFood[i]<-sp_food_coltp0[corResultCauseAllEffectNeg$cause[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}

corResultCauseAllEffectNeg<-as.data.frame(cbind(corResultCauseAllEffectNeg,resultCauseAllEffectNegCol,resultCauseAllEffectNegFood))

legend_data_AllNeg <- data.frame(
  resultAllNegFood = unique(corResultCauseAllEffectNeg$resultCauseAllEffectNegFood),
  resultAllNegCol = unique(corResultCauseAllEffectNeg$resultCauseAllEffectNegCol)
)

corResultCauseAllEffectNeg$resultCauseAllEffectNegCol<-gsub("white","black",corResultCauseAllEffectNeg$resultCauseAllEffectNegCol)

resultCauseAllEffectNegCol<-gsub("white","black",resultCauseAllEffectNegCol)

corResultCauseAllEffectNeg$resultCauseAllEffectNegFood <- factor(corResultCauseAllEffectNeg$resultCauseAllEffectNegFood, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corResultCauseAllEffectNeg$cause<-gsub(" ","",corResultCauseAllEffectNeg$cause)

pglscorCausePosEffectNegs <- gls(count.y ~ count.x, correlation=corBrownian(phy = anoleTree),
                                 data = corResultCauseAllEffectNeg, method = "ML")
summary(pglscorCausePosEffectNegs)



options(ggrepel.max.overlaps = Inf)
CauseAllNegativeEffect<-ggplot(corResultCauseAllEffectNeg, aes(x = count.x, y = count.y, colour = resultCauseAllEffectNegFood, label = cause)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = cause), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(f)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_result_CauseAllEffectNeg$estimate, 2),
                         "\nP-value:", signif(cor_test_result_CauseAllEffectNeg$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black",family="Times")+
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = setNames(corResultCauseAllEffectNeg$resultCauseAllEffectNegCol, corResultCauseAllEffectNeg$resultCauseAllEffectNegFood),guide=FALSE) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  labs(color = "Food Type")+
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))

ggsave("Causal_relationships_between_all_cause_relation_and_effect_negatively_relation_by_food_habit_include00.pdf",width = 10, height = 10, units = "in", device = cairo_pdf)


corResultCauseAllEffectPos<-left_join(cause_All_counts,effect_Pos_counts,by=c("cause"="effect"))

corResultCauseAllEffectPos<-rbind(corResultCauseAllEffectPos,c("Hap. mic",0,0),c("Cya. spp",0,0))

corResultCauseAllEffectPos$count.x<-as.numeric(corResultCauseAllEffectPos$count.x)

corResultCauseAllEffectPos$count.y<-as.numeric(corResultCauseAllEffectPos$count.y)

corResultCauseAllEffectPos[is.na(corResultCauseAllEffectPos)]<-0

resultCauseAllEffectPosCol<-vector()
for(i in 1:nrow(corResultCauseAllEffectPos)){
  resultCauseAllEffectPosCol[i]<-sp_food_coltp0[corResultCauseAllEffectPos$cause[i]==sp_food_coltp0$re.namesp,][1,]$col7
}

resultCauseAllEffectPosFood<-vector()
for(i in 1:nrow(corResultCauseAllEffectPos)){
  resultCauseAllEffectPosFood[i]<-sp_food_coltp0[corResultCauseAllEffectPos$cause[i]==sp_food_coltp0$re.namesp,][1,]$foodhabit7
}



corResultCauseAllEffectPos<-as.data.frame(cbind(corResultCauseAllEffectPos,resultCauseAllEffectPosCol,resultCauseAllEffectPosFood))

cor_test_result_CauseAllEffectPos<-cor.test(corResultCauseAllEffectPos$count.x, corResultCauseAllEffectPos$count.y)

corResultCauseAllEffectPos$resultCauseAllEffectPosFood <- factor(corResultCauseAllEffectPos$resultCauseAllEffectPosFood, levels = c("grazer", "browser", "omnivore", "shrimp-eater", "fry-feeder", "piscivore", "scale-eater"))

corResultCauseAllEffectPos$cause<-gsub(" ","",corResultCauseAllEffectPos$cause)

pglscorCauseAllEffectPos <- gls(count.y ~ count.x, correlation=corBrownian(phy = anoleTree),
                        data = corResultCauseAllEffectPos, method = "ML")

summary(pglscorCauseAllEffectPos)

options(ggrepel.max.overlaps = Inf)
CauseAllEffectPositive<-ggplot(corResultCauseAllEffectPos, aes(x = count.x, y = count.y, colour = resultCauseAllEffectPosFood, label = cause)) +
  geom_jitter(size = 2, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_text_repel(aes(label = cause), box.padding = 0.5, point.padding = 1, segment.color = "black",family="Times") +
  labs(title = "",
       x = "",
       y = "") +
  annotate("text", x=0, y=14, label= "(c)",size=8, family="Times" )+
  annotate("text", x = 13, y = 0,
           label = paste("Correlation:", round(cor_test_result_CauseAllEffectPos$estimate, 2),
                         "\nP-value:", signif(cor_test_result_CauseAllEffectPos$p.value, digits = 2)),
           hjust = 1, vjust = 0, size = 4, color = "black", family="Times")+
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = setNames(corResultCauseAllEffectPos$resultCauseAllEffectPosCol, corResultCauseAllEffectPos$resultCauseAllEffectPosFood),guide=FALSE) +
  scale_x_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  scale_y_continuous(breaks = seq(-1, 15, 1.0), limits = c(-1, 15)) + 
  coord_fixed(ratio = 1) +
  labs(color = "Food Type")+
  theme(legend.position=c(1, 1), legend.justification=c(1, 1),
        legend.background=element_rect(fill=NA, color=NA),
        plot.background=element_rect(fill=NA, color=NA),
        panel.background=element_rect(fill=NA, color="black"))




ggsave("Causal_relationships_between_all_cause_relation_and_effect_positively_relation_by_food_habit_include00.pdf",width = 8.27, height = 11.69, units = "in", device = cairo_pdf)


Fig5_a<-grid.arrange(PositiveCausePositiveEffect,
NegativeCausePositiveEffect,
CauseAllEffectPositive,
PositiveCauseNegativeEffect,
NegativeCauseNegativeEffect,
CauseAllNegativeEffect,
PositiveCauseAllEffect,
NegativeCauseAllEffect,
CauseAllEffectAll,ncol=3)

ggsave("Fig5_a.pdf",Fig5_a,width = 8.27, height = 11.69, units = "in", dpi = 300)

ggsave("1.pdf",PositiveCausePositiveEffect)
ggsave("2.pdf",NegativeCausePositiveEffect)
ggsave("3.pdf",CauseAllEffectPositive)

ggsave("4.pdf",PositiveCauseNegativeEffect)
ggsave("5.pdf",NegativeCauseNegativeEffect)
ggsave("6.pdf",CauseAllNegativeEffect)

ggsave("7.pdf",PositiveCauseAllEffect)
ggsave("8.pdf",NegativeCauseAllEffect)
ggsave("9.pdf",CauseAllEffectAll)


Fig5_b<-grid.arrange(CauseNegativeCausePositive,EffectNegativeEffectPositive,ncol=2)

ggsave("Fig5_b.pdf",Fig5_b,width = 8.27, height = 11.69, units = "in", dpi = 300)



ggsave("10.pdf",CauseNegativeCausePositive)
ggsave("11.pdf",EffectNegativeEffectPositive)















#全ての因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_All_all_digdis <- degree(igraphdatatp0ww8,mode = "all")

# 次数の頻度分布を計算
degree_freq <- table(degree_dist_All_all_digdis)
N <- sum(degree_freq)
# degree_dist_in_digdis (度数) は table の名前部分
degree_dist_in_digdis <- as.numeric(names(degree_freq))
k <- as.numeric(names(degree_freq))

lambda <- sum(degree_dist_in_digdis * k) / N
expected_counts <- N * dpois(degree_dist_in_digdis, lambda)
chi_squared <- sum((degree_freq - expected_counts)^2 / expected_counts)
df <- length(k) - 1
p_value_chi <- pchisq(chi_squared, df, lower.tail = FALSE)

# 結果の表示
cat("Chi-squared:", chi_squared, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", p_value_chi, "\n")

# 有意水準の設定
alpha <- 0.05
if (p_value_chi < alpha) {
  cat("帰無仮説は棄却されました。ポアソン型ネットワークではない可能性があります。\n")
} else {
  cat("帰無仮説は棄却されませんでした。ポアソン型ネットワークの可能性があります。\n")
}

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




#原因側の因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_out_digdis <- degree(igraphdatatp0ww8,mode = "out")

# 次数の頻度分布を計算
degree_freq <- table(degree_dist_out_digdis)
N <- sum(degree_freq)
# degree_dist_in_digdis (度数) は table の名前部分
degree_dist_in_digdis <- as.numeric(names(degree_freq))
k <- as.numeric(names(degree_freq))

lambda <- sum(degree_dist_in_digdis * k) / N
expected_counts <- N * dpois(degree_dist_in_digdis, lambda)
chi_squared <- sum((degree_freq - expected_counts)^2 / expected_counts)
df <- length(k) - 1
p_value_chi <- pchisq(chi_squared, df, lower.tail = FALSE)

# 結果の表示
cat("Chi-squared:", chi_squared, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", p_value_chi, "\n")

# 有意水準の設定
alpha <- 0.05
if (p_value_chi < alpha) {
  cat("帰無仮説は棄却されました。ポアソン型ネットワークではない可能性があります。\n")
} else {
  cat("帰無仮説は棄却されませんでした。ポアソン型ネットワークの可能性があります。\n")
}

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




#結果側の因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_in_digdis <- degree(igraphdatatp0ww8,mode = "in")

# 次数の頻度分布を計算
degree_freq <- table(degree_dist_in_digdis)
N <- sum(degree_freq)
# degree_dist_in_digdis (度数) は table の名前部分
degree_dist_in_digdis <- as.numeric(names(degree_freq))
k <- as.numeric(names(degree_freq))

lambda <- sum(degree_dist_in_digdis * k) / N
expected_counts <- N * dpois(degree_dist_in_digdis, lambda)
chi_squared <- sum((degree_freq - expected_counts)^2 / expected_counts)
df <- length(k) - 1
p_value_chi <- pchisq(chi_squared, df, lower.tail = FALSE)

# 結果の表示
cat("Chi-squared:", chi_squared, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", p_value_chi, "\n")

# 有意水準の設定
alpha <- 0.05
if (p_value_chi < alpha) {
  cat("帰無仮説は棄却されました。ポアソン型ネットワークではない可能性があります。\n")
} else {
  cat("帰無仮説は棄却されませんでした。ポアソン型ネットワークの可能性があります。\n")
}

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

