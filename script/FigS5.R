
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
library(cowplot)
library(poweRlaw)



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

#全ての因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_All_all_digdis <- igraph::degree(igraphdatatp0ww8,mode = "all")
write.csv(degree_dist_All_all_digdis,"degree_dist_All_all_digdis.csv")

degree_freq_all <- degree_dist_All_all_digdis$x
m_all<-displ$new(degree_freq_all)
est_xmin_all<-estimate_xmin(m_all)

m_all$setXmin(est_xmin_all)

est_pars_all<-estimate_pars(m_all)
m_all$setPars(est_pars_all)

bootstrap_all<-bootstrap_p(m_all,no_of_sims=500,threads=2)
#p値
bootstrap_all$p

#powerLowの係数
est_xmin_all$pars


pdf("FigS5_a.pdf",width=8,height=4)
# barabashiデータのプロット
plot(m_all, 
     main="Degree distribution with power-law fit for the all interaction network",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=1, 
     col="black", 
     pch=16)
text(x=3, y=0.15, labels=paste("γ=", round(est_xmin_all$pars, digits=3)))
text(x=3, y=0.1, labels=paste("p=", round(bootstrap_all$p, digits=3)))

# パワーローのフィットを重ねる
lines(m_all, col="red", lwd=2)
dev.off()

#原因側の因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_out_digdis <- igraph::degree(igraphdatatp0ww8,mode = "out")
write.csv(degree_dist_out_digdis,"degree_dist_out_digdis.csv")

degree_freq_out <- degree_dist_out_digdis$x
m_out<-displ$new(degree_freq_out)
est_xmin_out<-estimate_xmin(m_out)

m_out$setXmin(est_xmin_out)

est_pars_out<-estimate_pars(m_out)
m_out$setPars(est_pars_out)

bootstrap_out<-bootstrap_p(m_out,no_of_sims=500,threads=2)
#p値
bootstrap_out$p

#powerLowの係数
est_xmin_out$pars


pdf("FigS5_b.pdf")
# barabashiデータのプロット
plot(m_out, 
     main="Degree distribution with power-law fit for causality",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=1, 
     col="black", 
     pch=16)
text(x=3, y=0.15, labels=paste("γ=", round(est_xmin_out$pars, digits=3)))
text(x=3, y=0.1, labels=paste("p=", round(bootstrap_out$p, digits=3)))

# パワーローのフィットを重ねる
lines(m_out, col="red", lwd=2)
dev.off()





#結果側の因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_in_digdis <- igraph::degree(igraphdatatp0ww8,mode = "in")
write.csv(degree_dist_in_digdis,"degree_dist_in_digdis.csv")

degree_freq_in <- degree_dist_in_digdis$x
degree_freq_in<-degree_freq_in[degree_freq_in!=0]

m_in<-displ$new(degree_freq_in)
est_xmin_in<-estimate_xmin(m_in)

m_in$setXmin(est_xmin_in)

est_pars_in<-estimate_pars(m_in)
m_in$setPars(est_pars_in)

bootstrap_in<-bootstrap_p(m_in,no_of_sims=500,threads=2)
#p値
bootstrap_in$p

#powerLowの係数
est_xmin_in$pars

pdf("FigS5_c.pdf")
# barabashiデータのプロット
plot(m_in, 
     asp = 1,
     main="Degree distribution with power-law fit for recipient",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=1, 
     col="black", 
     pch=16,
     )
text(x=3, y=0.15, labels=paste("γ=", round(est_xmin_in$pars, digits=3)))
text(x=3, y=0.1, labels=paste("p=", round(bootstrap_in$p, digits=3)))

# パワーローのフィットを重ねる
lines(m_in, col="red", lwd=2)
dev.off()


pdf("FigS5.pdf", width=8, height=6)  # 高さを少し増やすとバランスが良くなります
par(mfrow=c(1,1))  # 2行2列のレイアウト

plot(m_all, 
     main="Degree distribution with power-law fit for the all interaction network",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=1, 
     col="black", 
     pch=16)
text(x=3, y=0.15, labels=paste("γ=", round(est_xmin_all$pars, digits=3)))
text(x=3, y=0.1, labels=paste("p=", round(bootstrap_all$p, digits=3)))

# パワーローのフィットを重ねる
lines(m_all, col="red", lwd=2)

par(mfrow=c(1,2))  # 2行2列のレイアウト

plot(m_out, 
     main="Degree distribution with power-law fit for causality",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=1, 
     col="black", 
     pch=16)
text(x=3, y=0.15, labels=paste("γ=", round(est_xmin_out$pars, digits=3)))
text(x=3, y=0.1, labels=paste("p=", round(bootstrap_out$p, digits=3)))

# パワーローのフィットを重ねる
lines(m_out, col="red", lwd=2)


plot(m_in, 
     asp = 1,
     main="Degree distribution with power-law fit for recipient",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=1, 
     col="black", 
     pch=16,
)
text(x=3, y=0.15, labels=paste("γ=", round(est_xmin_in$pars, digits=3)))
text(x=3, y=0.1, labels=paste("p=", round(bootstrap_in$p, digits=3)))

# パワーローのフィットを重ねる
lines(m_in, col="red", lwd=2)

dev.off()

