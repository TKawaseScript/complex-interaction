#################
###  読み込み ###
#################

###Set R-4.2.3
### install rEDM 0.6.9
### install renv

library(renv)

renv::restore()
library(rEDM)
library(dplyr)
par(family="Times")

set.seed(1)

Hori=read.csv("horikasengasensus_ccm_16May2021.csv",header=T)


summary(Hori)
View(Hori)

All_sp_list<-unique(Hori$species)

write.csv(gsub("_",".",All_sp_list),"all_species.csv")

######################
###種名と種数の確認###
######################
SSsize<-subset(Hori,Hori$size=="SSsize")
SMLsize<-subset(Hori,Hori$size!="SSsize")
table(SMLsize$size)
View(SMLsize)
sp_list<-unique(SMLsize$species_ccm)

#sp_list=sp_list_all[c(-35)];length(sp_list)

##############
###年の確認###
##############

table(SMLsize$year);max(SMLsize$year);min(SMLsize$year)

############################################
### すべての種の年変動をタイムシリーズ化 ###
############################################

N=c(rep(0,20))
for(S in 1:length(sp_list)){
  Nr=c()
  for(Y in min(SMLsize$year):max(SMLsize$year))
  {
    Nr=c(Nr,table(Hori[SMLsize$year==Y,]$species_ccm==sp_list[S])[2])
    Nr[is.na(Nr)]=0
  }
  N=data.frame(N,Nr)
}
(N=N[2:ncol(N)])
colnames(N)=sp_list
(TN=ts(N,start=min(SMLsize$year),frequency=1))
write.csv(TN,"TimeSeries.csv")


#40年に結合

Hori_east <- subset(SMLsize,x>5)
Hori_west <- subset(SMLsize,x<6)



###東###
N=c(rep(0,20))
for(S in 1:length(sp_list)){
  Nr=c()
  for(Y in min(Hori_east$year):max(Hori_east$year))
  {
    Nr=c(Nr,table(Hori_east[Hori_east$year==Y,]$species_ccm==sp_list[S])[2])
    Nr[is.na(Nr)]=0
  }
  N=data.frame(N,Nr)
}
N=N[2:ncol(N)]
colnames(N)=sp_list
TN_east=ts(N,start=min(Hori_east$year),frequency=1)
###西###
N=c(rep(0,20))
for(S in 1:length(sp_list)){
  Nr=c()
  for(Y in min(Hori_west$year):max(Hori_west$year))
  {
    Nr=c(Nr,table(Hori_west[Hori_west$year==Y,]$species_ccm==sp_list[S])[2])
    Nr[is.na(Nr)]=0
  }
  N=data.frame(N,Nr)
}
N=N[2:ncol(N)]
colnames(N)=sp_list
TN_west=ts(N,start=min(Hori_west$year),frequency=1)
###東と西を結合###
TimeSeries_40a=cbind(rbind(TN_east,TN_west))

write.csv(TimeSeries_40a,"TimeSeries_40.csv",row.names = FALSE)


###############################################
### 観測個体数が1度も超えていないかの確認(10未満で除去)###
###############################################
TimeSeries_over10index<-vector()
for(i in 1:ncol(TimeSeries_40a)){
  TimeSeries_over10index[i]<-max(TimeSeries_40a[,i])>9
}

TimeSeries_40a[,!TimeSeries_over10index]

write.csv(colnames(TimeSeries_40a)[TimeSeries_over10index],"over10popsp.csv")

write.csv(colnames(TimeSeries_40a)[!TimeSeries_over10index],"less10popsp.csv")

TimeSeries_40<-TimeSeries_40a[,colnames(TimeSeries_40a)[TimeSeries_over10index]]

z<-scale(TimeSeries_40)


write.csv(z,"datascale.csv")

length(z)
sp_list<-colnames(z)

##########################
### Simplex Projection ###
##########################
#最適埋め込み次元数（E）

pdf("ALL_Embedding.pdf")
par(oma=c(2,2,0,1),mfrow=c(6,6),mar=c(2,2,1,1),mgp=c(10,0.7,0))
E_with_MaxRHOs=c(rep(NA,length(sp_list)))
cantSP_40=NULL
for(S in c(1:length(sp_list))){
  simp.x.all <- simplex(z[,S],pred=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),lib=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),E=c(2:8),silent=T)
  par(family="Times")
  plot(rho~E,data=simp.x.all,type="l",ylim=c(-1,1))
  #par(family="Times-Italic")
  title(main=sp_list[S])
  mtext("E",side=1,outer=T,line=0.5,cex=1.5)
  mtext("rho",side=2,outer=T,line=0.3,cex=1.3)
  abline(h=0,lty=2)
  if(max(simp.x.all$rho,na.rm=T)<0){text(5,0.8, "X",col=1,cex=2.0)}
  
  if(max(simp.x.all$rho,na.rm=T)<0){cantSP_40=c(cantSP_40,S)}
}

dev.off()

#埋め込みできないものの除外
cantSP_40;length(cantSP_40);sp_list[cantSP_40]
canElist=z[,!colnames(z)%in%sp_list[cantSP_40]]

#埋め込みできたもののlistとoutput
canE_list <- colnames(canElist)

write.csv(canE_list,"canE_list.csv")

E_with_MaxRHOs<-vector()
for(S in 1:length(sp_list)){
  simp.x.all <- simplex(z[,S],pred=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),lib=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),E=c(2:8),silent=T)
  E_with_MaxRHOs[S]<-which.max(simp.x.all$rho)+1
}




Es<-E_with_MaxRHOs # S-mapに使える、rho最大の時限の数



#############
### S-map ###
#############
#非線形性の評価

pdf("ALL_Nonlinearity.pdf")
par(oma=c(2,2,0,1),mfrow=c(6,6),mar=c(2,2,1,1),mgp=c(10,0.7,0))
LinearSP_40=NULL
for(S in c(1:length(sp_list))){
  smap <- s_map(z[,S],E=E_with_MaxRHOs[S],pred=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),lib=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),silent=T)
  par(family="Times")
  plot(rho~theta,data=smap,type="l",ylim=c(-1,1),cex.main=0.8)
  #par(family="Times-Italic")
  title(main=sp_list[[S]])
  abline(h=0,lty=2)
  if(max(smap$rho,na.rm=T)==smap[smap$theta==0,]$rho || max(smap$rho,na.rm=T)-min(smap$rho,na.rm=T)<0.001 || max(smap$rho,na.rm=T)<0){print(z[S])}
  if(max(smap$rho,na.rm=T)==smap[smap$theta==0,]$rho || max(smap$rho,na.rm=T)-min(smap$rho,na.rm=T)<0.001 || max(smap$rho,na.rm=T)<0 || max(smap$rho,na.rm=T)==smap$rho[1]){text(4,0.8, "X",col=1,cex=2.0)}
  if(max(smap$rho,na.rm=T)==smap[smap$theta==0,]$rho || max(smap$rho,na.rm=T)-min(smap$rho,na.rm=T)<0.001 || max(smap$rho,na.rm=T)<0){LinearSP_40=c(LinearSP_40,S)}
}

dev.off()

#線形性の除外
LinearSP_40;length(LinearSP_40);sp_list[LinearSP_40]
zzz_nonlinearity_list<-sp_list[-LinearSP_40]

#CCMに適応可能なものの抽出

CCM_list<-intersect(canE_list,zzz_nonlinearity_list)

TimeSeries_40_rename<-TimeSeries_40

#species nameの省略化
colnames(TimeSeries_40_rename)

colnames(TimeSeries_40_rename)<-gsub("Cunningtonia_longiventralis","C_longiventralis",colnames(TimeSeries_40_rename))

colnames(TimeSeries_40_rename)<-gsub("Synodontis_multipunctatus","S_multipunctatus",colnames(TimeSeries_40_rename))

colnames(TimeSeries_40_rename)<-gsub("Variabilichromis_moorii","V_moorii",colnames(TimeSeries_40_rename))

zzz<-z[,CCM_list]

zzz_list<-CCM_list

zzz_list<-cbind(c(1:length(zzz_list)),zzz_list)

View(zzz_list)

write.csv(CCM_list,"CCM_list.csv")




#CCMに適応可能な種名の短縮化
CCM_list<-gsub("Cunningtonia_longiventralis","C_longiventralis",CCM_list)

CCM_list<-gsub("Synodontis_multipunctatus","S_multipunctatus",CCM_list)

CCM_list<-gsub("Variabilichromis_moorii","V_moorii",CCM_list)


colnames(z)<-gsub("Cunningtonia_longiventralis","C_longiventralis",colnames(z))

colnames(z)<-gsub("Synodontis_multipunctatus","S_multipunctatus",colnames(z))

colnames(z)<-gsub("Variabilichromis_moorii","V_moorii",colnames(z))

colnames(z)<-gsub("Xenotilapia_flavipinnis","X_flavipinnis",colnames(z))





#CCMに適応するための時系列データの抽出
zzz<-z[,CCM_list]
scalesp_list<-CCM_list
View(zzz)


zzz_list<-colnames(zzz)

zzz_list<-cbind(length(zzz_list),zzz_list)

colnames(zzz)

#埋め込み次元の選択
E_with_MaxRHOs=c(rep(NA,ncol(zzz)))
for(S in c(1:ncol(zzz))){
  simp.x.all <- simplex(zzz[,S],pred=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),lib=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),E=c(2:8),silent=T)
  E_with_MaxRHOs[S]<-which.max(simp.x.all$rho)+1
}



#############
### CCM 2 ###
#############
#1~10は手動
#tp=0, 1, 2から最適な時間遅れの検出
#自動保存になっているので注意
#DD <- zzz_list[N]が種の原因側指定、このまま回すと総当たり、DD(N)を指定して解析する方が良い
#for(N in 1:length(zzz_list)){


E_list<-cbind(colnames(zzz),E_with_MaxRHOs)

zzzz<-zzz

zzz<-NULL

for(i in 1:length(E_list[,1])){
  zzz<-cbind(zzz,zzzz[,colnames(zzzz)==E_list[,1][i]])
}

colnames(zzz)<-E_list[,1]

#高速計算するためのパッケージと設定
library(doParallel)
library(foreach)
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


ncol(zzz)
ccmalltp0<-list()
ccmeach<-list()
indexalltp0<-list()
indexeach<-list()

#CCMの計算と作図
#Titleが原因

foreach(f = 1 :length(colnames(zzz))) %do% {
  DD <- colnames(zzz)[f]
  pdf(eval(paste(DD,".pdf")))
  par(oma=c(3,3,3,0),mar=c(1,1,3,1),mgp=c(10,0.7,0),mfrow=c(5,6))
  foreach(S = 1:ncol(zzz)) %do% {
    lib.size <- c((as.numeric(E_list[S,2])+1):40)
    cmxy <- ccm(zzz,E=as.numeric(E_list[S,2]),lib_column=colnames(zzz)[S],target_column=DD,lib_sizes=lib.size,pred=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),lib=c(matrix(c(1,20,21,40),ncol=2,byrow=T)),tp=0,num_samples=1000,replace=F,RNGseed=1)
    ccmeach[[S]]<-cmxy
    x_xmap_y_means <- ccm_means(cmxy,na.rm=T)
    cmxy_sd <-list()
    cmxy_sd <-lapply(min(x_xmap_y_means$lib_size):max(x_xmap_y_means$lib_size), function(i) sd(subset(cmxy,cmxy$lib_size==i)$rho,na.rm=T))
    cmxy_sd[is.na(cmxy_sd)] <- 0
    cmxy_rho_upper<-vector()
    foreach(i = 1:nrow(x_xmap_y_means)) %do% {
      cmxy_rho_upper[i]<-x_xmap_y_means$rho[[i]]+cmxy_sd[[i]]
    }
    cmxy_rho_downer<-vector()
    foreach(i = 1:nrow(x_xmap_y_means)) %do% {
      cmxy_rho_downer[i]<-x_xmap_y_means$rho[[i]]-cmxy_sd[[i]]
    }
    par(family="Times")
    plot(x_xmap_y_means$lib_size,x_xmap_y_means$rho,type="l",ylim=c(-1,1),col=4,xlab="",ylab="")
    abline(h=0,lty=2)
    abline(h=max(quantile(subset(cmxy,cmxy$lib_size==min(cmxy$lib_size))$rho,p=c(0.025,0.5,0.975),na.rm = T)))
    lines(x_xmap_y_means$lib_size,cmxy_rho_downer,col=4,lty=2)
    lines(x_xmap_y_means$lib_size,cmxy_rho_upper,col=4,lty=2)
    AA <- subset(cmxy,cmxy$lib_size==(E_list[S,2]))$rho
    BB <- AA[!is.na(AA)]
    BB<-BB[!is.infinite(BB)]
    CC <- mean(BB)
    AAA<- subset(cmxy,cmxy$lib_size==(E_list[S,2]))$rho
    BBB<- AAA[!is.na(AAA)]
    BBB<-BBB[!is.infinite(BBB)]
    CCC<- sd(BBB)
    index<-max(quantile(subset(cmxy,lib_size==min(cmxy$lib_size))$rho,p=c(0.025,0.5,0.975),na.rm=T))
    #par(family="Times-Italic")
    if(tail(x_xmap_y_means,n=1)$rho>index&&tail(x_xmap_y_means,n=1)$rho>0&&!anyNA(x_xmap_y_means$rho)){
      mtext(zzz_list[S,2],side=3,line=1,col=2,cex=0.7)
    }else{
      mtext(zzz_list[S,2],side=3,line=1,col=1,cex=0.7)
    }
    
    indexeach[S]<-index
    
    #if(zzz_list[S]==DD){mtext((max(x_xmap_y_means$lib_size,na.rm=T)+min(x_xmap_y_means$lib_size,na.rm=T))/2,0.95,"X",col=1,cex=2)}
    par(family="Times")
    mtext("Library Size",side=1,outer=T,line=1.5,cex=1.5)
    mtext("rho",side=2,outer=T,line=1,cex=1.5)
    par(family="Times")
    mtext(DD,side=3,outer=T,line=0.3,cex=1.5)
    
  }
  dev.off()
  ccmalltp0[[f]]<-ccmeach
  indexalltp0[[f]]<-indexeach
  
}


stopCluster(cl)




#CCMで推定された因果関係のリスト化
cal.listtp0<-mapply(rep, 1:length(ccmalltp0),0)

#8/28一旦CCMの内容を保存する


for(f in 1:length(ccmalltp0)){
  for(S in 1:length(ccmalltp0[[f]])){
    if(tail(ccmalltp0[[f]][[S]],n=1)$rho>indexalltp0[[f]][[S]]&&tail(ccmalltp0[[f]][[S]],n=1)$rho>0&&!is.nan(tail(ccmalltp0[[f]][[S]],n=1)$rho)){
      cal.listtp0[[f]][S]<-zzz_list[S,2]
    }else{
      cal.listtp0[[f]][S]<-NA
    }
  }
}


names(cal.listtp0)<-colnames(zzz)

cal.listtp0<-lapply(cal.listtp0,function(v){v[!is.na(v)]})


cal.list.alltp0<-list()
for(i in 1:length(cal.listtp0)){
  cal.list.alltp0[[i]]<-cbind(rep(colnames(zzz)[i],length(cal.listtp0[[i]])),cal.listtp0[[i]])
}


listtp0<-NULL
for(i in 1:length(cal.list.alltp0)){
  listtp0<-rbind(listtp0,cal.list.alltp0[[i]])
}


#CCMで推定された因果関係のリスト整形とoutput
listtp0<-as.data.frame(listtp0)

colnames(listtp0)<-c("cause","effect")

nodelisttp0cause<-unique(listtp0[,1])
nodelisttp0effect<-unique(listtp0[,2])

sort(nodelisttp0cause)==sort(nodelisttp0effect)

countcausetp0<-t(table(listtp0[,1]))
counteffecttp0<-t(table(listtp0[,2]))


#ver1.10.3の例
z = read.csv("datascale.csv")

colnames(z)[1] <- "Time"

lib = "1 40"
pred = "1 40"
libSize = "3 39 1"

#A_compressicepsを例に計算

colnames(z)[1] <- "Time"

lib = "1 40"
pred = "1 40"
libSize = "3 39 1"

#A_compressicepsを例に計算

rho_E <- EmbedDimension(dataFrame = z,lib = lib,pred = pred,maxE = 8,
                        columns = "A_compressiceps", target = "A_compressiceps",showPlot = TRUE)


rho_theta <- PredictNonlinear(dataFrame = z,lib = lib,pred = pred,
                              columns = "A_compressiceps", target = "A_compressiceps",E = which.max(rho_E$rho))

cmap <- CCM(dataFrame = z, E = which.max(rho_E$rho), Tp = 0, columns = "A_compressiceps",
            target = "N_fasciatus", libSize = libSize, sample = 100, includeData = TRUE, parameterList = TRUE, seed = 1, showPlot = TRUE)

#A_compressiceps xmap N_fasciatus (blue)
head(cmap$CCM1_PredictStat)
#N_fasciatus xmap A_compressiceps (red)
head(cmap$CCM2_PredictStat)
