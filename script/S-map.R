library(rEDM)
library(dplyr)

sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T)
Smapbasedata <- read.csv(datascale.csv,header=T)[-1]

#or use z from CCM.R
#Smapbasedata<-z
par(mfrow=c(1,1))

effectlist000<-unique(listtp0[,2])
listtp0$effect

effectlist00<-list()
for(i in 1:length(effectlist000)){
  effectlist00[[i]]<-subset(listtp0,listtp0[,2]==effectlist000[i])
}

eachunlist0<-list()
for(i in 1:length(effectlist00)){
  eachunlist0[[i]]<-unique(c(effectlist00[[i]]$effect,effectlist00[[i]]$cause))
}

effectlisttp0<-vector()
for(i in 1:length(effectlist00)){
  effectlisttp0[i]<-unique(effectlist00[[i]]$effect)
}

effectlisttp0<-gsub("Cunningtonia_longiventralis","C_longiventralis",effectlisttp0)
effectlisttp0<-gsub("Synodontis_multipunctatus","S_multipunctatus",effectlisttp0)
effectlisttp0<-gsub("Variabilichromis_moorii","V_moorii",effectlisttp0)
names(eachunlist0)<-effectlisttp0

#Abbreviations of species names
colnames(Smapbasedata)<-gsub("Synodontis_multipunctatus","S_multipunctatus",colnames(Smapbasedata))
colnames(Smapbasedata)<-gsub("Cunningtonia_longiventralis","C_longiventralis",colnames(Smapbasedata))
colnames(Smapbasedata)<-gsub("Variabilichromis_moorii","V_moorii",colnames(Smapbasedata))

for(i in 1:length(names(eachunlist0))){
  eachunlist0[[i]]<-gsub("Synodontis_multipunctatus","S_multipunctatus",eachunlist0[[i]])
  eachunlist0[[i]]<-gsub("Cunningtonia_longiventralis","C_longiventralis",eachunlist0[[i]])
  eachunlist0[[i]]<-gsub("Variabilichromis_moorii","V_moorii",eachunlist0[[i]])
}

#Recipient species
spdatasmap<-list()
for(i in 1:length(names(eachunlist0))){
  spdatasmap[[i]]<-Smapbasedata[,eachunlist0[[i]]]
}


#Smap
spthetas<-list()
for(i in 1:length(spdatasmap)){
  spthetas[[i]]<-block_lnlp(spdatasmap[[i]],method="s-map",theta=c(0,1e-04,3e-04,0.001,0.003,0.01,0.03,0.1,0.3,0.5,0.75,1,1.5,2,3,4,6,8),silent=T,lib=c(matrix(c(1,20,21,40),ncol=2, byrow=T)),pred=c(matrix(c(1,20,21,40),ncol=2, byrow=T)))
}

spbesttheta<-list()
for(i in 1:length(spthetas)){
  spbesttheta[[i]]<-spthetas[[i]][which.max(spthetas[[i]]$rho),]$theta
}

spdatasmaplnlp<-list()
for(i in 1:length(spdatasmap)){
  spdatasmaplnlp[[i]]<-block_lnlp(spdatasmap[[i]],method="s-map",theta=spbesttheta[[i]],pred = c(matrix(c(1,20,21,40),ncol=2, byrow=T)),lib=c(matrix(c(1,20,21,40),ncol=2, byrow=T)),silent=T,save_smap_coefficients=T)
}

names(spdatasmaplnlp)<-names(eachunlist0)

#SMAP coefficient
spsmap<-list()
for(i in 1:length(spdatasmaplnlp)){
  first<-spdatasmaplnlp[[i]]$smap_coefficients[[1]][c(1:19),]
  end <- spdatasmaplnlp[[i]]$smap_coefficients[[1]][c(21:39),]
  spsmap[[i]]<-rbind(first,NA,end)
}

for(i in 1:length(spdatasmap)){
  colnames(spsmap[[i]])<-c(eachunlist0[[i]],"b")
}

names(spsmap)<-names(eachunlist0)
names(spthetas)<-names(eachunlist0)

spsmaptp0<-spsmap
spthetastp0<-spthetas

renames<-gsub("_",".",sp_food_coltp0$cause)
save(spsmap,file="spsmap.Rdata")


#box plot of smap coefficient
pdf("S_map_coef_tp0.pdf")

par(mgp=c(4,1,0),family="Times")
for(i in 1:length(spsmaptp0)){
  boxplot(spsmaptp0[[i]],cex.axis=0.8,xlab="Species",ylab="S-map coefficients",cex.names=0.5,main=names(spsmaptp0[i]),las=2,family="mono")
  
  abline(h=0,lty=2)
}

dev.off()


#Summarize Smap coefficients (min,max,3rd Qu...)
d <- data.frame() 
d1 <- data.frame()  
d2 <- data.frame() 
d3 <- NULL 
d5 <- NULL 
d5tp0 <- list() 
d6 <- c("Min.   :", "1st Qu.:", "Median :", "Mean   :", "3rd Qu.:", "Max.   :","NA's  :")  

for(i in 1:length(spsmap)) {  
  d1 <- data.frame(apply(spsmap[[i]], 2, summary))  # summaryを計算
  
  d <- rep(effectlisttp0[[i]], nrow(d1) * ncol(d1))  
  
  for(j in 1:ncol(spsmap[[i]])) {    
    d3 <- c(d3, rep(colnames(spsmap[[i]])[j], nrow(d1)))  
    d5 <- c(d5, d1[,j]) 
  }
  
  index_length <- length(d)  
  d2 <- data.frame(
    "effect" = d,
    "cause" = d3,
    "index" = rep(d6, length.out = index_length),  
    "menas" = d5
  )
  
  d5tp0[[i]] <- d2 
  
  d3 <- NULL  
  d5 <- NULL  
}


d7<-list()

for(i in 1:length(d5tp0)){
  d7[[i]]<-d5tp0[[i]] %>%mutate(NewCol =paste(!!!rlang::syms(c("index", "menas")), sep=""))
}

d4tp0<-list()
for(i in 1:length(d7)){
  d4tp0[[i]]<-data.frame("effect"=d7[[i]]$effect,"cause"=d7[[i]]$cause,"menas"=d7[[i]]$NewCol)
}
bind_rows(d4tp0)

for(i in 1: length(d4tp0)){
  d4tp0[[i]] <- d4tp0[[i]] %>%　filter(cause!="b")
}

write.csv(as.data.frame(bind_rows(d4tp0)),"Smapbasedatatp0.csv")

#Population size
popmean<-as.data.frame(apply(TimeSeries_40,2,mean))
colnames(popmean)<-c("mean")

popsd<-as.data.frame(apply(TimeSeries_40,2,sd))
colnames(popsd)<-c("sd")

#Abbreviations of species names
rownames(popmean)<-gsub("Variabilichromis_moorii","V_moorii",rownames(popmean))
rownames(popmean)<-gsub("Synodontis_multipunctatus","S_multipunctatus",rownames(popmean))

causemeantp0<-vector()

causemeantp0<-gsub(rownames(popmean)[1],popmean[1,],listtp0$cause)
causemeantp0<-gsub("Synodontis_multipunctatus","S_multipunctatus",causemeantp0)
causemeantp0<-gsub("Variabilichromis_moorii","V_moorii",causemeantp0)

causemeantp0<-gsub(rownames(popmean)[2],popmean[2,],causemeantp0)

for(i in 3:nrow(popmean)){
  causemeantp0<-gsub(rownames(popmean)[i],popmean[i,],causemeantp0)
}

causemeantp0<-as.numeric(causemeantp0)

rownames(popsd)<-gsub("Variabilichromis_moorii","V_moorii",rownames(popsd))
rownames(popsd)<-gsub("Synodontis_multipunctatus","S_multipunctatus",rownames(popsd))

causesdtp0<-vector()

causesdtp0<-gsub(rownames(popsd)[1],popsd[1,],listtp0$cause)
causesdtp0<-gsub("Synodontis_multipunctatus","S_multipunctatus",causesdtp0)
causesdtp0<-gsub("Variabilichromis_moorii","V_moorii",causesdtp0)

causesdtp0<-gsub(rownames(popsd)[2],popsd[2,],causesdtp0)

for(i in 3:nrow(popmean)){
  causesdtp0<-gsub(rownames(popsd)[i],popsd[i,],causesdtp0)
}

causesdtp0<-as.numeric(causesdtp0)

effectmeantp0<-vector()

effectmeantp0<-gsub(rownames(popmean)[1],popmean[1,],listtp0$effect)
effectmeantp0<-gsub("Synodontis_multipunctatus","S_multipunctatus",effectmeantp0)
effectmeantp0<-gsub("Variabilichromis_moorii","V_moorii",effectmeantp0)
effectmeantp0<-gsub(rownames(popmean)[2],popmean[2,],effectmeantp0)

for(i in 3:nrow(popmean)){
  effectmeantp0<-gsub(rownames(popmean)[i],popmean[i,],effectmeantp0)
}

effectmeantp0<-as.numeric(effectmeantp0)

effectsdtp0<-vector()

effectsdtp0<-gsub(rownames(popmean)[1],popmean[1,],listtp0$effect)
effectsdtp0<-gsub("Synodontis_multipunctatus","S_multipunctatus",effectsdtp0)
effectsdtp0<-gsub("Variabilichromis_moorii","V_moorii",effectsdtp0)

effectsdtp0<-gsub(rownames(popmean)[2],popmean[2,],effectsdtp0)

for(i in 3:nrow(popmean)){
  effectsdtp0<-gsub(rownames(popsd)[i],popsd[i,],effectsdtp0)
}
effectsdtp0<-as.numeric(effectsdtp0)

effecthabtp0<-vector()
for(i in 1:length(listtp0$effect)){
  effecthabtp0[i] = sp_food_coltp0[listtp0$effect[i]==sp_food_coltp0$cause,]$foodhabitat
}

causehabtp0<-vector()

for(i in 1:length(listtp0$cause)){
  causehabtp0[i] = sp_food_coltp0[listtp0$cause[i]==sp_food_coltp0$cause,]$foodhabitat
}

#Make a dataframe for Smap coefficients
basedatatp0<-cbind(listtp0$cause,listtp0$effect,causemeantp0,effectmeantp0,causesdtp0,effectsdtp0,causehabtp0,effecthabtp0)

colnames(basedatatp0)<-c("cause","effect","causemeantp0","effectmeantp0","causesdtp0","effectsdtp0","causehabtp0","effecthabtp0")
#colnames(basedatatp0)<-c("cause","effect","strength","causepopmean","effectpopmean","causepopsd","effectpopsd","causehabitat","effecthabitat")

write.csv(basedatatp0,"basedatatp0.csv")

#causative species
causetp0count<-vector()

for(i in 1:length(unique(listtp0$cause))){
  causetp0count[i]<-length(subset(listtp0$cause,listtp0$cause==unique(listtp0$cause)[i]))
}

intercausetp0<-unique(listtp0$cause)

foodhabitcausetp0<-NULL

for(i in 1:length(intercausetp0)){
  foodhabitcausetp0<-c(foodhabitcausetp0,sp_food_coltp0[sp_food_coltp0$cause==intercausetp0[i],3])
}

write(intercausetp0,"intercausetp0.txt")


names(d4tp0)<-effectlisttp0
meanstp0eachmin<-list()
meanstp0each1st<-list()
meanstp0eachmedian<-list()
meanstp0eachmean<-list()
meanstp0each3rd<-list()
meanstp0eachmax<-list()
Smaptp0summaryeach<-list()
Smaptp0summary<-list()

#
for(i in 1:length(d4tp0)){
  meanstp0eachmin[[i]]<-d4tp0[[i]]$menas[grep("Min",d4tp0[[i]]$menas)]
  meanstp0eachmin[[i]]<-as.numeric(gsub("Min.   :","",meanstp0eachmin[[i]]))
  #meanstp0eachspmin<-rbind(meanstp0eachspmin,meanstp0eachmin)
  
  meanstp0each1st[[i]]<-d4tp0[[i]]$menas[grep("1st",d4tp0[[i]]$menas)]
  meanstp0each1st[[i]]<-as.numeric(gsub("1st Qu.:","",meanstp0each1st[[i]]))
  #meanstp0eachsp1st<-rbind(meanstp0eachsp1st,meanstp0each1st)
  
  meanstp0eachmedian[[i]]<-d4tp0[[i]]$menas[grep("Median",d4tp0[[i]]$menas)]
  meanstp0eachmedian[[i]]<-as.numeric(gsub("Median :","",meanstp0eachmedian[[i]]))
  #meanstp0eachspmedian<-rbind(meanstp0eachspmedian,meanstp0eachmedian)
  
  meanstp0eachmean[[i]]<-d4tp0[[i]]$menas[grep("Mean",d4tp0[[i]]$menas)]
  meanstp0eachmean[[i]]<-as.numeric(gsub("Mean   :","",meanstp0eachmean[[i]]))
  #meanstp0eachspmean<-rbind(meanstp0eachspmean,meanstp0eachmean)
  
  meanstp0each3rd[[i]]<-d4tp0[[i]]$menas[grep("3rd",d4tp0[[i]]$menas)]
  meanstp0each3rd[[i]]<-as.numeric(gsub("3rd Qu.:","",meanstp0each3rd[[i]]))
  #meanstp0eachsp3rd<-rbind(meanstp0eachsp3rd,meanstp0each3rd)
  
  meanstp0eachmax[[i]]<-d4tp0[[i]]$menas[grep("Max",d4tp0[[i]]$menas)]
  meanstp0eachmax[[i]]<-as.numeric(gsub("Max.   :","",meanstp0eachmax[[i]]))
  #meanstp0eachspmax<-rbind(meanstp0eachspmax,meanstp0eachmax)
}

#add recipent species
names(meanstp0eachmin)<-names(d4tp0)
#meanstp0eachspmin<-rbind(meanstp0eachspmin,meanstp0eachmin)

names(meanstp0each1st)<-names(d4tp0)
#meanstp0eachsp1st<-rbind(meanstp0eachsp1st,meanstp0each1st)

names(meanstp0eachmedian)<-names(d4tp0)
#meanstp0eachspmedian<-rbind(meanstp0eachspmedian,meanstp0eachmedian)

names(meanstp0eachmean)<-names(d4tp0)
#meanstp0eachspmean<-rbind(meanstp0eachspmean,meanstp0eachmean)

names(meanstp0each3rd)<-names(d4tp0)
#meanstp0eachsp3rd<-rbind(meanstp0eachsp3rd,meanstp0each3rd)

names(meanstp0eachmax)<-names(d4tp0)

#Make a list
Smaptp0summaryeachs<-list()
for(i in 1:length(meanstp0eachmin)){
  Smaptp0summaryeachs[[i]]<-cbind(rep(unique(d4tp0[i][[1]]$effect),length(unique(d4tp0[i][[1]]$cause))),
                                  unique(d4tp0[i][[1]]$cause),
                                  meanstp0eachmin[i][[1]],
                                  meanstp0each1st[i][[1]],
                                  meanstp0eachmedian[i][[1]],
                                  meanstp0eachmean[i][[1]],
                                  meanstp0each3rd[i][[1]],
                                  meanstp0eachmax[i][[1]])
}

names(Smaptp0summaryeachs)<-names(d4tp0)

for(i in 1:length(Smaptp0summaryeachs)){
  colnames(Smaptp0summaryeachs[[i]])<-c("effect","cause","min","1st","median","mean","3rd","max")
}

#Make a dataframe
Smaptp0summaryeach<-NULL
for(i in 1:length(Smaptp0summaryeachs)){
  Smaptp0summaryeach<-rbind(Smaptp0summaryeach,Smaptp0summaryeachs[i][[1]])
}

#delete " "
Smaptp0summaryeach<-gsub(" ","",Smaptp0summaryeach)

Smaptp0summaryeach<-as.data.frame(Smaptp0summaryeach)

#ratio
retioCount<-function(x){if(sum(x>0,na.rm = T)/38>0.5){
  return(sum(x>0)/38)
}else{
  return(sum(x<0)/38)
}
}

forEdgeGrade <- function(x) {
  cleaned <- x[complete.cases(x)] 
  X <- retioCount(cleaned)        
  return(X)
}

#Make vectors
list_names <- names(spsmaptp0)
result <- unlist(lapply(list_names, function(lst_name) {
  
  col_names <- colnames(spsmaptp0[[lst_name]])
  
  if (!is.null(col_names) && length(col_names) > 0) {

    paste(col_names, lst_name, sep = "_")
  } else {
    NULL 
  }
  }))

print(result)

ratios<-list()
for (i in 1:length(spsmaptp0)) {
  ratios[[i]] <- apply(spsmaptp0[[i]], 2, forEdgeGrade)
}

ratios<-unlist(ratios)

ratio_name<-cbind(ratios,result)

#Positive or negative
strengthTFtp0<-as.numeric(Smaptp0summaryeach$mean)>0

strengthTFtp0<-gsub(strengthTFtp0,pattern="TRUE",replacement="positive")
strengthTFtp0<-gsub(strengthTFtp0,pattern="FALSE",replacement="negative")

Smaptp0summary<-data.frame(Smaptp0summaryeach,"strength"=strengthTFtp0)

basedatatp0 <- cbind(basedatatp0,paste(basedatatp0[,1],basedatatp0[,2],sep="_"))

basedatatp0<-as.data.frame(basedatatp0)
colnames(basedatatp0)<-c("cause","effect","causemeantp0","effectmeantp0","causesdtp0","effectsdtp0","causehabtp0","effecthabtp0","cause_effect")

Smaptp0summary<-cbind(Smaptp0summary,paste(Smaptp0summary$cause,Smaptp0summary$effect,sep="_"))

colnames(Smaptp0summary)<-c("effect","cause","min","X1st","median","mean","X3rd","max","strength","cause_effect")

checktp0 <- charmatch(basedatatp0$cause_effect,Smaptp0summary$cause_effect)
checktp0

library(dplyr)
GLMbasedata <- left_join(basedatatp0,Smaptp0summary, by = "cause_effect")

ratio_name<-as.data.frame(ratio_name)

colnames(ratio_name)<-c("ratio","cause_effect")
GLMbasedatatp0<-left_join(GLMbasedata,ratio_name, by = "cause_effect")

write.csv(GLMbasedatatp0,"GLMbasedatatp0.csv")
