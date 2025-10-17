TimeSeries_40a<-read.csv("TimeSeries_40.csv",header=T,row.names = NULL)[,-1]


colnames(TimeSeries_40a)<-gsub("Cunningtonia_longiventralis","C_longiventralis",colnames(TimeSeries_40a))

colnames(TimeSeries_40a)<-gsub("Synodontis_multipunctatus","S_multipunctatus",colnames(TimeSeries_40a))

colnames(TimeSeries_40a)<-gsub("Variabilichromis_moorii","V_moorii",colnames(TimeSeries_40a))

colnames(TimeSeries_40a)<-gsub("Xenotilapia_flavipinnis","X_flavipinnis",colnames(TimeSeries_40a))

population<-TimeSeries_40a[,CCM_list]

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


pdf("FigS1.pdf", width = 8.27, height = 11.69)  # A4 size
par(family="Times",mfrow=c(6,3),mar=c(3,3,1,1),oma=c(1.5,1.5,0,0))
for(i in 1:length(populationList)){
  plot(populationList[[i]],xlab = "Time index",ylab="Population",type="l",ylim=c(0,460),main=bquote(italic(.(names(populationList)[i]))),xaxt = "n")
  abline(v=21,lty=2)
  text(labels="East",x=3,y=400)
  text(labels="West",x=25,y=400)
  axis(1, at = 1:length(populationList[[i]]),labels = c(years,NA,years),las=2)
}
mtext("Time index",side=1,outer=TRUE,line=0.1)
mtext("Individial number of fish species",side=2,outer=TRUE,line=0.1)
dev.off()
