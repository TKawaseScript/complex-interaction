#FigS2
library(gridExtra)
library(ggplot2)

load("spsmap.Rdata")


index<-NULL

for(i in 1:length(spsmaptp0)){
  index[i]<-ncol(spsmaptp0[[i]])>2
}

SmapInterListWithIntB<-spsmaptp0[index]

SmapInterOnly<-list()
for(i in 1:length(SmapInterListWithIntB)){
SmapInterOnly[[i]]<-SmapInterListWithIntB[[i]][,c(-1,-ncol(SmapInterListWithIntB[[i]]))]
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

dataframe_Smap_L_atts<-NULL


dataframe_Smap_L_atts <- c(SmapInterOnly$L_attenuatus[c(1:19)],rep(NA,1),SmapInterOnly$L_attenuatus[c(20:38)])
dataframe_Smap_L_atts <- cbind(dataframe_Smap_L_atts,rep("P_trewavasae",length(dataframe_Smap_L_atts)))

dataframe_Smap_L_atts<-data.frame("Smap"=dataframe_Smap_L_atts[,1],"cause"=dataframe_Smap_L_atts[,2],"index"=c(1:39))

dataframe_Smap_L_atts$Smap<-as.numeric(dataframe_Smap_L_atts$Smap)
dataframe_Smap_L_atts$index<-as.numeric(dataframe_Smap_L_atts$index)

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
for_all_A_dew<-cbind(for_all_A_dew,rep(paste(for_all_A_dew$effect,"-",for_all_A_dew$cause))
colnames(for_all_A_dew)<-c("Smap","cause","index","effect","effect_cause")

for_all_I_loo<-cbind(dataframe_Smap_I_loo,rep("I_loocki",nrow(dataframe_Smap_I_loo)))
colnames(for_all_I_loo)<-c("Smap","cause","index","effect")
for_all_I_loo<-cbind(for_all_I_loo,rep(paste(for_all_I_loo$effect,"-",for_all_I_loo$cause)))
colnames(for_all_I_loo)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_cal<-cbind(dataframe_Smap_L_cal,rep("L_callipterus",nrow(dataframe_Smap_L_cal)))
colnames(for_all_L_cal)<-c("Smap","cause","index","effect")
for_all_L_cal<-cbind(for_all_L_cal,rep(paste(for_all_L_cal$effect,"-",for_all_L_cal$cause)))
colnames(for_all_L_cal)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_elo<-cbind(dataframe_Smap_L_elo,rep("L_elongatus",nrow(dataframe_Smap_L_elo)))
colnames(for_all_L_elo)<-c("Smap","cause","index","effect")
for_all_L_elo<-cbind(for_all_L_elo,rep(paste(for_all_L_elo$effect,"-",for_all_L_elo$cause)))
colnames(for_all_L_elo)<-c("Smap","cause","index","effect","effect_cause")

for_all_N_fas<-cbind(dataframe_Smap_N_fas,rep("N_fasciatus",nrow(dataframe_Smap_N_fas)))
colnames(for_all_N_fas)<-c("Smap","cause","index","effect")
for_all_N_fas<-cbind(for_all_N_fas,rep(paste(for_all_N_fas$effect,"-",for_all_N_fas$cause)))
colnames(for_all_N_fas)<-c("Smap","cause","index","effect","effect_cause")


for_all_N_sex<-cbind(dataframe_Smap_N_sex,rep("N_sexpressiceps",nrow(dataframe_Smap_N_sex)))
colnames(for_all_N_sex)<-c("Smap","cause","index","effect")
for_all_N_sex<-cbind(for_all_N_sex,rep(paste(for_all_N_sex$effect,"-",for_all_N_sex$cause)))
colnames(for_all_N_sex)<-c("Smap","cause","index","effect","effect_cause")


for_all_O_ven<-cbind(dataframe_Smap_O_ven,rep("O_ventralis",nrow(dataframe_Smap_O_ven)))
colnames(for_all_O_ven)<-c("Smap","cause","index","effect")
for_all_O_ven<-cbind(for_all_O_ven,rep(paste(for_all_O_ven$effect,"-",for_all_O_ven$cause)))
colnames(for_all_O_ven)<-c("Smap","cause","index","effect","effect_cause")

for_all_P_mic<-cbind(dataframe_Smap_P_mic,rep("P_microlepis",nrow(dataframe_Smap_P_mic)))
colnames(for_all_P_mic)<-c("Smap","cause","index","effect")
for_all_P_mic<-cbind(for_all_P_mic,rep(paste(for_all_P_mic$effect,"-",for_all_P_mic$cause)))
colnames(for_all_P_mic)<-c("Smap","cause","index","effect","effect_cause")


for_all_P_tre<-cbind(dataframe_Smap_P_tre,rep("P_trewavasae",nrow(dataframe_Smap_P_tre)))
colnames(for_all_P_tre)<-c("Smap","cause","index","effect")
for_all_P_tre<-cbind(for_all_P_tre,rep(paste(for_all_P_tre$effect,"-",for_all_P_tre$cause)))
colnames(for_all_P_tre)<-c("Smap","cause","index","effect","effect_cause")


for_all_T_moo<-cbind(dataframe_Smap_T_moo,rep("T_moorii",nrow(dataframe_Smap_T_moo)))
colnames(for_all_T_moo)<-c("Smap","cause","index","effect")
for_all_T_moo<-cbind(for_all_T_moo,rep(paste(for_all_T_moo$effect,"-",for_all_T_moo$cause)))
colnames(for_all_T_moo)<-c("Smap","cause","index","effect","effect_cause")


for_all_T_vit<-cbind(dataframe_Smap_T_vit,rep("T_vittatus",nrow(dataframe_Smap_T_vit)))
colnames(for_all_T_vit)<-c("Smap","cause","index","effect")
for_all_T_vit<-cbind(for_all_T_vit,rep(paste(for_all_T_vit$effect,"-",for_all_T_vit$cause)))
colnames(for_all_T_vit)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_dar<-cbind(dataframe_Smap_L_dar,rep("L_dardennii",nrow(dataframe_Smap_L_dar)))
colnames(for_all_L_dar)<-c("Smap","cause","index","effect")
for_all_L_dar<-cbind(for_all_L_dar,rep(paste(for_all_L_dar$effect,"-",for_all_L_dar$cause)))
colnames(for_all_L_dar)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_tan<-cbind(dataframe_Smap_L_tan,rep("L_tanganicanus",nrow(dataframe_Smap_L_tan)))
colnames(for_all_L_tan)<-c("Smap","cause","index","effect")
for_all_L_tan<-cbind(for_all_L_tan,rep(paste(for_all_L_tan$effect,"-",for_all_L_tan$cause)))
colnames(for_all_L_tan)<-c("Smap","cause","index","effect","effect_cause")



for_all_J_orn<-cbind(dataframe_Smap_J_orn,rep("J_ornatus",nrow(dataframe_Smap_J_orn)))
colnames(for_all_J_orn)<-c("Smap","cause","index","effect")
for_all_J_orn<-cbind(for_all_J_orn,rep(paste(for_all_J_orn$effect,"-",for_all_J_orn$cause)))
colnames(for_all_J_orn)<-c("Smap","cause","index","effect","effect_cause")

for_all_Par<-cbind(dataframe_Smap_Par,rep("Paracyprichromisspp",nrow(dataframe_Smap_Par)))
colnames(for_all_Par)<-c("Smap","cause","index","effect")
for_all_Par<-cbind(for_all_Par,rep(paste(for_all_Par$effect,"-",for_all_Par$cause)))
colnames(for_all_Par)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_att<-cbind(dataframe_Smap_L_atts,rep("L_attenuatus",nrow(dataframe_Smap_L_atts)))
colnames(for_all_L_att)<-c("Smap","cause","index","effect")
for_all_L_att<-cbind(for_all_L_att,rep(paste(for_all_L_att$effect,"-",for_all_L_att$cause)))
colnames(for_all_L_att)<-c("Smap","cause","index","effect","effect_cause")


for_all_X_pap<-cbind(dataframe_Smap_X_pap,rep("X_papilio",nrow(dataframe_Smap_X_pap)))
colnames(for_all_X_pap)<-c("Smap","cause","index","effect")
for_all_X_pap<-cbind(for_all_X_pap,rep(paste(for_all_X_pap$effect,"-",for_all_X_pap$cause)))
colnames(for_all_X_pap)<-c("Smap","cause","index","effect","effect_cause")


for_all_V_moo<-cbind(dataframe_Smap_V_moo,rep("V__moorii",nrow(dataframe_Smap_V_moo)))
colnames(for_all_V_moo)<-c("Smap","cause","index","effect")
for_all_V_moo<-cbind(for_all_V_moo,rep(paste(for_all_V_moo$effect,"-",for_all_V_moo$cause)))
colnames(for_all_V_moo)<-c("Smap","cause","index","effect","effect_cause")


for_all_P_pol<-cbind(dataframe_Smap_P_pol,rep("P_polyodon",nrow(dataframe_Smap_P_pol)))
colnames(for_all_P_pol)<-c("Smap","cause","index","effect")
for_all_P_pol<-cbind(for_all_P_pol,rep(paste(for_all_P_pol$effect,"-",for_all_P_pol$cause)))
colnames(for_all_P_pol)<-c("Smap","cause","index","effect","effect_cause")


for_all_S_mul<-cbind(dataframe_Smap_S_mul,rep("S_multipunctatus",nrow(dataframe_Smap_S_mul)))
colnames(for_all_S_mul)<-c("Smap","cause","index","effect")
for_all_S_mul<-cbind(for_all_S_mul,rep(paste(for_all_S_mul$effect,"-",for_all_S_mul$cause)))
colnames(for_all_S_mul)<-c("Smap","cause","index","effect","effect_cause")


for_all_L_lem<-cbind(dataframe_Smap_L_lem,rep("L_lemairii",nrow(dataframe_Smap_L_lem)))
colnames(for_all_L_lem)<-c("Smap","cause","index","effect")
for_all_L_lem<-cbind(for_all_L_lem,rep(paste(for_all_L_lem$effect,"-",for_all_L_lem$cause)))
colnames(for_all_L_lem)<-c("Smap","cause","index","effect","effect_cause")

for_all_L_lab<-cbind(dataframe_Smap_L_lab,rep("L_labiatus",nrow(dataframe_Smap_L_lab)))
colnames(for_all_L_lab)<-c("Smap","cause","index","effect")
for_all_L_lab<-cbind(for_all_L_lab,rep(paste(for_all_L_lab$effect,"-",for_all_L_lab$cause)))
colnames(for_all_L_lab)<-c("Smap","cause","index","effect","effect_cause")

for_all_L_lem<-cbind(dataframe_Smap_L_lem,rep("L_lemairii",nrow(dataframe_Smap_L_lem)))
colnames(for_all_L_lem)<-c("Smap","cause","index","effect")
for_all_L_lem<-cbind(for_all_L_lem,rep(paste(for_all_L_lem$effect,"-",for_all_L_lem$cause)))
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
  ggtitle("(a)Aul. dew")+
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
           x = 10, y = c(max(dataframe_Smap_A_dew$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_A_dew$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_A_dew$Smap,na.rm=T),max(dataframe_Smap_A_dew$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20
  

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
  ggtitle("(c)Jul. orn")+
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
           x = 10, y = c(max(dataframe_Smap_J_orn$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_J_orn$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_J_orn$Smap,na.rm=T),max(dataframe_Smap_J_orn$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(k)Neo. fas")+
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
           x = 10, y = c(max(dataframe_Smap_N_fas$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_N_fas$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_N_fas$Smap,na.rm=T),max(dataframe_Smap_N_fas$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20


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
  #geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("(q)Par. spp")+
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
           x = 10, y = c(max(dataframe_Smap_Par$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_Par$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_Par$Smap,na.rm=T),max(dataframe_Smap_Par$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(s)Tro. moo")+
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
           x = 10, y = c(max(dataframe_Smap_T_moo$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_T_moo$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_T_moo$Smap,na.rm=T),max(dataframe_Smap_T_moo$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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

Smap_I_loo <- ggplot(data = dataframe_Smap_I_loo, aes(x = index, y = Smap, group = cause, color = cause)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  xlab("Year") + 
  ggtitle("(b)Int. loo") + 
  theme(plot.title = element_text(size = rel(2), colour = "black")) + 
  theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30)) + 
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20)) + 
  scale_x_continuous(breaks = seq(1, 40, 1), labels = yearslabel) + 
  theme_classic() + 
  theme(plot.title = element_text(face = "italic"), text = element_text(family = "Times")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) + 
  theme(legend.text = element_text(family = "Times", face = "italic"), 
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  annotate(geom = "text", 
           x = 10, 
           y = c(max(dataframe_Smap_I_loo$Smap, na.rm = T) + 0.05), 
           color = "black", 
           size = 4, 
           label = "East", 
           family = "Times") + 
  annotate(geom = "text", 
           x = 30, 
           y = c(max(dataframe_Smap_I_loo$Smap, na.rm = T) + 0.05), 
           color = "black", 
           size = 4, 
           label = "West", 
           family = "Times") + 
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1) 

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
  ggtitle("(e)Lam. cal")+
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
           x = 10, y = c(max(dataframe_Smap_L_cal$Smap,na.rm=T)+0.2),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_L_cal$Smap,na.rm=T)+0.2),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_cal$Smap,na.rm=T),max(dataframe_Smap_L_cal$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(g)Lep. elo")+
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
           x = 10, y = c(max(dataframe_Smap_L_elo$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_L_elo$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_elo$Smap,na.rm=T),max(dataframe_Smap_L_elo$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

ggsave(file = "Smap_L_elo.pdf", plot = Smap_L_elo, dpi = 100, width = 10, height = 5)


#L_lemairii(eff)-A_dewindti(cau)


dataframe_Smap_L_lem$cause<-gsub(unique(dataframe_Smap_L_lem$cause),sp_food_coltp0[unique(dataframe_Smap_L_lem$cause)==sp_food_coltp0$cause,]$re.namesp,dataframe_Smap_L_lem$cause)

Smap_L_lem<-ggplot(data=dataframe_Smap_L_lem, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("(i)Lam. lem")+
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
           x = 10, y = c(max(dataframe_Smap_L_lem$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_L_lem$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_lem$Smap,na.rm=T),max(dataframe_Smap_L_lem$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(l)Neo. sex")+
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
           x = 10, y = c(max(dataframe_Smap_N_sex$Smap,na.rm=T)+0.1),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_N_sex$Smap,na.rm=T)+0.1),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_N_sex$Smap,na.rm=T),max(dataframe_Smap_N_sex$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(m)Oph. ven")+
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
           x = 10, y = c(max(dataframe_Smap_O_ven$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_O_ven$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_O_ven$Smap,na.rm=T),max(dataframe_Smap_O_ven$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(n)Per. mic")+
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
           x = 10, y = c(max(dataframe_Smap_P_mic$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_P_mic$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_P_mic$Smap,na.rm=T),max(dataframe_Smap_P_mic$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(p)Pet. tre")+
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
           x = 10, y = c(max(dataframe_Smap_P_tre$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_P_tre$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_P_tre$Smap,na.rm=T),max(dataframe_Smap_P_tre$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(t)Tel. vit")+
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
           x = 10, y = c(max(dataframe_Smap_T_vit$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_T_vit$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_T_vit$Smap,na.rm=T),max(dataframe_Smap_T_vit$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(f)Lim. dar")+
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
           x = 10, y = c(max(dataframe_Smap_L_dar$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_L_dar$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_dar$Smap,na.rm=T),max(dataframe_Smap_L_dar$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(j)Lam. tan")+
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
           x = 10, y = c(max(dataframe_Smap_L_tan$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_L_tan$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_tan$Smap,na.rm=T),max(dataframe_Smap_L_tan$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

ggsave(file = "Smap_L_tan.pdf", plot = Smap_L_tan, dpi = 100, width = 10, height = 5)



Lepatt_leg_orders<-NULL
for(i in 1:length(unique(dataframe_Smap_L_atts$cause))){
  Lepatt_leg_order<-c(unique(dataframe_Smap_L_atts$cause)[i],mean(subset(dataframe_Smap_L_atts$Smap,dataframe_Smap_L_atts$cause==unique(dataframe_Smap_L_atts$cause)[i]),na.rm=T))
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

for(i in 1:length(dataframe_Smap_L_atts$cause)){
  dataframe_Smap_L_atts$cause[i]<-sp_food_coltp0[dataframe_Smap_L_atts$cause[i]==sp_food_coltp0$cause,]$re.namesp
}

dataframe_Smap_L_atts$cause<-factor(dataframe_Smap_L_atts$cause,levels=c(Lepatt_leg_orders$cause))

Smap_L_att<-ggplot(data=dataframe_Smap_L_atts, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("(d)Lep .att")+
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
           x = 10, y = c(max(dataframe_Smap_L_atts$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_L_atts$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_atts$Smap,na.rm=T),max(dataframe_Smap_L_atts$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

ggsave(file = "Smap_L_att.pdf", plot = Smap_L_att, dpi = 100, width = 10, height = 5)


#--L_labiatus(eff)対L_att(cas)
dataframe_Smap_L_lab$cause<-gsub(unique(dataframe_Smap_L_lab$cause),sp_food_coltp0[unique(dataframe_Smap_L_lab$cause)==sp_food_coltp0$cause,]$re.namesp,dataframe_Smap_L_lab$cause)

Smap_L_lab<-ggplot(data=dataframe_Smap_L_lab, aes(x = index, y = Smap,  group = cause, color = cause))+
  geom_line()+
  geom_hline(yintercept=0)+
  xlab("Year")+
  ggtitle("(h)Lob.lab")+
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
           x = 10, y = c(max(dataframe_Smap_L_lab$Smap,na.rm=T)+1),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_L_lab$Smap,na.rm=T)+1),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_L_lab$Smap,na.rm=T),max(dataframe_Smap_L_lab$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(v)Xen. pap")+
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
           x = 10, y = c(max(dataframe_Smap_X_pap$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_X_pap$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_X_pap$Smap,na.rm=T),max(dataframe_Smap_X_pap$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(u)Var. moo")+
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
           x = 10, y = c(max(dataframe_Smap_V_moo$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_V_moo$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_V_moo$Smap,na.rm=T),max(dataframe_Smap_V_moo$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(o)Pet. pol")+
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
           x = 10, y = c(max(dataframe_Smap_P_pol$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_P_pol$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_P_pol$Smap,na.rm=T),max(dataframe_Smap_P_pol$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

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
  ggtitle("(r)Syn. mul")+
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
           x = 10, y = c(max(dataframe_Smap_S_mul$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "East",
           family="Times")+
  annotate(geom = "text",
           x = 30, y = c(max(dataframe_Smap_S_mul$Smap,na.rm=T)+0.05),  # テキストの中心座標位置
           color = "black",
           size = 4,
           label = "West",
           family="Times")+
  coord_cartesian(xlim = c(1,length(yearslabel)),
                  ylim = c(min(dataframe_Smap_S_mul$Smap,na.rm=T),max(dataframe_Smap_S_mul$Smap,na.rm=T)),
                  clip = "off")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1)  # Vertical line at x = 20

ggsave(file = "Smap_S_mul.pdf", plot = Smap_S_mul, dpi = 100, width = 10, height = 5)


FigS2a<-grid.arrange(Smap_A_dew,
                      Smap_I_loo,
                      Smap_J_orn,
                      Smap_L_att,
                      Smap_L_cal,
                      Smap_L_dar,
                     ncol=2)

FigS2b<-grid.arrange(Smap_L_elo,
                      Smap_L_lab,
                      Smap_L_lem,
                      Smap_L_tan,
                      Smap_N_fas,
                      Smap_N_sex,
                     ncol=2)

FigS2c<-grid.arrange(Smap_O_ven,
                      Smap_P_mic,
                      Smap_P_pol,
                      Smap_P_tre,
                      Smap_Par,
                      Smap_S_mul,
                      ncol=2)

FigS2d<-grid.arrange(Smap_T_moo,
                      Smap_T_vit,
                      Smap_V_moo,
                      Smap_X_pap,
                      ncol=2)

ggsave('FigS2a.pdf',FigS2a,height = 8, width = 15)
ggsave('FigS2b.pdf',FigS2b,height = 8, width = 15)
ggsave('FigS2c.pdf',FigS2c,height = 8, width = 15)
ggsave('FigS2d.pdf',FigS2d,height = 8, width = 15)


#west,east記述　縦は線追加すること
