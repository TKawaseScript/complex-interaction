library(cowplot)
library(tidyr)
library(dplyr)
library(ggplot2)

load("spsmap.Rdata")
sp_food_coltp0<-read.csv("spcollisttp=0.csv",header=T)

#リストのデータ名の変更
for(i in 1:length(spsmap)){
  rename<-NULL
  for(j in 1:ncol(spsmap[[i]])-1){
    rename<-c(rename,sp_food_coltp0[colnames(spsmap[[i]])[j]==sp_food_coltp0$cause,]$bindre.namesp)
    }
  colnames(spsmap[[i]])[1:length(spsmap[[i]])-1]<-rename
}

listname<-NULL
for(i in 1:length(names(spsmap))){
  listname<-c(listname,sp_food_coltp0[sp_food_coltp0$cause==names(spsmap)[i],]$bindre.namesp)
}
names(spsmap)<-listname


# プロットを格納するための空のリストを作成
plots <- list()

# spsmapリストの各データフレームに対してループ処理
for (i in seq_along(spsmap)) {
  # リストから各データフレームを抽出
  data <- spsmap[[i]]
  
  # x軸の値として使用するためにObservation列を追加
  data <- data %>% mutate(Observation = row_number())
  
  # データをワイド形式からロング形式に変換
  tidy_data <- pivot_longer(data, cols = -c(b, Observation), names_to = "Species", values_to = "Value")
  
  # 各Speciesの折れ線グラフを作成
  p <- ggplot(tidy_data, aes(x = Observation, y = Value, color = Species, group = Species)) +
    geom_line() +
    labs(title=paste("(",letters[i],")",colnames(spsmap[[i]][1]),seq=""),
         x = "",  # x軸ラベル
         y = "") +      # y軸ラベル
    theme(plot.title =element_text(face="italic"))+
    theme_minimal()+
    geom_vline(xintercept=20,color="black",linetype="dashed")+
    annotate("text",label="East",x=2,y=max(spsmap[[i]],na.rm=T)+1)+
    annotate("text",label="West",x=22,y=max(spsmap[[i]],na.rm=T)+1)
  
  # 作成したプロットをリストに格納
  plots[[i]] <- p
}

for(i in seq(1, length(plots), by=4)){
  p <- plot_grid(plotlist = plots[i:(i+9)], ncol = 2, nrow = 2)
  
  # ファイルに保存
  ggsave(filename = paste0("FigS2_", i, ".pdf"), plot = p, width = 11, height = 8.5)
}
