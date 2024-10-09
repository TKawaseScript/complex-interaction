source("igraphplot2.R")

environment(plot.igraph2) <- asNamespace('igraph')
environment(igraph.Arrows2) <- asNamespace('igraph')



plot(igraphdatatp0ww8,layout=lay.crctp0ww8,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     #Smap_cum_sumの中身sumしてabsに変更
     edge.width=log(igraph_allww8$Smap_cum_sum),
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


plot.igraph2(igraphdatatp0ww8,layout=lay.crctp0ww8,
     vertex.frame.color="black",
     edge.curved=0.3,
     vertex.label.cex=0.7,
     vertex.label.family="Times",
     vertex.label.font=4,
     #Smap_cum_sumの中身sumしてabsに変更
     edge.width=log(igraph_allww8$Smap_cum_sum),
     edge.arrow.width = log(igraph_allww8$Smap_cum_sum)*1/2,
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




plot.igraph2(g2,edge.arrow.size=E(g2)$weight/max(E(g2)$weight)/2,edge.arrow.width=E(g2)$weight/max(E(g2)$weight))
