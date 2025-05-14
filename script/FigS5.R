
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

# プロット
plot_data <- data.frame(log_k = log_k, log_P_k = log_P_k)
powera<-ggplot(plot_data, aes(x = log_k, y = log_P_k)) +
  geom_point() +
  labs(x = "", y = "log10(P(k))")+
  geom_smooth(method = "lm", se = FALSE, col = "black",lty=1)+
  annotate("text", x =0.5, y =-1.2, 
           label =paste("γ = ",round(est_xmin_all$pars,digits = 2)) , hjust = 0, vjust = 1, size = 5)+
  ylim(min(plot_data$log_P_k),0)+
  xlim(0,max(plot_data$log_k))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  ggtitle("(a)")


# 結果の表示
print(summary(fit))

# 回帰係数（log_kの係数とp値）を取得
log_k_estimate <- model_summary$coefficients["log_k", "Estimate"]
log_k_p_value <- model_summary$coefficients["log_k", "Pr(>|t|)"]

# 結果を表示
cat(sprintf("log_kの係数（γ）: %.4f\n", log_k_estimate))
cat(sprintf("log_kのp値: %.4f\n", log_k_p_value))

# γ（べき指数）として係数の符号を反転させる
gamma <- -log_k_estimate

cat(sprintf("べき指数 γ: %.2f\n", gamma))

#ただし最小二乗法を用いた場合傾きは性も負もあり得るためその傾きの絶対値を取りマイナスをかけた値が冪乗法則のλに当たる
#大きな𝜆の値は、ネットワーク内のハブノードがさらに希少化し、ノードの次数のばらつきがより大きくなることを示唆します。つまり、ごく少数のノードが非常に多くのリンクを持ち、残りのノードはそれほど多くのリンクを持たない、より不均衡なネットワーク構造が予想されます。
#また、𝜆の値が大きい場合、ネットワーク内での情報や影響の伝播は、ごく少数の中心的なノードによって支配される可能性が高くなります。そのため、ネットワークのロバスト性や耐障害性が低下し、ハブノードの故障や攻撃に対する脆弱性が増すかもしれません。




#原因側の因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_out_digdis <- igraph::degree(igraphdatatp0ww8,mode = "out")
write.csv(degree_dist_out_digdis,"degree_dist_out_digdis.csv")

# 次数の頻度分布を計算
degree_freq <- table(degree_dist_out_digdis)
N <- sum(degree_freq)
# degree_dist_in_digdis (度数) は table の名前部分
degree_dist_out_digdis <- as.numeric(names(degree_freq))
k <- as.numeric(names(degree_freq))

lambda <- sum(degree_dist_out_digdis * k) / N
expected_counts <- N * dpois(degree_dist_out_digdis, lambda)
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



# barabashiデータのプロット
plot(m_out, 
     main="Degree distribution with power-law fit",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=0.5, 
     col="darkgray", 
     pch=16)

# パワーローのフィットを重ねる
lines(m_out, col="red", lwd=2)



# プロット
plot_data <- data.frame(log_k = log_k, log_P_k = log_P_k)
powerb<-ggplot(plot_data, aes(x = log_k, y = log_P_k)) +
  geom_point() +
  labs(x = "log10(k)", y = "")+
  geom_smooth(method = "lm", se = FALSE, col = "black",lty=1)+
  annotate("text", x =0, y =-0.3, 
           label =paste("γ = ",round(est_xmin_out$pars,digits = 2)) , hjust = 0, vjust = 1, size = 5)+
  ylim(min(plot_data$log_P_k),0)+
  xlim(0,max(plot_data$log_k))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  ggtitle("(b)")



# 結果の表示
print(summary(fit))

log_k_estimate <- model_summary$coefficients["log_k", "Estimate"]
log_k_p_value <- model_summary$coefficients["log_k", "Pr(>|t|)"]

# 結果を表示
cat(sprintf("log_kの係数（γ）: %.4f\n", log_k_estimate))
cat(sprintf("log_kのp値: %.4f\n", log_k_p_value))

# γ（べき指数）として係数の符号を反転させる
gamma <- -log_k_estimate

cat(sprintf("べき指数 γ: %.2f\n", gamma))





#結果側の因果関係についてのものを採用(sup)
# ノードの次数を取得
degree_dist_in_digdis <- igraph::degree(igraphdatatp0ww8,mode = "in")
write.csv(degree_dist_in_digdis,"degree_dist_in_digdis.csv")
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


# barabashiデータのプロット
plot(m_in, 
     main="Degree distribution with power-law fit",
     xlab="Degree (k)", 
     ylab="P(X ≥ k)", 
     cex=0.5, 
     col="darkgray", 
     pch=16)

# パワーローのフィットを重ねる
lines(m_in, col="red", lwd=2)


plot_data <- data.frame(log_k = log_k, log_P_k = log_P_k)
powerc<-ggplot(plot_data, aes(x = log_k, y = log_P_k)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black",lty=1) +
  labs(x = "log10(k)", y = "log10(P(k))")+
  annotate("text", x =0.1, y =-1.2, 
           label =paste("γ = ",round(est_xmin_in$pars,digits = 2)) , hjust = 0, vjust = 1, size = 5)+
  ylim(min(plot_data$log_P_k),0)+
  xlim(0,max(plot_data$log_k))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  ggtitle("(c)")



# 結果の表示
print(summary(fit))

log_k_estimate <- model_summary$coefficients["log_k", "Estimate"]
log_k_p_value <- model_summary$coefficients["log_k", "Pr(>|t|)"]

# 結果を表示
cat(sprintf("log_kの係数（γ）: %.4f\n", log_k_estimate))
cat(sprintf("log_kのp値: %.4f\n", log_k_p_value))

# γ（べき指数）として係数の符号を反転させる
gamma <- -log_k_estimate

cat(sprintf("べき指数 γ: %.2f\n", gamma))

top_plot<-plot_grid(powera,ncol=1)
bottom_plot<-plot_grid(powerb,powerc,ncol=2)
combined_plot<-plot_grid(top_plot,bottom_plot,ncol=1,rel_heights = c(1,1))
ggsave("FigS5.pdf",combined_plot,width = 8.27, height = 5.69, units = "in")

