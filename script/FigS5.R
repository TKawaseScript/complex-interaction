library(caret)
library(tools)
library(dplyr)
library(igraph)
library(tidyverse)
library(plyr)
library(ggrepel)
library(tidygraph)
library(ape)
library(nlme)
library(cowplot)
library(poweRlaw)
library(renv)

renv::restore()

# データの読み込みと整形
sp_food_coltp0 <- read.csv("spcollisttp=0.csv", header = TRUE, fileEncoding = "UTF-8")
strengthtp0 <- read.csv("GLMbasedatatp0.csv", header = TRUE)[, -c(1, 9:11)]

colnames(strengthtp0) <- c("cause", "effect", "causepopmean", "effectpopmean", "causepopsd",
                           "effectpopsd", "causehabitat", "effecthabitat", "smapmin", "smapX1st",
                           "smapmedian", "smapmean", "smapX3rd", "smapmax", "strength", "ratio")

intratp0 <- NULL
intertp0 <- NULL

for (i in 1:nrow(strengthtp0)) {
  if (strengthtp0$cause.x[i] == strengthtp0$effect.x[i]) {
    intratp0 <- rbind(intratp0, strengthtp0[i, ])
  } else {
    intertp0 <- rbind(intertp0, strengthtp0[i, ])
  }
}

# 原因側と結果側のまとめ
countcausetp0 <- t(table(intertp0$cause))
counteffecttp0 <- t(table(intertp0$effect))

forGray <- list()
for (i in 1:nrow(intertp0)) {
  if (intertp0$strength[i] == "positive") {
    forGray[[i]] <- c(0, 1)
  } else {
    forGray[[i]] <- c(1, 0)
  }
}

forGray_data <- do.call(rbind, forGray)
colnames(forGray_data) <- c("Neg", "Pos")

igraph_allww8s <- cbind(intertp0, forGray_data)
igraphdatatp0ww8 <- graph(t(cbind(igraph_allww8s$cause.x, igraph_allww8s$effect.x)))

# 全体の次数分布
degree_dist_All_all_digdis <- igraph::degree(igraphdatatp0ww8, mode = "all")
degree_freq_all <- as.numeric(degree_dist_All_all_digdis)
m_all <- displ$new(degree_freq_all)
est_xmin_all <- estimate_xmin(m_all)
m_all$setXmin(est_xmin_all)
est_pars_all <- estimate_pars(m_all)
m_all$setPars(est_pars_all)
set.seed(123)
bootstrap_all <- bootstrap_p(m_all, no_of_sims = 5000, threads = 2)


# 原因側（出次数）
degree_dist_out_digdis <- igraph::degree(igraphdatatp0ww8, mode = "out")
degree_freq_out <- as.numeric(degree_dist_out_digdis)
m_out <- displ$new(degree_freq_out)
est_xmin_out <- estimate_xmin(m_out)
m_out$setXmin(est_xmin_out)
est_pars_out <- estimate_pars(m_out)
m_out$setPars(est_pars_out)
set.seed(123)
bootstrap_out <- bootstrap_p(m_out, no_of_sims = 5000, threads = 2)

# 結果側（入次数）
degree_dist_in_digdis <- igraph::degree(igraphdatatp0ww8, mode = "in")
degree_freq_in <- as.numeric(degree_dist_in_digdis)
degree_freq_in <- degree_freq_in[degree_freq_in != 0]
m_in <- displ$new(degree_freq_in)
est_xmin_in <- estimate_xmin(m_in)
m_in$setXmin(est_xmin_in)
est_pars_in <- estimate_pars(m_in)
m_in$setPars(est_pars_in)
set.seed(123)
bootstrap_in <- bootstrap_p(m_in, no_of_sims = 5000, threads = 2)

xmin_all <- m_all$getXmin()
alpha_all <- m_all$pars
x_vals_all <- seq(xmin_all, max(m_all$dat), length.out = 100)
y_vals_all <- (x_vals_all / xmin_all)^(-alpha_all + 1)
y_vals_all <- y_vals_all / sum(y_vals_all)
ccdf_vals_all <- 1 - cumsum(y_vals_all)

xmin_out <- m_out$getXmin()
alpha_out <- m_out$pars
x_vals_out <- seq(xmin_out, max(m_out$dat), length.out = 100)
y_vals_out <- (x_vals_out / xmin_out)^(-alpha_out + 1)
y_vals_out <- y_vals_out / sum(y_vals_out)
ccdf_vals_out <- 1 - cumsum(y_vals_out)

xmin_in <- m_in$getXmin()
alpha_in <- m_in$pars
x_vals_in <- seq(xmin_in, max(m_in$dat), length.out = 100)
y_vals_in <- (x_vals_in / xmin_in)^(-alpha_in + 1)
y_vals_in <- y_vals_in / sum(y_vals_in)
ccdf_vals_in <- 1 - cumsum(y_vals_in)


# 理論的なCCDFを計算する関数
get_theoretical_ccdf <- function(model) {
  xmin <- model$getXmin()
  alpha <- model$pars
  x_vals <- seq(xmin, max(model$dat), length.out = 100)
  y_vals <- (x_vals / xmin)^(-alpha + 1)
  y_vals <- y_vals / sum(y_vals)
  ccdf_vals <- 1 - cumsum(y_vals)
  return(list(x = x_vals, ccdf = ccdf_vals))
}

# 各モデルに対してCCDFを計算
ccdf_all <- get_theoretical_ccdf(m_all)
ccdf_out <- get_theoretical_ccdf(m_out)
ccdf_in <- get_theoretical_ccdf(m_in)


# PDF出力
pdf("FigS5_combined.pdf", width = 8, height = 6)

layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))
par(mar = c(4, 4, 2, 2), oma = c(2, 2, 2, 2)) 

# (a) All
plot(m_all,
     main = "",
     xlab = "",
     ylab = "P(k)",
     cex = 1,
     col = "black",
     pch = 16
)
lines(ccdf_all$x, ccdf_all$ccdf, col = "red", lwd = 2)
mtext("(a)", side = 3, adj = 0, line = 1.5, cex = 1.2)

text(x = 3, y = 0.15, labels = paste("γ =", round(m_all$pars, 2)), cex = 1.1)
text(x = 3, y = 0.1, labels = paste("p =", round(bootstrap_all$p, 2)), cex = 1.1)


# (b) Out
plot(m_out,
     main = "",
     xlab = "k",
     ylab = "P(k)",
     cex = 1,
     col = "black",
     pch = 16
)
lines(ccdf_out$x, ccdf_out$ccdf, col = "red", lwd = 2)
mtext("(b)", side = 3, adj = 0, line = 1.5, cex = 1.2)
text(x = 3, y = 0.15, labels = paste("γ =", round(m_out$pars, 2)), cex = 1.1)
text(x = 3, y = 0.1, labels = paste("p =", round(bootstrap_out$p, 2)), cex = 1.1)


library(poweRlaw)

# 入次数データ（0を除外）
degree_freq_in <- degree_freq_in[degree_freq_in > 0]

# 通常のべき乗則モデル（連続）
m_pl <- conpl$new(degree_freq_in)
m_pl$setXmin(estimate_xmin(m_pl))
m_pl$setPars(estimate_pars(m_pl))

# 「Truncated Power-law」として比較するには、別の連続分布を使う
# ここでは対数正規分布（log-normal）を代替として比較する例
m_ln <- conlnorm$new(degree_freq_in)
m_ln$setXmin(4)
m_ln$setPars(estimate_pars(m_ln))

# モデル比較
comparison <- compare_distributions(m_pl, m_ln)

# 結果表示
print(comparison)

# (c) In
plot(m_in,
     main = "",
     xlab = "k",
     ylab = "P(k)",
     cex = 1,
     col = "black",
     pch = 16
)
lines(ccdf_in$x, ccdf_in$ccdf, col = "red", lwd = 2)
mtext("(c)", side = 3, adj = 0, line = 1.5, cex = 1.2)
text(x = 3, y = 0.15, labels = paste("γ =", round(m_in$pars, 2)), cex = 1.1)
text(x = 3, y = 0.1, labels = paste("p =", round(bootstrap_in$p, 2)), cex = 1.1)


dev.off()


# データの準備（0を除外）
degree_freq_all <- degree_freq_all[degree_freq_all > 0]

# モデルの作成
m_pl_all <- conpl$new(degree_freq_all)
m_ln_all <- conlnorm$new(degree_freq_all)

# xmall を揃える（Power-law に合わせるのが一般的）
est_xmin_all <- estimate_xmin(m_pl_all)$xmin
m_pl_all$setXmin(est_xmin_all)
m_ln_all$setXmin(est_xmin_all)

# パラメータ推定
m_pl_all$setPars(estimate_pars(m_pl_all))
m_ln_all$setPars(estimate_pars(m_ln_all))

# モデル比較
comparison_all <- compare_distributions(m_pl_all, m_ln_all)

# 結果表示
print(comparison_all)


# データの準備（0を除外）
degree_freq_out <- degree_freq_out[degree_freq_out > 0]

# モデルの作成
m_pl_out <- conpl$new(degree_freq_out)
m_ln_out <- conlnorm$new(degree_freq_out)

# xmout を揃える（Power-law に合わせるのが一般的）
est_xmin_out <- estimate_xmin(m_pl_out)$xmin
m_pl_out$setXmin(est_xmin_out)
m_ln_out$setXmin(est_xmin_out)

# パラメータ推定
m_pl_out$setPars(estimate_pars(m_pl_out))
m_ln_out$setPars(estimate_pars(m_ln_out))

# モデル比較
comparison_out <- compare_distributions(m_pl_out, m_ln_out)

# 結果表示
print(comparison_out)


# データの準備（0を除外）
degree_freq_in <- degree_freq_in[degree_freq_in > 0]

# モデルの作成
m_pl_in <- conpl$new(degree_freq_in)
m_ln_in <- conlnorm$new(degree_freq_in)

# xmin を揃える（Power-law に合わせるのが一般的）
est_xmin_in <- estimate_xmin(m_pl_in)$xmin
m_pl_in$setXmin(est_xmin_in)
m_ln_in$setXmin(est_xmin_in)

# パラメータ推定
m_pl_in$setPars(estimate_pars(m_pl_in))
m_ln_in$setPars(estimate_pars(m_ln_in))

# モデル比較
comparison_in <- compare_distributions(m_pl_in, m_ln_in)

# 結果表示
print(comparison_in)





