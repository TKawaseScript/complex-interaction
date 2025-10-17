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
library(reshape2)

renv::restore()

# Read data from input_files and output_files folders
sp_food_coltp0 <- read.csv("spcollisttp=0.csv", header = TRUE, fileEncoding = "UTF-8")
strengthtp0<-read.csv("GLMbasedatatp0.csv",header=T)[,-c(1,10:12)]
colnames(strengthtp0) <- c("cause", "effect", "causepopmean", "effectpopmean", "causepopsd",
                           "effectpopsd", "causehabitat", "effecthabitat", "smapmin", "smapX1st",
                           "smapmedian", "smapmean", "smapX3rd", "smapmax", "strength", "ratio")

intratp0 <- NULL
intertp0 <- NULL
for (i in 1:nrow(strengthtp0)) {
  if (strengthtp0$cause[i] == strengthtp0$effect[i]) {
    intratp0 <- rbind(intratp0, strengthtp0[i, ])
  } else {
    intertp0 <- rbind(intertp0, strengthtp0[i, ])
  }
}

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
igraphdatatp0ww8 <- graph(t(cbind(igraph_allww8s$cause, igraph_allww8s$effect)))

# degree distribution
degree_dist_All_all_digdis <- igraph::degree(igraphdatatp0ww8, mode = "all")
write.csv(degree_dist_All_all_digdis,"degree_dist_All_all_digdis.csv")
degree_freq_all <- as.numeric(degree_dist_All_all_digdis)

#making a dataset for python script
python_csv_all<-melt(table(degree_freq_all))
colnames(python_csv_all)<-c("xvalue","counts")
write.csv(python_csv_all,"python_csv_all.csv")

degree_freq_all <- degree_freq_all[degree_freq_all > 0]  # 0を除外

# 通常のべき乗則モデル（離散）
m_all <- displ$new(degree_freq_all)
est_xmin_all <- estimate_xmin(m_all)
m_all$setXmin(est_xmin_all)
est_pars_all <- estimate_pars(m_all)
m_all$setPars(est_pars_all)
set.seed(123)
bootstrap_all <- bootstrap_p(m_all, no_of_sims = 5000, threads = 2,seed=1)

# 指数カットオフ付きパワーロー（離散版）
m_cutoff_all <- disexp$new(degree_freq_all)
m_cutoff_all$setXmin(m_all$getXmin())
est_pars_cutoff_all <- estimate_pars(m_cutoff_all)
m_cutoff_all$setPars(est_pars_cutoff_all)
cutoff_available <- TRUE
set.seed(123)
bootstrap_m_cutoff_all <- bootstrap_p(m_cutoff_all, no_of_sims = 5000, threads = 2,seed=1)


# 対数正規分布との比較
m_ln_all <- conlnorm$new(degree_freq_all)
m_ln_all$setXmin(m_all$getXmin())
m_ln_all$setPars(estimate_pars(m_ln_all))

# 原因側（出次数）
degree_dist_out_digdis <- igraph::degree(igraphdatatp0ww8, mode = "out")
degree_freq_out <- as.numeric(degree_dist_out_digdis)

#pythonように整形
python_csv_out<-melt(table(degree_freq_out))
colnames(python_csv_out)<-c("xvalue","counts")
write.csv(python_csv_out,"python_csv_out.csv")

degree_freq_out <- degree_freq_out[degree_freq_out > 0]  # 0を除外

# 通常のべき乗則モデル（離散）
m_out <- displ$new(degree_freq_out)
est_xmin_out <- estimate_xmin(m_out)
m_out$setXmin(est_xmin_out)
est_pars_out <- estimate_pars(m_out)
m_out$setPars(est_pars_out)
set.seed(123)
bootstrap_out <- bootstrap_p(m_out, no_of_sims = 5000, threads = 2,seed=1)

# 指数カットオフ付きパワーロー
m_cutoff_out <- disexp$new(degree_freq_out)
m_cutoff_out$setXmin(m_out$getXmin())
est_pars_cutoff_out <- estimate_pars(m_cutoff_out)
m_cutoff_out$setPars(est_pars_cutoff_out)
set.seed(123)
bootstrap_m_cutoff_out <- bootstrap_p(m_cutoff_out, no_of_sims = 5000, threads = 2,seed=1)


# 対数正規分布
m_ln_out <- conlnorm$new(degree_freq_out)
m_ln_out$setXmin(m_out$getXmin())
m_ln_out$setPars(estimate_pars(m_ln_out))

# 結果側（入次数）
degree_dist_in_digdis <- igraph::degree(igraphdatatp0ww8, mode = "in")
degree_freq_in <- as.numeric(degree_dist_in_digdis)

#pythonように整形
python_csv_in<-melt(table(degree_freq_in))
colnames(python_csv_in)<-c("xvalue","counts")
write.csv(python_csv_in,"python_csv_in.csv")

degree_freq_in <- degree_freq_in[degree_freq_in != 0]

# 通常のべき乗則モデル（離散）
m_in <- displ$new(degree_freq_in)
est_xmin_in <- estimate_xmin(m_in)
m_in$setXmin(est_xmin_in)
est_pars_in <- estimate_pars(m_in)
m_in$setPars(est_pars_in)
set.seed(123)
bootstrap_in <- bootstrap_p(m_in, no_of_sims = 5000, threads = 2,seed=1)

# 指数カットオフ付きパワーロー
m_cutoff_in <- disexp$new(degree_freq_in)
m_cutoff_in$setXmin(m_in$getXmin())
est_pars_cutoff_in <- estimate_pars(m_cutoff_in)
m_cutoff_in$setPars(est_pars_cutoff_in)
set.seed(123)
bootstrap_m_cutoff_in <- bootstrap_p(m_cutoff_in, no_of_sims = 5000, threads = 2,seed=1)

# 対数正規分布
m_ln_in <- conlnorm$new(degree_freq_in)
m_ln_in$setXmin(m_in$getXmin())
m_ln_in$setPars(estimate_pars(m_ln_in))

# 理論的なCCDFを計算する関数
get_theoretical_ccdf <- function(model) {
  xmin <- model$getXmin()
  x_vals <- seq(xmin, max(model$dat), length.out = 100)
  
  if (class(model)[1] == "displ") {
    # 通常のパワーロー
    alpha <- model$pars
    y_vals <- (x_vals / xmin)^(-alpha + 1)
    y_vals <- y_vals / sum(y_vals)
    ccdf_vals <- 1 - cumsum(y_vals)
  } else if (class(model)[1] == "disexp" || class(model)[1] == "conexp") {
    # 指数カットオフ付きパワーロー
    alpha <- model$pars[1]
    lambda <- model$pars[2]
    y_vals <- (x_vals / xmin)^(-alpha + 1) * exp(-lambda * (x_vals - xmin))
    y_vals <- y_vals / sum(y_vals)
    ccdf_vals <- 1 - cumsum(y_vals)
  } else if (class(model)[1] == "conlnorm") {
    # 対数正規分布
    mu <- model$pars[1]
    sigma <- model$pars[2]
    ccdf_vals <- 1 - plnorm(x_vals, meanlog = mu, sdlog = sigma)
  }
  
  return(list(x = x_vals, ccdf = ccdf_vals))
}

# 各モデルに対してCCDFを計算
ccdf_all <- get_theoretical_ccdf(m_all)
ccdf_cutoff_all <- get_theoretical_ccdf(m_cutoff_all)
ccdf_ln_all <- get_theoretical_ccdf(m_ln_all)

ccdf_out <- get_theoretical_ccdf(m_out)
ccdf_cutoff_out <- get_theoretical_ccdf(m_cutoff_out)
ccdf_ln_out <- get_theoretical_ccdf(m_ln_out)

ccdf_in <- get_theoretical_ccdf(m_in)
ccdf_cutoff_in <- get_theoretical_ccdf(m_cutoff_in)
ccdf_ln_in <- get_theoretical_ccdf(m_ln_in)

# PDF出力
pdf("FigS5.pdf", width = 6, height = 12)
layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))
par(mar = c(4, 4, 2, 2), oma = c(2, 2, 2, 2))

# (a) All
plot(m_all,
     main = "",
     xlab = "",
     ylab = "P(k)",
     cex = 1,
     col = "black",
     pch = 16)
lines(ccdf_all$x, ccdf_all$ccdf, col = "black", lwd = 2)
#lines(ccdf_cutoff_all$x, ccdf_cutoff_all$ccdf, col = "blue", lwd = 2)
#lines(ccdf_ln_all$x, ccdf_ln_all$ccdf, col = "black", lwd = 2,lty=2)
mtext("(a)", side = 3, adj = 0, line = 1.5, cex = 1.2)
text(x = 3, y = 0.25, labels = paste("γ =", round(m_all$pars, 2)), cex = 1.1)
#text(x = 3, y = 0.2, labels = paste("p =", round(bootstrap_all$p, 2)), cex = 1.1)
text(x = 3, y = 0.15, labels = paste("γ =", round(m_cutoff_all$pars, 2)), cex = 1.1)
#text(x = 3, y = 0.1, labels = paste("p =", round(bootstrap_m_cutoff_all$p, 2)), cex = 1.1)
#legend("bottomleft", legend = c("Power-law", "Log-normal"), 
#      col = "black", lwd = 2, lty=c(1,2), cex = 0.8)

# (b) Out
plot(m_out,
     main = "",
     xlab = "k",
     ylab = "P(k)",
     cex = 1,
     col = "black",
     pch = 16)
lines(ccdf_out$x, ccdf_out$ccdf, col = "black", lwd = 2)
#lines(ccdf_cutoff_out$x, ccdf_cutoff_out$ccdf, col = "blue", lwd = 2)
#lines(ccdf_ln_out$x, ccdf_ln_out$ccdf, col = "black", lwd = 2, lty=2)
mtext("(b)", side = 3, adj = 0, line = 1.5, cex = 1.2)
text(x = 3, y = 0.25, labels = paste("γ =", round(m_out$pars, 2)), cex = 1.1)
text(x = 3, y = 0.2, labels = paste("p =", round(bootstrap_out$p, 2)), cex = 1.1)
#text(x = 3, y = 0.15, labels = paste("γ =", round(m_cutoff_out$pars, 2)), cex = 1.1)
#text(x = 3, y = 0.1, labels = paste("p =", round(bootstrap_m_cutoff_out$p, 2)), cex = 1.1)

# (c) In
plot(m_in,
     main = "",
     xlab = "k",
     ylab = "P(k)",
     cex = 1,
     col = "black",
     pch = 16)
lines(ccdf_in$x, ccdf_in$ccdf, col = "black", lwd = 2)
#lines(ccdf_cutoff_in$x, ccdf_cutoff_in$ccdf, col = "blue", lwd = 2)
#lines(ccdf_ln_in$x, ccdf_ln_in$ccdf, col = "black", lwd = 2, lty=2)
mtext("(c)", side = 3, adj = 0, line = 1.5, cex = 1.2)
text(x = 3, y = 0.25, labels = paste("γ =", round(m_in$pars, 2)), cex = 1.1)
text(x = 3, y = 0.2, labels = paste("p =", round(bootstrap_in$p, 2)), cex = 1.1)
#text(x = 3, y = 0.15, labels = paste("γ =", round(m_cutoff_in$pars, 2)), cex = 1.1)
#text(x = 3, y = 0.1, labels = paste("p =", round(bootstrap_m_cutoff_in$p, 2)), cex = 1.1)

dev.off()

#poweRlawパッケージの利用可能なクラスを見ると、剪断付きパワーロー（truncated power-law）に相当するのは見当たりませんが、指数カットオフ付きパワーロー（exponential cutoff power-law）としてconexp（連続版）やdisexp（離散版）が利用できます。
#conexpとdisexpは指数カットオフ付きパワーロー分布を推定するもので、以下の形式です：

#採用:連続版 (conexp): P(x) ∝ x^(-α) × exp(-λx)
#不採用:離散版 (disexp): P(k) ∝ k^(-α) × exp(-λk) 推定値が全てNA

#これは剪断付きパワーローの一種で、パワーロー減衰に指数的な減衰項が加わったものです。


#PL法を採択した理由のコード
## 入力: 離散データ（例：次数ベクトル）
## k <- your_degree_vector (整数ベクトル)
## 例）k <- c(1,1,2,2,2,3,3,5,6, ...)

k_all<-degree_freq_all

k_out<-degree_freq_out

k_in<-degree_freq_in

## ---- AICc 関数 ----
AICc <- function(logLik, k, n){
  -2*logLik + 2*k + (2*k*(k+1)) / (n - k - 1)
}

## ---- 共通: xmin を PL で推定し固定（モデル比較の一貫性のため）----
estimate_common_xmin <- function(x){
  m <- displ$new(x)
  est <- estimate_xmin(m)
  list(xmin = est$xmin)
}

## ---- PL（離散）フィット ----
fit_pl <- function(x, xmin){
  m <- displ$new(x)
  m$setXmin(xmin)
  est <- estimate_pars(m)
  m$setPars(est)
  xx <- x[x >= xmin]
  alpha <- est$pars
  kseq <- xmin:max(xx)
  p <- kseq^(-alpha)
  p <- p / sum(p)
  idx <- match(xx, kseq)
  ll <- sum(log(p[idx]))  # 手動で log-likelihood を計算
  list(model = m, logLik = ll, k_par = 1)
}


## ---- DLN（離散化対数正規）フィット ----
fit_dlnorm <- function(x, xmin){
  m <- dislnorm$new(x)
  m$setXmin(xmin)
  est <- estimate_pars(m)
  m$setPars(est)
  xx <- x[x >= xmin]
  meanlog <- est$pars[1]
  sdlog   <- est$pars[2]
  kseq <- xmin:max(xx)
  p <- dlnorm(kseq, meanlog, sdlog)
  p <- p / sum(p)
  idx <- match(xx, kseq)
  ll <- sum(log(p[idx]))  # 手動で log-likelihood を計算
  list(model = m, logLik = ll, k_par = 2)
}

## ---- TPL（離散・指数カットオフ付きべき則）フィット（自作MLE）----
tpl_fit <- function(x, xmin, Kcap = NULL){
  xx <- x[x >= xmin]
  n  <- length(xx)
  if(is.null(Kcap)) Kcap <- max(xx) + 100L
  kseq <- xmin:Kcap
  
  loglik_tpl <- function(par){
    alpha  <- 1 + exp(par[1])
    lambda <- exp(par[2])
    w <- kseq^(-alpha) * exp(-lambda * kseq)
    Z <- sum(w)
    p <- w / Z
    idx <- xx - xmin + 1L
    sum(log(p[idx]))
  }
  
  par0 <- c(log(2-1), log(0.1))
  opt  <- optim(par0, fn = function(p) -loglik_tpl(p), method = "Nelder-Mead")
  alpha  <- 1 + exp(opt$par[1])
  lambda <- exp(opt$par[2])
  
  # 最終対数尤度（log-likelihood）
  ll <- -opt$value  # 自作 loglik_tpl に基づく MLE の結果
  
  list(alpha = alpha, lambda = lambda, xmin = xmin,
       logLik = ll, k_par = 2, Kcap = Kcap)
}

## ---- TPL: CDF & KS ----
tpl_pmf <- function(k, alpha, lambda, xmin, Kcap){
  kseq <- xmin:Kcap
  w <- kseq^(-alpha) * exp(-lambda * kseq)
  p <- w / sum(w)
  p[match(k, kseq)]
}

tpl_cdf_full <- function(alpha, lambda, xmin, Kcap){
  kseq <- xmin:Kcap
  w <- kseq^(-alpha) * exp(-lambda * kseq)
  p <- w / sum(w)
  cumsum(p)
}

ks_stat_tpl <- function(x, fit){
  xx <- sort(x[x >= fit$xmin])
  n  <- length(xx)
  kseq <- fit$xmin:fit$Kcap
  cdf_model <- tpl_cdf_full(fit$alpha, fit$lambda, fit$xmin, fit$Kcap)
  
  vals <- sort(unique(xx))
  ecdf_vals <- sapply(vals, function(v) mean(xx <= v))
  model_vals <- cdf_model[match(vals, kseq)]
  max(abs(ecdf_vals - model_vals))
}

## ---- TPL: 乱数生成（inverse CDF）----
rtpl <- function(n, alpha, lambda, xmin, Kcap){
  kseq <- xmin:Kcap
  w <- kseq^(-alpha) * exp(-lambda * kseq)
  p <- w / sum(w)
  cdf <- cumsum(p)
  u <- runif(n)
  idx <- findInterval(u, c(0, cdf))
  kseq[idx]
}

## ---- TPL: KSブートストラップp値（Clauset流）----
gof_tpl_boot <- function(x, fit, B = 200, seed = 1){
  set.seed(seed)
  xx <- x[x >= fit$xmin]
  n  <- length(xx)
  ks0 <- ks_stat_tpl(xx, fit)
  cnt <- 0L
  for(b in seq_len(B)){
    xs <- rtpl(n, fit$alpha, fit$lambda, fit$xmin, fit$Kcap)
    fit_b <- tpl_fit(xs, xmin = fit$xmin, Kcap = fit$Kcap)
    ks_b  <- ks_stat_tpl(xs, fit_b)
    if(ks_b >= ks0) cnt <- cnt + 1L
  }
  pval <- cnt / B
  list(ks = ks0, p = pval)
}

## ---- ワークフロー ----
fit_all_models <- function(k, B = 200, use_common_xmin = TRUE){
  stopifnot(all(k == as.integer(k)))
  xmin <- if(use_common_xmin) estimate_common_xmin(k)$xmin else min(k)
  
  pl <- fit_pl(k, xmin = xmin)
  n_tail <- sum(k >= xmin)
  aicc_pl <- AICc(pl$logLik, k = pl$k_par, n = n_tail)
  gof_pl  <- bootstrap_p(pl$model, no_of_sims = B, threads = 1,seed=1)
  p_pl    <- gof_pl$p
  
  dln <- fit_dlnorm(k, xmin = xmin)
  aicc_dln <- AICc(dln$logLik, k = dln$k_par, n = n_tail)
  gof_dln  <- bootstrap_p(dln$model, no_of_sims = B, threads = 1,seed = 1)
  p_dln    <- gof_dln$p
  
  tpl <- tpl_fit(k, xmin = xmin)
  aicc_tpl <- AICc(tpl$logLik, k = tpl$k_par, n = n_tail)
  gof_t    <- gof_tpl_boot(k, tpl, B = B)
  p_tpl    <- gof_t$p
  
  out <- data.frame(
    model   = c("PL (discrete)", "TPL (discrete)", "DLN (discrete)"),
    logLik  = c(pl$logLik, tpl$logLik, dln$logLik),
    k_param = c(pl$k_par, tpl$k_par, dln$k_par),
    AICc    = c(aicc_pl, aicc_tpl, aicc_dln),
    GOF_p   = c(p_pl, p_tpl, p_dln)
  )
  ord <- order(out$AICc)
  list(
    xmin = xmin, n_tail = n_tail,
    summary = out[ord, ],
    fits = list(PL = pl, TPL = tpl, DLN = dln),
    gof  = list(PL = gof_pl, TPL = gof_t, DLN = gof_dln)
  )
}

## ---- 使い方 ----
## k <- あなたの次数データ（整数ベクトル）
res_all <- fit_all_models(k_all, B = 200)
res_in <- fit_all_models(k_in, B = 200)
res_out <- fit_all_models(k_out, B = 200)

res_all$summary   # AICcとGOFの要約（AICcが最小のモデルが第一候補）PL

res_out$summary # AICcとGOFの要約（AICcが最小のモデルが第一候補）PL

res_in$summary # AICcとGOFの要約（AICcが最小のモデルが第一候補）PL
