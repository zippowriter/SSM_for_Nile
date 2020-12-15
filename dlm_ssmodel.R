library(dlm)
library(ggplot2)
library(ggfortify)
# Step1 モデルの構造を決める
build_local_level_dlm <- function(theta){
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))
}
# Step2 パラメタ推定
par_local_level_dlm <- dlmMLE(Nile, parm=c(1, 1), build_local_level_dlm)
# 推定された分散を使って、モデルを組み直す
fit_local_level_dlm <- build_local_level_dlm(par_local_level_dlm$par)
# Step3 フィルタリング
filter_local_level_dlm <-dlmFilter(Nile, fit_local_level_dlm)
# Step4 スムージング
smooth_local_level_dlm <- dlmSmooth(filter_local_level_dlm)

# フィルタ化推定量の図示
autoplot(filter_local_level_dlm, fitted.colour = "black",
         fitted.size = 1.5, main = "filtered estimator")
# 平滑化状態の図示
p_nile <- autoplot(Nile)
autoplot(smooth_local_level_dlm, fitted.colour = "black",
         colour = "black", size = 1.5, main="smoothed state", p=p_nile)

# 散漫カルマンフィルタの実装
# 状態の推定値
mu_diffuse_filter <- numeric(N + 1)
# 状態の予測誤差の分散
P_diffuse_filter <- numeric(N + 1)
# 散漫初期化を用いると、1時点目のフィルタ化推定量は以下のようになる
mu_diffuse_filter[2] <- Nile[1]
P_diffuse_filter[2] <- sigma_v
# 観測値の予測誤差
y_resid_diffuse <- numeric(N)
# 観測値の予測誤差の分散
F_diffuse <- numeric(N)
# カルマンゲイン
K_diffuse <- numeric(N)

for (i in 2:N) {
  kekka <- kfLocalLevel(
    y = Nile[i], mu_pre = mu_diffuse_filter[i],
    P_pre = P_diffuse_filter[i], sigma_w = sigma_w, sigma_v = sigma_v
  )
  mu_diffuse_filter[i + 1] <- kekka$mu_filter
  P_diffuse_filter[i + 1] <- kekka$P_filter
  y_resid_diffuse[i] <- kekka$y_resid
  F_diffuse[i] <- kekka$F
  K_diffuse[i] <- kekka$K
}
mu_diffuse_filter
mu_filter

# 散漫対数尤度の実装
# dnorm 関数を使った対数尤度の計算
sum(
  log(
    dnorm(y_resid_diffuse[-1], mean = 0, sd = sqrt(F_diffuse[-1]))
  )
)
