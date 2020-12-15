# dlmのパラメタ設定
mod_dlm <- dlmModPoly(
  order = 1, m0 = 0, C0 = 10000000, dW = sigma_w, dV = sigma_v
)

mu_filter_dlm <- dlmFilter(Nile, mod_dlm)

mu_filter_dlm$m
mu_filter
sum((mu_filter_dlm$m[-1] - mu_filter[-1])^2)

# 対数尤度の指標
dlmLL(Nile, mod_dlm)
# 比較
1/2 * sum(log(F) + y_resid^2 / F)

mu_smooth_dlm <- dlmSmooth(mu_filter_dlm)
mu_smooth_dlm$s
mu_smooth
sum((mu_smooth_dlm$s - mu_smooth)^2)

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
         fitted.size = 1.5, main = "フィルタ化推定量")
# 平滑化状態の図示
p_nile <- autoplot(Nile)
autoplot(smooth_local_level_dlm, fitted.colour = "black",
         colour = "black", size = 1.5, main="平滑化状態", p=p_nile)
