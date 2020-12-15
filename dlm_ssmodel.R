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
