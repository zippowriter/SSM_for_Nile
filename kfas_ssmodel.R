library(KFAS)
library(ggplot2)

nile_train <- window(Nile, end = 1950)
nile_train[41:60] <- NA

# Step1:モデルの構造を決める
build_kfas <- SSModel(
  H = NA,
  nile_train ~ SSMtrend(degree = 1, Q = NA)
)
# Step2:パラメタ推定
fit_kfas <- fitSSM(build_kfas, inits = c(1, 1))
# Step3:フィルタリング・スムージング
result_kfas <- KFS(
  fit_kfas$model,
  filtering = c("state", "mean"),
  smoothing = c("state", "mean")
)

# 観測誤差の分散
fit_kfas$model$H
# 過程誤差の分散
fit_kfas$model$Q

# フィルタ化推定量
mu_filter_kfas <- result_kfas$a[-1]
# 平滑化推定状態
mu_smooth_kfas <- result_kfas$alphahat

# 推定結果の図示
df_filter <- data.frame(
  y = as.numeric(Nile[1:80]),
  time = 1871:1950,
  mu_filter = mu_filter_kfas
)
ggplot(data = df_filter, aes(x = time, y = y)) +
  labs(title="filtered estimator") +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = mu_filter), size = 1.2)

# KFASによる状態の推定と信頼・予測区間
smooth_conf <- predict(
  fit_kfas$model, interval = "confidence", level = 0.95
)
head(smooth_conf, n = 3)
smooth_pred <- predict(
  fit_kfas$model, interval = "prediction", level = 0.95
)
head(smooth_pred, n = 3)
forecast_pred <- predict(
  fit_kfas$model, interval = "prediction", level = 0.95, n.ahead = 20
)
estimate_all <- rbind(smooth_pred, forecast_pred)
df_forecast <- cbind(
  data.frame(y = as.numeric(Nile), time = 1871:1970),
  as.data.frame(estimate_all)
)
ggplot(data = df_forecast, aes(x = time, y = y)) +
  labs(title = "smoothed state and future forecast") +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3)
