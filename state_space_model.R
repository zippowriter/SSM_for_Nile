#データ
Nile
#サンプルサイズ
length(Nile)
kfLocalLevel <- function(y, mu_pre, P_pre, sigma_w, sigma_v) {
## Step1 prediction
  mu_forecast <- mu_pre
  P_forecast <- P_pre + sigma_w
  y_forecast <- mu_forecast
  F <- P_forecast + sigma_v
  
## Step2 filtering
  K <- P_forecast / (P_forecast + sigma_v)
  y_resid <- y - y_forecast
  mu_filter <- mu_forecast + K * y_resid
  P_filter <- (1 - K) * P_forecast
  # storage of results
  result <- data.frame(
    mu_filter = mu_filter,
    P_filter = P_filter,
    y_resid = y_resid,
    F = F,
    K = K
  )
  return(result)
}

# sample size
N <- length(Nile)
# state estimate
mu_filter <- numeric(N)
# the initial value of the "state" is 0
mu_zero <- 0
mu_filter <- c(mu_zero, mu_filter)
# Variance of state prediction error
P_filter <- numeric(N)
# Set the initial value of "variance of state prediction error" to 10000000
P_zero <- 10000000
P_filter <- c(P_zero, P_filter)
# Observation error
y_resid <- numeric(N)
# Variance of prediction error of observed values
F <- numeric(N)
# Kalman gain
K <- numeric(N)
# Variance of process error
sigma_w <- 1000
# Variance of observation error
sigma_v <- 10000

for (i in 1:N) {
  kekka <- kfLocalLevel(
    y = Nile[i], mu_pre = mu_filter[i], P_pre = P_filter[i], 
    sigma_w = sigma_w, sigma_v = sigma_v
  )
  mu_filter[i + 1] <- kekka$mu_filter
  P_filter[i + 1] <- kekka$P_filter
  y_resid[i] <- kekka$y_resid
  F[i] <- kekka$F
  K[i] <- kekka$K
}
print(mu_filter)

sum(log(dnorm(y_resid, mean = 0, sd = sqrt(F))))
1/2 * sum(log(F) + y_resid^2 / F)

calkLogLik <- function(sigma){
  sigma_w <- exp(sigma[1])
  sigma_v <- exp(sigma[2])
  # variable definition exc
  N <- length(Nile)                 ; mu_filter <- numeric(N)
  mu_zero <- 0                      ; mu_filter <- c(mu_zero, mu_filter)
  P_filter <- numeric(N)            ; P_zero <- 10000000
  P_filter <- c(P_zero, P_filter)   ; y_resid <- numeric(N)
  F <- numeric(N)                   ; K <- numeric(N)
  # running of Kalman filter
  for(i in 1:N) {
    kekka <- kfLocalLevel(
      y = Nile[i], mu_pre = mu_filter[i], P_pre = P_filter[i],
      sigma_w = sigma_w, sigma_v = sigma_v
    )
    mu_filter[i + 1] <- kekka$mu_filter
    P_filter[i + 1] <- kekka$P_filter
    y_resid[i] <- kekka$y_resid
    F[i] <- kekka$F
    K[i] <- kekka$K
  }
  return(1/2 * sum(log(F) + y_resid^2 / F))
}

best_sigma <- optim(calkLogLik, par = c(1, 1), method = "L-BFGS")

exp(best_sigma$par)

smoothLocalLevel <- function(mu_filterd, P_filterd, r_post, s_post,
                            F_post, y_resid_post, K_post) {
  # 状態平滑化漸化式
  r <- y_resid_post/F_post + (1 - K_post) * r_post
  mu_smooth <- mu_filterd + P_filterd * r
  # 状態分散平滑化漸化式
  s <- 1/F_post + (1 - K_post)^2 * s_post
  P_smooth <- P_filterd - P_filterd^2 * s
  # 結果の格納
  result <- data.frame(
    mu_smooth = mu_smooth,
    P_smooth = P_smooth,
    r = r,
    s = s
  )
  return(result)
}

# 平滑化状態
mu_smooth <- numeric(N + 1)
# 平滑化状態分散
P_smooth <- numeric(N + 1)
# 漸化式のパラメタ（初期値は０のままで良い）
r <- numeric(N)
s <- numeric(N)
# 最後のデータは、フィルタリングの結果とスムージングの結果が一致する
mu_smooth[N + 1] <- mu_filter[N + 1]
P_smooth[N + 1] <- P_filter[N + 1]

for (i in 1:N) {
  kekka <- smoothLocalLevel(
    mu_filter[i], P_filter[i], r[i], s[i], F[i], y_resid[i], K[i]
  )
  mu_smooth[i] <- kekka$mu_smooth
  P_smooth[i] <- kekka$P_smooth
  r[i - 1] <- kekka$r
  s[i -1] <- kekka$s
}

mu_smooth
