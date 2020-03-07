####################
# カルマンフィルタ #
####################
# 外生変数の時変係数モデル(SSMregression)のみ

# Set Working Directory by yourself

library(KFAS)
library(xts)
library(ggplot2)

# オリジナルのデータは1年分あり。サンプルは1日分のみ公開。
data <- read.csv("sample_data.csv")

# 12時だけを使う
train <- data[ as.POSIXlt(data$validtime)$hour==12 & as.POSIXlt(data$validtime)$min==0, ]

#-日射量、気温で時変係数モデルを組む
# step1 : モデルの構造を決める
model <- SSModel(
  H = NA,
  power_sum ~
    SSMregression( ~ DSWRF_SFC + TMP_SFC, Q = diag(rep(NA, 2)) ),
  data = train
)

# step2 : パラメタ推定（2度行った方が良かった）
model.tmp <- fitSSM(model, inits = rep(1, 3))
model.fit <- fitSSM(model, inits = model.tmp$optim.out$par)

# step3,4 : フィルタリング、スムージング
model.result <- KFS(
  model.fit$model,
  filtering = c("state", "mean"),
  smoothing = c("state", "mean")
)

# 日射量の回帰係数（ここではフィルタ推定量）をグラフ化
df_filter <- data.frame(
  validtime = 1:365,
  coef_dswrf = model.result$a[-1,"DSWRF_SFC"]
)

ggplot(data = df_filter, aes(x = validtime, y = coef_dswrf)) +
  labs(title="Estimated Coefficient of DSWRF_SFC") +
  geom_line(aes(y = coef_dswrf), size=1.2) +
  ylab(label = "") +
  xlab(label = "Time Step")
