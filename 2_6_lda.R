# 体格指数を用いた線形判別分析
library(readxl)
data <- read_excel("data/finalReport.xlsx")
# View(data)

data$改善警告 <- ifelse(data$改善警告 == "警告あり", 1, 0)
data$戸籍上の性別<- ifelse(data$戸籍上の性別 == "男", 1, 0)

mData <- data[data$戸籍上の性別 == 1, ]
fData <- data[data$戸籍上の性別 == 0, ]
exp_data <- data[, -1]
mexp_data <- mData[, -1]
fexp_data <- fData[, -1]

install.packages("MASS")
library(MASS)

# -----線形判別分析-----
lda_model <- lda(改善警告 ~ 体格指数 + 体力指数, data)

#各変数の判別係数
a <- lda_model$scaling
# 定数項
b <- mean(lda_model$means %*% lda_model$scaling)

# 警告あり/なしで色分けした散布図
plot(data$体格指数, data$体力指数, type = "p", 
     col = ifelse(data$改善警告 == 1, "red", "blue"),
     xlab = "体格指数", ylab = "体力指数")


#判別境界線
abline(c(b / a[2], -a[1] / a[2]))

# 各個体の判別得点
y <- a[1] * data[, "体格指数"] + a[2] * data[, "体力指数"] - b

# 判別得点に基づき，警告ありと判定された個体:TRUE，無しと判定された個体:FALSE
# lda_result <- y * mean(y[data$改善警告 == 1]) > 0
lda_result <- y * mean(y[data$改善警告 == 1, ]) > 0
# lda_resultのTRUE:"改善警告あり", FALSE:"改善警告なし"に置き換える
lda_result[(lda_result == TRUE)] <- 1
lda_result[(lda_result == FALSE)] <- 0

# 各個体の判別得点と判別結果
data.frame(y, lda_result)
# 全てのデータ
data.frame(data, y, lda_result)

# 誤判別率
mean(data[, 1] != lda_result)


# -----線形判別分析_男性-----
lda_model <- lda(改善警告 ~ 体格指数 + 体力指数, mData)

#各変数の判別係数
a <- lda_model$scaling
# 定数項
b <- mean(lda_model$means %*% lda_model$scaling)

# 警告あり/なしで色分けした散布図
plot(mData$体格指数, mData$体力指数, type = "p", 
     col = ifelse(mData$改善警告 == 1, "red", "blue"),
     xlab = "体格指数", ylab = "体力指数")


#判別境界線
abline(c(b / a[2], -a[1] / a[2]))

# 各個体の判別得点
y <- a[1] * mData[, "体格指数"] + a[2] * mData[, "体力指数"] - b

# 判別得点に基づき，警告ありと判定された個体:TRUE，無しと判定された個体:FALSE
# lda_result <- y * mean(y[mData$改善警告 == 1]) > 0
lda_result <- y * mean(y[mData$改善警告 == 1, ]) > 0
# lda_resultのTRUE:"改善警告あり", FALSE:"改善警告なし"に置き換える
lda_result[(lda_result == TRUE)] <- 1
lda_result[(lda_result == FALSE)] <- 0

# 各個体の判別得点と判別結果
data.frame(y, lda_result)
# 全てのデータ
data.frame(mData, y, lda_result)

# 誤判別率
mean(mData[, 1] != lda_result)

# 2/28 = 0.07

# -----線形判別分析_女性-----
lda_model <- lda(改善警告 ~ 体格指数 + 体力指数, fData)

#各変数の判別係数
a <- lda_model$scaling
# 定数項
b <- mean(lda_model$means %*% lda_model$scaling)

# 警告あり/なしで色分けした散布図
plot(fData$体格指数, fData$体力指数, type = "p", 
     col = ifelse(fData$改善警告 == 1, "red", "blue"),
     xlab = "体格指数", ylab = "体力指数")


#判別境界線
abline(c(b / a[2], -a[1] / a[2]))

# 各個体の判別得点
y <- a[1] * fData[, "体格指数"] + a[2] * fData[, "体力指数"] - b

# 判別得点に基づき，警告ありと判定された個体:TRUE，無しと判定された個体:FALSE
# lda_result <- y * mean(y[fData$改善警告 == 1]) > 0
lda_result <- y * mean(y[fData$改善警告 == 1, ]) > 0
# lda_resultのTRUE:"改善警告あり", FALSE:"改善警告なし"に置き換える
lda_result[(lda_result == TRUE)] <- 1
lda_result[(lda_result == FALSE)] <- 0

# 各個体の判別得点と判別結果
data.frame(y, lda_result)
# 全てのデータ
data.frame(fData, y, lda_result)

# 誤判別率
mean(fData[, 1] != lda_result)

# 2/22 = 0.09