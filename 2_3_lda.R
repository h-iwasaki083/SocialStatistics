library(readxl)
data <- read_excel("finalReport.xlsx")
View(data)

install.packages("MASS")
library(MASS)

lda_model <- lda(改善警告 ~ `1日平均運動時間` + 体格指数, data)

#各変数の判別係数
a <- lda_model$scaling
# 定数項
b <- mean(lda_model$means %*% lda_model$scaling)

# 警告あり/なしで色分けした散布図
plot(data$`1日平均運動時間`, data$体格指数, type = "p", 
     col = ifelse(data$改善警告 == "警告あり", "red", "blue"),
     xlab = "1日平均運動時間", ylab = "体格指数")


#判別境界線
abline(c(b / a[2], -a[1] / a[2]))

# 各個体の判別得点
y <- a[1] * data[, "1日平均運動時間"] + a[2] * data[, "体格指数"] - b

# 判別得点に基づき，警告ありと判定された個体:TRUE，無しと判定された個体:FALSE
lda_result <- y * mean(y[data$改善警告 == "警告あり"]) > 0
# lda_resultのTRUE:"改善警告あり", FALSE:"改善警告なし"に置き換える
lda_result[(lda_result == TRUE)] <- "警告あり"
lda_result[(lda_result == FALSE)] <- "警告なし"

# 各個体の判別得点と判別結果
data.frame(y, lda_result)
# 全てのデータ
data.frame(data, y, lda_result)

# 誤判別率
mean(data[, 1] != lda_result)
