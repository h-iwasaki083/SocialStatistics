library(readxl)
data <- read_excel("data/finalReport.xlsx")
data <- data[data$戸籍上の性別 == "男", ]
View(data)

data$改善警告 <- ifelse(data$改善警告 == "警告あり", 1, 0)
data <- data[, -2]
data <- data[, 1:5]

# データが壊れていないか一応チェック
library(rpivotTable)
rpivotTable(data)

# -----説明変数同士の相関等々確認-----
# PC1列(= res_data)を削除
exp_data <- data[, -1]

# 相関係数
cor(exp_data)

# -----体格指数の作り直し-----
height_weight <- data[, 3:4]


# 相関係数行列による主成分分析
prcomp_model_2 <- prcomp(height_weight, scale = TRUE) 

# 固有値のルートと，主成分係数
prcomp_model_2

# 固有値
prcomp_model_2$sdev^2

# 寄与率，累積寄与率の表示
summary(prcomp_model_2)

#主成分負荷量
Loadings <- prcomp_model_2$rotation * diag(cor(height_weight))^(-0.5) %*% t(prcomp_model_2$sdev)


S# 第1主成分負荷量と第2主成分負荷量の散布図の作成
plot(Loadings[, 1], Loadings[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第1主成分負荷量", ylab = "第2主成分負荷量")
text(Loadings[, 1], Loadings[, 2], rownames(Loadings))

# 主成分得点の取得
scores <- as.data.frame(prcomp_model_2$x)
data <- cbind(data, scores$PC1)
colnames(data) <- c("改善警告","年齢", "身長", "体重", "1日平均運動時間", "体格指数")

# -----体力指数の作成-----
# 体力指数に必要なものだけ取り出し
age <- data[, 3]
exercise <- data[, 5]
age_exercise <- cbind(age, exercise)


# 相関係数行列による主成分分析
prcomp_model_2 <- prcomp(age_exercise, scale = TRUE) 

# 固有値のルートと，主成分係数
prcomp_model_2

# 固有値
prcomp_model_2$sdev^2

# 寄与率，累積寄与率の表示
summary(prcomp_model_2)

#主成分負荷量
Loadings <- prcomp_model_2$rotation * diag(cor(age_exercise))^(-0.5) %*% t(prcomp_model_2$sdev)


S# 第1主成分負荷量と第2主成分負荷量の散布図の作成
plot(Loadings[, 1], Loadings[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第1主成分負荷量", ylab = "第2主成分負荷量")
text(Loadings[, 1], Loadings[, 2], rownames(Loadings))

# 主成分得点の取得
scores <- as.data.frame(prcomp_model_2$x)
data <- cbind(data, scores$PC1)
colnames(data) <- c("改善警告","年齢", "身長", "体重", "1日平均運動時間", "体格指数", "体力指数")


# CSVファイルに出力
write.csv(scores, "data/主成分得点_3.csv", row.names = FALSE)

# -----体力指数の作成-----
# 体力指数に必要なものだけ取り出し
age <- data[, 3]
exercise <- data[, 6]
age_exercise <- cbind(age, exercise)


# 相関係数行列による主成分分析
prcomp_model_2 <- prcomp(age_exercise, scale = TRUE) 

# 固有値のルートと，主成分係数
prcomp_model_2

# 固有値
prcomp_model_2$sdev^2

# 寄与率，累積寄与率の表示
summary(prcomp_model_2)

#主成分負荷量
Loadings <- prcomp_model_2$rotation * diag(cor(age_exercise))^(-0.5) %*% t(prcomp_model_2$sdev)


S# 第1主成分負荷量と第2主成分負荷量の散布図の作成
plot(Loadings[, 1], Loadings[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第1主成分負荷量", ylab = "第2主成分負荷量")
text(Loadings[, 1], Loadings[, 2], rownames(Loadings))

# 主成分得点の取得
scores <- as.data.frame(prcomp_model_2$x)
data <- cbind(data, scores$PC1)

# CSVファイルに出力
write.csv(scores, "data/主成分得点_3.csv", row.names = FALSE)




# -----ロジスティック回帰にかける-----
logistic_model_2 <- glm(改善警告 ~ 体格指数 + 体力指数,
                        data = data, family = binomial)

summary(logistic_model_2)

# 推定
logistic_result_2 <- predict(logistic_model_2, data, type = "response")
logistic_result_2 <- round(logistic_result_2)

logistic_table_2 <- data.frame(Status = data[, 1], Predict = logistic_result_2)

logistic_table_2 <- table(logistic_table_2)

# 誤判別率
mean(data[, 1] != logistic_result_2)

1-sum(diag(logistic_table_2))/sum(logistic_table_2)

