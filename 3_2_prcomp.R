library(readxl)
data <- read_excel("data/data_edu.xlsx")

# 相関係数行列による主成分分析
prcomp_model <- prcomp(data[, -1], scale = TRUE)

# 固有値のルートと，主成分係数
prcomp_model

# 固有値
prcomp_model$sdev^2

# 寄与率，累積寄与率の表示
summary(prcomp_model)

#主成分負荷量
Loadings <- prcomp_model$rotation * diag(cor(data[, -1]))^(-0.5) %*% t(prcomp_model$sdev)


# 第1主成分負荷量と第2主成分負荷量の散布図の作成
par("mar"=c(2,2,2,2))
plot(Loadings[, 1], Loadings[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第1主成分負荷量", ylab = "第2主成分負荷量")
text(Loadings[, 1], Loadings[, 2], rownames(Loadings))

# 主成分得点の取得
scores <- as.data.frame(prcomp_model$x)

cor_matrix3 <- cor(data[ , -1])
corrplot(cor_matrix3, method = "color")

# CSVファイルに出力
write.csv(scores[1], "data/主成分得点_5.csv", row.names = FALSE)
