# 体格指数の作成
library(readxl)
data <- read_excel("data/finalReport.xlsx")
View(data)
data <- data[-51, ]

# 体格指数に必要なものだけ取り出し
height_weight <- data[, 4:5]

# 相関係数行列による主成分分析
prcomp_model <- prcomp(height_weight, scale = TRUE)

# 固有値のルートと，主成分係数
prcomp_model

# 固有値
prcomp_model$sdev^2

# 寄与率，累積寄与率の表示
summary(prcomp_model)

#主成分負荷量
Loadings <- prcomp_model$rotation * diag(cor(height_weight))^(-0.5) %*% t(prcomp_model$sdev)


# 第1主成分負荷量と第2主成分負荷量の散布図の作成
plot(Loadings[, 1], Loadings[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第1主成分負荷量", ylab = "第2主成分負荷量")
text(Loadings[, 1], Loadings[, 2], rownames(Loadings))

# 主成分得点の取得
scores <- as.data.frame(prcomp_model$x)

# dataに付け加え
data <- cbind(data, scores[1])


# CSVファイルに出力
write.csv(scores, "主成分得点.csv", row.names = FALSE)

# -----既存のファイルに追記したい！→ できなかったので断念…。-----
install.packages("openxlsx")
library(openxlsx)

# Excelファイルを読み込み
wb <- loadWorkbook("finalReport.xlsx")

# 書き込むデータを指定したシートとセルに設定
addWorksheet(wb, sheetName = "Data (2)")
writeData(wb, sheet = "Data (2)", x = scores, startCol = "G", startRow = 1, colNames = TRUE)

# Excelファイルを保存
saveWorkbook(wb, "finalReport2.xlsx")
