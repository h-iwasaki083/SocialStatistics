library(readxl)
data <- read_excel("finalReport.xlsx")
View(data)

data$改善警告 <- ifelse(data$改善警告 == "警告あり", 1, 0)
data$戸籍上の性別<- ifelse(data$戸籍上の性別 == "男", 1, 0)

# データが壊れていないか一応チェック
library(rpivotTable)
rpivotTable(data)

# 説明変数同士の相関等々確認
# PC1列(= res_data)を削除
exp_data <- data[, -8]

# 相関係数
cor(exp_data)
