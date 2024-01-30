library(readxl)
data <- read_excel("data/data_edu2019.xlsx")
View(data)

# クロス集計(使いこなせない)
library(rpivotTable)

rpivotTable(data)

# 相関係数行列の計算
cor_matrix <- cor(data)

# CSVファイルに書き込む
write.csv(cor_matrix, file = "correlation_matrix.csv", row.names = TRUE)

install.packages("corrplot")


cleaned_data <- read_excel("data/data_edu2.xlsx")
cor_matrix <- cor(cleaned_data[ , -1])

# 相関係数行列のヒートマップの作成
library(corrplot)
corrplot(cor_matrix, method = "color")

cleaned_data <- cleaned_data[ , -2]
cleaned_data
cleaned_data <- cleaned_data[ , -3]
cleaned_data <- cleaned_data[ , -4]
cleaned_data <- cleaned_data[ , -11]
cleaned_data <- cleaned_data[ , -10]


prcomp_data <- cleaned_data
prcomp_data

cor_matrix2 <- cor(prcomp_data[ , -1])
corrplot(cor_matrix2, method = "color")

# CSVファイルに書き込む
write.csv(prcomp_data, file = "data/data_edu.csv", row.names = TRUE)
