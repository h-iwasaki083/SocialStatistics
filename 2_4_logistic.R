# 性別、年齢、身長、体重と改善警告の有無のロジスティック回帰
library(readxl)
data <- read_excel("data/finalReport.xlsx")
View(data)

data$改善警告 <- ifelse(data$改善警告 == "警告あり", 1, 0)
data$戸籍上の性別<- ifelse(data$戸籍上の性別 == "男", 1, 0)

# 男女別データ
mData <- data[data$戸籍上の性別 == 1, -2]
fData <- data[data$戸籍上の性別 == 0, -2]

# -----<< ロジスティック回帰 >>-----
logistic_model <- glm(改善警告 ~ 戸籍上の性別 + 年齢 + 身長 + 体重,
                      data = data, family = binomial)

summary(logistic_model)

# モデルの選択
# logistic_model_1 <- step(logistic_model)

# 推定
logistic_result <- predict(logistic_model, data, type = "response")
logistic_result <- round(logdsistic_result)

logistic_table <- data.frame(Status = data[, 1], Predict = logistic_result)

logistic_table <- table(logistic_table)

# 誤判別率
mean(data[, 1] != logistic_result)

1-sum(diag(logistic_table))/sum(logistic_table)

# -----<< 男女別にしてみる >>-----
# 男女別データの作成
mData <- data[data$戸籍上の性別 == 1, -2]
fData <- data[data$戸籍上の性別 == 0, -2]

m_logistic_model <- glm(改善警告 ~ 年齢 + 身長 + 体重,
                      data = mData, family = binomial)
f_logistic_model <- glm(改善警告 ~ 年齢 + 身長 + 体重,
                        data = fData, family = binomial)

# summary(logistic_model)

# 推定
m_logistic_result <- predict(m_logistic_model, mData, type = "response")
m_logistic_result <- round(m_logistic_result)

f_logistic_result <- predict(f_logistic_model, fData, type = "response")
f_logistic_result <- round(f_logistic_result)

logistic_table <- data.frame(Status = data[, 1], Predict = logistic_result)

logistic_table <- table(logistic_table)

# 誤判別率
mean(mData[, 1] != m_logistic_result)
mean(fData[, 1] != f_logistic_result)

1-sum(diag(logistic_table))/sum(logistic_table)