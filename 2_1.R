# 基本統計量

library(readxl)
data <- read_excel("finalReport.xlsx")
View(data)

# 改善警告あり:1 なし:0 に変換
data$改善警告 <- ifelse(data$改善警告 == "警告あり", 1, 0)

# install.packages("rpivotTable")
library(rpivotTable)

rpivotTable(data)

