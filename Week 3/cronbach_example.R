library(DescTools)

data = read.csv("Sample Survey Data.csv")

CronbachAlpha(data, cond = FALSE, conf.level = 0.95)
CronbachAlpha(data[, 1:9], cond = TRUE, conf.level = 0.95)
