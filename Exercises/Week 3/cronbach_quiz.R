filename <- "Cronbachs_alpha.csv"

library(DescTools)

data = read.csv(filename)

CronbachAlpha(data, cond = FALSE, conf.level = 0.95)
CronbachAlpha(data[, 1:4], cond = TRUE, conf.level = 0.95)
