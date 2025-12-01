likerts <- read.csv("University Campus.csv")

# Automated 
likerts[, paste0("Q", 1:9)] <- lapply(
  likerts[, paste0("Q", 1:9)],
  function(x) factor(
    x,
    levels = 1:5,
    labels = c("Strongly Disagree",
               "Disagree",
               "Neither Agree nor Disagree",
               "Agree",
               "Strongly Agree"),
    ordered = TRUE
  )
)

library(likert)

results=likert(likerts[,1:9])
likert.histogram.plot(results) # No Missing Data.
summary(results, centre=3)

plot(results, type="bar")
plot(results, type="heat")
plot(results, type="density")

# Automate Reverse

likerts[, paste0("Q", 1:9)] <- lapply(
  likerts[, paste0("Q", 1:9)],
  function(x) as.numeric(x)   # Uses the factor's underlying codes 1–5
)

# Reverse code
likerts[ , 4] = 6 - likerts[ , 4]
likerts[ , 8] = 6 - likerts[ , 8]

likerts$avg_summary <- rowSums(likerts[, 1:9], na.rm = TRUE)
likerts$major <- factor(ifelse(likerts$avg_summary >= 31.5, "major", "not_major"))

likerts$major=as.factor(likerts$major)
relevel(likerts$major,2)

likerts$Sex=as.factor(likerts$Sex)


modelcon=lm(avg_summary~Sex+Year.of.Study, data=likerts)
summary(modelcon)

modelbin=glm(major~Sex+Year.of.Study, family="binomial"(link="logit"), data=likerts)
summary(modelbin)

# No Significant Variables contributing to the targets.


