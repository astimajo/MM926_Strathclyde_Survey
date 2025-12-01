# install.packages("likert")

likerts=read.csv("Course Satisfaction.csv")

likerts$I.enjoy.learning.statistics=factor(likerts$I.enjoy.learning.statistics, labels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
likerts$Managing.my.time.in.this.course.has.been.difficult=factor(likerts$Managing.my.time.in.this.course.has.been.difficult, labels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
likerts$Instructions.for.exercises.are.clear=factor(likerts$Instructions.for.exercises.are.clear, labels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
likerts$Feedback.on.my.work.has.been.fair=factor(likerts$Feedback.on.my.work.has.been.fair, labels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
likerts$I.am.satisfied.with.the.course=factor(likerts$I.am.satisfied.with.the.course, labels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
likerts$I.would.not.choose.this.course.again=factor(likerts$I.would.not.choose.this.course.again, labels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))

library(likert)
?likert
results=likert(likerts[,1:6])
likert.histogram.plot(results) # missing data
summary(results, centre=3)

plot(results, type="bar")
plot(results, type="heat")
plot(results, type="density")


likerts$I.enjoy.learning.statistics=as.numeric(likerts$I.enjoy.learning.statistics)
likerts$Managing.my.time.in.this.course.has.been.difficult=as.numeric(likerts$Managing.my.time.in.this.course.has.been.difficult)
likerts$Instructions.for.exercises.are.clear=as.numeric(likerts$Instructions.for.exercises.are.clear)
likerts$Feedback.on.my.work.has.been.fair=as.numeric(likerts$Feedback.on.my.work.has.been.fair)
likerts$I.am.satisfied.with.the.course=as.numeric(likerts$I.am.satisfied.with.the.course)
likerts$I.would.not.choose.this.course.again=as.numeric(likerts$I.would.not.choose.this.course.again)

#Reverse code
likerts[ , 4] = 6 - likerts[ , 4]
likerts[ , 6] = 6 - likerts[ , 6]
View(likerts)

likerts$summary=rowSums(likerts[ , c(1:6)], na.rm=TRUE)
likerts$high <- factor(ifelse(likerts$summary>=24, "high", "low"))

modelcon=lm(summary~Age+Sex+Country+Hours, data=likerts)
summary(modelcon)

modelbin=glm(high~Age+Sex+Country+Hours, family="binomial"(link="logit"), data=likerts)
summary(modelbin)

likerts$change=likerts$Summary2-likerts$summary
likerts$improve <- factor(ifelse(likerts$change>=1, "high", "low"))




# Ordinal Regression

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

Changes=read.csv("Course Satisfaction Change.csv")
Changes$Change=factor(Changes$Change, labels = c("Negative", "Neutral", "Positive"))

## fit ordered logit model and store results 'm'
m <- polr(Change ~ Age+Sex+Country+Hours, data = Changes, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

ci=confint.default(m) # CIs assuming normality

## odds ratios
exp(coef(m))

## OR and CI
exp(cbind(OR = coef(m), ci))





