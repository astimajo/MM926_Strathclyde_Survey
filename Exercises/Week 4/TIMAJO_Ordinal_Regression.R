require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

Changes=read.csv("House Price Changes.csv")
Changes$Change=factor(Changes$Change, labels = c("Negative", "Neutral", "Positive"))

## fit ordered logit model and store results 'm'
m <- polr(Change ~ Property.Type+Area+EPC.Rating, data = Changes, Hess=TRUE)

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