library(survey)
data("api")

apistrat_design <- svydesign(data = apistrat,
                             weights = ~pw, 
                             fpc = ~fpc, 
                             id = ~1, 
                             strata = ~stype)
summary(apistrat_design)

svymean(~growth, apistrat_design)
mean(apistrat$growth)

svyciprop(~both, apistrat_design)
table(apistrat$both)
113/200

# visualise

svyhist(~meals, apistrat_design)
hist(apistrat$meals)

svyplot(api00~api99, apistrat_design, xlab="1999 API",ylab="2000 API", style="transparent", legend=TRUE)

#regression

lmw=svyglm(growth~awards+meals+mobility+avg.ed, design=apistrat_design)
summary(lmw)

lmuw=lm(growth~awards+meals+mobility+avg.ed, data=apistrat)
summary(lmuw)

