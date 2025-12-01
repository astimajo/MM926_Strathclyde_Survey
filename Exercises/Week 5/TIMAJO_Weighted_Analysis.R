library(survey)
data("api")

apistrat_design <- svydesign(data = apistrat,
                             weights = ~pw, 
                             fpc = ~fpc, 
                             id = ~1, 
                             strata = ~stype)

svymean(~pcttest, apistrat_design)
mean(apistrat$pcttest)
# Unweighted Mean is smaller for pcttest.

svymean(~full, apistrat_design)
mean(apistrat$full)
# Unweighted Mean smaller for full.

svyhist(~pcttest, apistrat_design)
hist(apistrat$pcttest)

svyhist(~full, apistrat_design)
hist(apistrat$full)

svyciprop(~awards, apistrat_design)
table(apistrat$awards)
113/200

# Unweighted proportion is smaller for awards.

svyplot(api.stu~meals, apistrat_design, style="transparent", legend=TRUE)

lmw=svyglm(full~awards+meals+acs.k3+acs.46+avg.ed+mobility, design=apistrat_design)
summary(lmw)

lmuw=lm(full~awards+meals+acs.k3+acs.46+avg.ed+mobility, data=apistrat)
summary(lmuw)