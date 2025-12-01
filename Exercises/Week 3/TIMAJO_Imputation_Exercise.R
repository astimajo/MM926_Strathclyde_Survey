require(tidyverse)
require(mice)
require(naniar)
library(simputation)
library(dplyr)

shopping <- read.csv("Shopping survey.csv")

vis_miss(shopping) # Q3 has the most -> 25% missing data.
mcar_test(shopping) # Quite high -> MCAR data.

# Shadow Matrix

as_shadow(shopping)
shadow_shopping <- bind_shadow(shopping)

# Mean imputation
imp_mean=impute_mean(shadow_shopping$Q3)

hist(shadow_shopping$Q3)
hist(imp_mean) # The peaks shifted.

# Regression imputation

shpppingdata1=complete(mice(shopping, method="norm.predict", m=1, maxit=1))

# Decimals showed up.

# Predictive Mean Matching

shoppingdata2=complete(mice(shadow_shopping, method="pmm", m=1, maxit = 1))

# Imputed vs Observed.

ks.test(shadow_shopping$Q3, imp_mean) # Low p-value. They come from different distributions.
ks.test(shadow_shopping$Q3, shpppingdata1$Q3) # High p-value. Same distribution BUT decimals.
ks.test(shadow_shopping$Q3, shoppingdata2$Q3) # Very High p-value. Same distribution. Best.
