require(tidyverse)
require(mice)
require(naniar)
library(simputation)
library(dplyr)

donors=read.csv("Blood donor data.csv")
donors$Sex=as.factor(donors$Sex)
donors$Adverse.Event=as.factor(donors$Adverse.Event)

vis_miss(donors)

mcar_test(donors) # Low value means not MCAR.

#Shadow Matrix

as_shadow(donors)
shadow_donors=bind_shadow(donors)

chisq.test(shadow_donors$Sex, shadow_donors$Weight_NA) # High P-value maybe not.


ggplot(shadow_donors,
       aes(x = Height,
           colour = X5.Year.Hb_NA)) + 
  geom_density()

ggplot(shadow_donors,
       aes(x = Age,
           fill = X5.Year.Hb_NA)) + 
  geom_histogram()

ggplot(donors,
       aes(x = Height, y= X5.Year.Hb)) +
  geom_miss_point()

# The likert package is useful for this purpose. 
# Given a dataset called “data” here in which the 
# columns represent Likert scale responses to a set of questions, 
# if these columns are factors rather than integer or numeric variables, 
# the following code may be used, if the likert package is installed first:
  

library(likert)  
responses=likert(data) # define the results as a likert object in R for use below
likert.histogram.plot(responses) # histograms

plot(responses, type=”bar”) # barcharts
plot(responses, type=”heat”) # heatmap
plot(responses, type=”density”) # densityplots

# or

likert.bar.plot(responses)
likert.heat.plot(responses)
likert.density.plot(responses)




# Imputation

# Mean imputation

imp_mean=impute_mean(shadow_donors$Weight)

hist(shadow_donors$Weight)
hist(imp_mean)


# Regression Imputation

donordata1=complete(mice(donors, method="norm.predict", m=1, maxit=1))

#Specify Imputation Model

shadow_donors %>%
  impute_lm(Weight ~ Sex+Age+Adverse.Event) %>%
  ggplot(aes(x = Donations,
             y = Weight, 
             colour=Weight_NA)) + 
  geom_point()

# Generate edf

shadow_donors %>%
  impute_lm(Weight ~ Sex+Age+Adverse.Event) %>%
  ggplot(aes(x = Donations,
             colour=Weight_NA)) + 
  stat_ecdf()

#PMM

donordata2=complete(mice(shadow_donors, method="pmm", m=1, maxit = 1))

ggplot(donordata2, 
       aes(x = X5.Year.Hb,
           colour = X5.Year.Hb_NA)) + 
  geom_density()

ggplot(donordata2, 
       aes(x = X5.Year.Hb,
           fill = X5.Year.Hb_NA)) + 
  geom_histogram()


donordata2 %>%
  ggplot(aes(x = Donations,
             y=Weight,
             colour = Weight_NA)) + 
  geom_point()


# Generate edf

donordata2 %>%
  ggplot(aes(x = Weight,
             colour = Weight_NA)) + 
  stat_ecdf()



# Kolmogorov-Smirnov Test

ks.test(donordata1$X5.Year.Hb, donordata2$X5.Year.Hb) # High p-value meaning they come from the same distribution.
ks.test(shadow_donors$X5.Year.Hb, donordata2$X5.Year.Hb)
