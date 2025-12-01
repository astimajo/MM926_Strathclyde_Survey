library(tidyverse)
library(ggplot2)
library(effects)
library(jtools)
library(interactions)
library(sandwich)
library(jtools) # for summ()

data <- read.csv("Exercise and Weight Loss.csv")

model <- lm(Time ~ Weight * Loss + Age + Sex, data = data)
summ(model)

# There is a significant p-value for the interaction variables.


interact_plot(model, pred = Loss, modx = Weight, partial.residuals = TRUE)

sim_slopes(model, pred = Loss, modx = Weight, johnson_neyman = TRUE, jnalpha = 0.5, control.fdr = TRUE)

# The J-N interval says that if the loss is below 19.82 there is a significant
# effect on Time. 
