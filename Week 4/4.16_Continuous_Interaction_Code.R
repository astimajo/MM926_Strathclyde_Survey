library(tidyverse)
library(ggplot2)
library(effects)
library(jtools)
library(interactions)
library(sandwich)
library(jtools) # for summ()
states <- as.data.frame(state.x77)
fiti <- lm(Income ~ Illiteracy * Murder + `HS Grad`, data = states)
summ(fiti)

interact_plot(fiti, pred = Illiteracy, modx = Murder, partial.residuals = TRUE,
              modx.values = c(4, 5, 6, 8))

sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = FALSE)
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = TRUE)

# NOTE MOD is the Moderator and Pred is the slopes you want to test at different values of the MOD.
# There is an interaction with illiterary rates when murder rates are very high. 
# Note WHEN MURDER RATES ARE VERY HIGH meaning we concluded this based on the Moderator.
