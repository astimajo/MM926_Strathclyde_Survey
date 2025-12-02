### TASK 1

## a 
## Assess the internal consistency of the LIKERT questions. Would the removal of any
## questions improve the overall internal consistency?

library(DescTools)

# Load in Data.
data <- read.csv("project_data.csv")

# Sanity Check to see the LIKERT questions columns.
names(data)[6:20]

# Internal Consistency
CronbachAlpha(data[, 6:20], cond = FALSE, conf.level = 0.95, na.rm = TRUE)

# Conditional Cronbach Alpha to see which questions to remove to improve
CronbachAlpha(data[, 6:20], cond = TRUE, conf.level = 0.95, na.rm = TRUE)

# Interpretation:
# The Unconditional Cronbach Alpha which is our measure for internal consistency
# gives us a value of 0.6693380 with Confidence intervals of (0.6489242 , 0.6890686)
# at a 95% Confidence level. This value indicates that our overall internal consistency
# for the 15 questions is "Questionable" based on the table of interpretation for
# Cronbach Alpha.

# Furthermore, by performing a Conditional Cronbach Alpha Analysis on all the 
# questions it is most noteworthy that removing the 9th LIKERT question 
# Networking (Social media is useful for networking) increased our Cronbach
# Alpha to 0.7245072 which elevates the score to an "Acceptable" Internal
# Consistency threshold. Therefore, we can say that Networking is the item
# that reduces internal consistency the most and removing this item will
# improve the Internal consistency of the LIKERT questions the most.
# Removing any other question against the others does not improve anything.

## b
## Provide an appropriate visualisation of the responses to the LIKERT questions and
## use this to identify LIKERT questions that have (i) elicited a strong positive response
## from participants; (ii) elicited a strong negative response from participants;
## (iii) divided the opinion of participants.

# Note: Code LIKERTS as factors first.
# strongly disagree (1), disagree (2), neither agree nor disagree (3); agree (4), strongly
# agree (5)

# Performance, Social, Mental Health, Awareness, Connected, Resources, Expression,
# Community, Networking, Interactions, Time Wasted, Concentration, Negative Content,
# Data Security, Inadequate

data$Performance <- factor(data$Performance, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Social <- factor(data$Social, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Mental_Health <- factor(data$Mental_Health, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Awareness <- factor(data$Awareness, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Connected <- factor(data$Connected, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Resources <- factor(data$Resources, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Expression <- factor(data$Expression, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Community <- factor(data$Community, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Networking <- factor(data$Networking, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Interactions <- factor(data$Interactions, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Time_Wasted <- factor(data$Time_Wasted, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Concentration <- factor(data$Concentration, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Negative_Content <- factor(data$Negative_Content, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Data_Security <- factor(data$Data_Security, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))
data$Inadequate <- factor(data$Inadequate, levels = 1:5, labels = c("strongly disagree", "disagree", "neither agree nor disagree", "agree", "strongly agree"))

# Visualizations
library(likert)

results <- likert(data[,6:20])

plot(results, type="bar")
plot(results, type="heat")
plot(results, type="density")

# Interpretation:

# Strong positive responses can be seen for "Connected" (81.7% A+SA) and 
# "Interactions" (73.6% A+SA). Students mostly strongly believe that Social 
# media helps them stay connected with their family and friends and furthermore, 
# they strongly disagreed that Social media reduced face-to-face interactions 
# which is a positive response when taking the question to its context. 

# Strong negative responses can be seen for "Time Wasted" (60.7% A+SA) and 
# "Data Security" (58.5% A+SA) which shows that students feel that Social media
# is a waste of their time and that they are concerned with their security based
# on how they agreed on the negatively written questions.

# Respondents were most divided on the "Concentration", "Inadequate" and 
# "Mental Health". This means that students were not completely in agreement
# whether it affects their concentration as some people may prefer to use
# Social media as a positive reinforcement tool. As for inadequate some 
# people use Social media as a means to watch entertainment so it does not
# really apply to everyone and for Mental Health this is quite a divided topic
# as some people use Social Media as a means to cope with their loneliness 
# so this can be seen as an escape or they may perceive this as something they
# need for their mental health.


## c
## Conduct an investigation into the missing data.

## Provide an appropriate visualisation of the missing data.

likert.histogram.plot(results) # missing data

## Test whether or not the data is missing completely at random.

## Impute missing data using regression imputation and predictive mean matching.

## Use appropriate plots to assess the two imputations for the variable with the
## most missing data. Comment on the results.

## Use Kolmogorov-Smirnov tests to assess the imputations for the variable with
## the most missing data. Comment on the results.

## Select an imputation method, with justification, and save the dataset that is
## created when this imputation method is used.


