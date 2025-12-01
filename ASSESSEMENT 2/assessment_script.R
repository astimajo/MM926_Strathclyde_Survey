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

