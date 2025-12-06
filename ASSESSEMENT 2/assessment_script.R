### TASK 1

set.seed(1234) # Just to ensure replicability

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

# Missing Answers on LIKERT Questions.
likert.histogram.plot(results)

# Return the LIKERTs from Categorical to Numerical Variables.
data$Performance <- as.numeric(data$Performance)
data$Social <- as.numeric(data$Social)
data$Mental_Health <- as.numeric(data$Mental_Health)
data$Awareness <- as.numeric(data$Awareness)
data$Connected <- as.numeric(data$Connected)
data$Resources <- as.numeric(data$Resources)
data$Expression <- as.numeric(data$Expression)
data$Community <- as.numeric(data$Community)
data$Networking <- as.numeric(data$Networking)
data$Interactions <- as.numeric(data$Interactions)
data$Time_Wasted <- as.numeric(data$Time_Wasted)
data$Concentration <- as.numeric(data$Concentration)
data$Negative_Content <- as.numeric(data$Negative_Content)
data$Data_Security <- as.numeric(data$Data_Security)
data$Inadequate <- as.numeric(data$Inadequate)

require(tidyverse)
require(mice)
require(naniar)
library(simputation)
library(dplyr)

data1 <- data[,2:20] # remove ID
vis_miss(data1) # This seems to be the more appropriate visualization.

## Test whether or not the data is missing completely at random.
mcar_test(data1)

# Interpretation: Data is consistent with MCAR as we fail to reject the Null hypothesis which
# is MCAR. This means that the missingness in the data does not seem to depend 
# on any observed variables in the dataset.

## Impute missing data using regression imputation and predictive mean matching.

# Create shadow matrix first.
as_shadow(data1)
shadow_data <- bind_shadow(data1)

# Regression Imputation

data2 <- complete(mice(shadow_data, method="norm.predict", m=1, maxit=1))

# Predictive Mean Matching

data3 <- complete(mice(shadow_data, method="pmm", m=1, maxit = 1))

## Use appropriate plots to assess the two imputations for the variable with the
## most missing data. Comment on the results.

# Regression Imputation

ggplot(data2,
       aes(x = Awareness,
           colour = Awareness_NA)) + 
  geom_density()

data2 %>%
  ggplot(aes(x = Awareness,
             colour=Awareness_NA)) + 
  stat_ecdf()

# The imputed values follow the same distribution generally as the observed values.
# The imputed curve is noticeably smoother.
# The ECDF shows a good amount of overlap.

# Predictive Mean Matching

ggplot(data3,
       aes(x = Awareness,
           colour = Awareness_NA)) + 
  geom_density()

data3 %>%
  ggplot(aes(x = Awareness,
             colour=Awareness_NA)) + 
  stat_ecdf()

# The imputed values follows the exact shape of the observed data.
# The Density plots overlap almost perfectly.
# ECDF plot also shows an almost perfect overlap.

## Use Kolmogorov-Smirnov tests to assess the imputations for the variable with
## the most missing data. Comment on the results.

ks.test(shadow_data$Awareness, data2$Awareness) # Good p-value
ks.test(shadow_data$Awareness, data3$Awareness) # Best p-value (1). Same distribution. Best.

## Select an imputation method, with justification, and save the dataset that is
## created when this imputation method is used.

# Predictive Mean Matching is the best approach for imputation for this exact problem.
# First reason for this is the Density and ECDF plots which show an almost perfect overlap
# with the observed data with almost identical variance behavior. When we compare this with
# the Regression imputation method it shows that the variance is minimized not to mention
# there is a chance for float values with the regression which affects the behavior of 
# the imputed values. Second reason for this is the extremely high p-value that was
# observed in the K-S Test. The Regression imputation gives us a significant result
# but the PMM imputation gave us a p-value of 1 which is extremely higher which means
# that PMM imputation gave us values that really closely follows the distribution of our
# observed data. Generally, it is also most recommended to use PMM for LIKERT scale
# variables because it is known that this method produces values that closely 
# reflect the original data distribution

library(data.table)

data_imp_final <- copy(data3)

# Add back ID column
data_imp_final$ID <- data$ID

# Place ID as first column for new data
data_imp_final <- data_imp_final %>% select(ID, everything())

# Remove shadow variables
data_imp_final <- data_imp_final[,1:20]

# Sanity check
vis_miss(data_imp_final) # Clean.

# Save the dataset
write.csv(data_imp_final, "data3_imputed_PMM.csv", row.names = FALSE)

## d
## Using the imputed dataset saved in the previous question, perform unweighted 
## logistic generalised linear regression to investigate whether sex or year is associated with
## a positive overall attitude towards social media, as indicated by an average value of
## 3.5 or more for the total score from the LIKERT questions. Provide an interpretation
## of your results.

# Load in Data.
data_new <- read.csv("data3_imputed_PMM.csv")

# Recode Negative questions
data_new$Performance <- 6 - data_new$Performance
data_new$Mental_Health <- 6 - data_new$Mental_Health
data_new$Interactions <- 6 - data_new$Interactions
data_new$Time_Wasted <- 6 - data_new$Time_Wasted
data_new$Concentration <- 6 - data_new$Concentration
data_new$Negative_Content <- 6 - data_new$Negative_Content
data_new$Inadequate <- 6 - data_new$Inadequate
data_new$Data_Security <- 6 - data_new$Data_Security

# Create the Binary target variable.

data_new$summary <- rowSums(data_new[, 6:20])
data_new$positive_attitude <- ifelse(data_new$summary >= 52.5, "positive", "not_positive")
data_new$positive_attitude <- as.factor(data_new$positive_attitude)
data_new$positive_attitude <- relevel(data_new$positive_attitude, ref = "not_positive") # 1 as positive.

# Recode demographic variables as factors.

data_new$Sex <- factor(data_new$Sex)
data_new$Faculty <- factor(data_new$Faculty)

# Logistic Regression Analysis.

model <- glm(positive_attitude ~ Sex + Year, data = data_new, family="binomial"(link="logit"))
summary(model)

# Both Year and Sex is not significantly associated with positive attitudes toward social media
# This is evidenced by the extremely high p-values for Sex and Year in the Logistic Regression
# Results. 

## e
## Now suppose that participants were stratified based on the faculty that they belong
## to, and that there were a total of 2000 students in Faculty 1, 700 students in Faculty
## 2 and 300 students in Faculty 3. Use the imputed data from part (c) to construct a
## weighted logistic generalised linear model to investigate whether sex or year is associated 
## with a positive overall attitude towards social media, as defined in (d). How do the results from 
## the weighted regression compare to those from the unweighted
## regression?

table(data_new$Faculty) # Sample breakdowwn

# Create weights variable

# population sizes variable
population_sizes <- c("1" = 2000, "2" = 700, "3" = 300)

# weights = N / n just like in the API dataset.

sample_sizes <- table(data_new$Faculty)
weight_lookup <- population_sizes / sample_sizes
data_new$weight <- weight_lookup[as.character(data_new$Faculty)]


model_weighted <- glm(positive_attitude ~ Sex + Year, data = data_new,
                      family="binomial"(link="logit"), weights = weight)

summary(model_weighted)

# Both Year and Sex is still not significantly associated with positive attitudes toward social media
# This is evidenced by the still extremely high p-values for Sex and Year in the Logistic Regression
# Results. There is a noticeable improvement in the p-value for Year but overall both are 
# still not significant.

## f
## Create a word cloud associated with positive qualitative responses and a word cloud
## associated with negative qualitative responses. Provide comments comparing the
## two.

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# data=read.csv("Attitudes.csv")
positive=readLines("positive comments.txt")
text=Corpus(VectorSource(positive))

# Convert the text to lower case
text=tm_map(text, content_transformer(tolower))
# Remove english common stopwords
text = tm_map(text, removeWords, stopwords("english"))

# get a matrix of the most common words and their frequencies
dtm = TermDocumentMatrix(text)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

# set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


negative=readLines("negative comments.txt")
text2=Corpus(VectorSource(negative))

# Convert the text to lower case
text2=tm_map(text2, content_transformer(tolower))
# Remove english common stopwords
text2 = tm_map(text2, removeWords, stopwords("english"))

dtm = TermDocumentMatrix(text2)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

# set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# The positive word cloud highlights themes of enjoyment, creativity, and connection, 
# with prominent words such as “enjoy,” “love,” “platform,” “space,” “inspiration,” and 
# “connection,” suggesting that students who responded positively view social media as a 
# supportive, creative, and socially enriching environment. In contrast, the negative word 
# cloud contains terms like “hard,” “overwhelming,” “fake,” “pressure,” “comparison,” and “distracts,” 
# reflecting concerns about emotional strain, misinformation, unrealistic standards, and cognitive overload.
# Overall, the contrast between the two word clouds shows that positive attitudes centre on community and
# empowerment, whereas negative attitudes emphasise stress, distraction, and the psychological burdens 
# associated with social media use.

### EDIT THE COMMENTS AND RESULTS IN THE SUMMARY PAPER.
