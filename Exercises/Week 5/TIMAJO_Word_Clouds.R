# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# data=read.csv("Attitudes.csv")
best=readLines("positive comments.txt")
text=Corpus(VectorSource(best))

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

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


changes=readLines("Negative comments.txt")
text2=Corpus(VectorSource(changes))

# Convert the text to lower case
text2=tm_map(text2, content_transformer(tolower))
# Remove english common stopwords
text2 = tm_map(text2, removeWords, stopwords("english"))

dtm = TermDocumentMatrix(text2)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))