
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
setwd("/Users/Jake/Projects/BeigeBook")
# function score.sentiment based on work by Jeffrey Breen
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

#Load positive and negative lexicons:
pos = scan('positive-words.txt',
             what='character', comment.char=';')
#pos = read.csv("positive-words.txt", header = FALSE, skip=47)
pos_finance = read.csv("LoughranMcDonald_pos.csv", header = FALSE)
neg = scan('negative-words.txt',
           what='character', comment.char=';')
#neg = read.csv("negative-words.txt", header = FALSE, skip = 45)
neg_finance = read.csv("LoughranMcDonald_neg.csv", header = FALSE)

#combine them
pos_all = c(pos, pos_finance)
neg_all = c(neg, neg_finance)
#Beige Book data:::

bb = read.csv("BB.csv")

colnames(bb)

bbText = as.data.frame(bb$text)
bbText$year = bb$year

bbText$Date = as.Date( paste(bb$year, bb$month, bb$day, sep = "-")  , format =   "%Y-%m-%d" )
bbText$Date = strptime(as.character(bbText$Date), "%Y-%m-%d")
colnames(bbText) = c("text", "year", "date")

#Corpus analysis
bbCorpus = Corpus(VectorSource(bbText))

#tolower
bbCorpus = tm_map(bbCorpus, tolower)
bbCorpus = tm_map(bbCorpus, removePunctuation)


#Did it work? inspect(bbCorpus)

#Removing Stopwords - the SMART stopword list is found in the TM package.

myStopwords = stopwords("SMART")

#standard stopwords are generally a useful starting point but sometimes we want to add specific stopwords to
#improve the analysis. Found below are some words to add:

bbStopwords = c(myStopwords, "district", "districts",
                "reported", "noted", "city", "cited",   "activity", "contacts",
                "chicago", "dallas", "kansas", "san", "richmond", "francisco",
                "cleveland", "atlanta", "sales", "boston", "york", "philadelphia",
                "minneapolis", "louis",   "services","year", "levels", " louis")

bbCorpus = tm_map(bbCorpus, function(x)removeWords(x,myStopwords))

#Stemming via the SnowballC package:

bbTextStem = tm_map(bbCorpus, stemDocument)



#some additional cleaning to prepare for the creation of the term document matrix - bbTF is used as a 
#control for the creation of the term document matrix:
bbTF = list(weighting = weightTf, stopwords = bbStopwords,
            removePunctuation = TRUE,
            tolower = TRUE,
            minWordLength = 4,
            removeNumbers = TRUE)

#Create the TDM

bbTDM = TermDocumentMatrix(bbCorpus, control = bbTF)

#Sort by frequent words:
bbFrequent = sort(rowSums(as.matrix(bbTDM)), decreasing = TRUE)

#sum of frequent words:
sum(bbFrequent)
#Explore frequent words:
bbFrequent[1:30]

#Look at terms with a minimum frequency:

findFreqTerms(bbTDM, lowfreq = 50)


#Create word cloud:
#Remove sparse terms from the document with a numeric value of 0.95; representing the maximum allowed sparsity.

bb95 = removeSparseTerms(bbTDM, .95)

# Here we are sorting and counting the row sums of bb95
bbRsums = sort(rowSums(as.matrix(bb95)), decreasing=TRUE)

#Create a dataframe with words and their frequencies.
bbdfRSums = data.frame(word = names(bbRsums), freq = bbRsums)

#Blue to green color palette:
palette = brewer.pal(9, "BuGn")
palette = palette[-(1:2)]

#Create a png and set a directory:
png(filename="~/Projects/BeigeBook/wordcloud.png")

#Create the wordcloud:
bbWordCloud = wordcloud(bbdfRSums$word, bbdfRSums$freq, scale = c(7, .2),
                        min.freq = 4, max.words = 200,
                        random.order = FALSE, colors = palette)

#dev.off to complete plot and savne png.
dev.off()


#Although the cloud is interesting, its now time to run the dataframe against the score sentiment function defined 
#at the beginning fo the file:
#Using score senitment on bbText$text against pos and neg

#Keeps the data and year columns, otherwise they are dropped.
bbKeep = bbText[, c("date", "year")]

#run score.sentiment on the text field using pos words and neg words.  


bbScore = score.sentiment(bbText$text, pos, neg, .progress = "text") #finally works after "scanning" the pos text
#rather than using read.csv. I suspect it is because it is of class character.

#add back bbKeep:

bbSentiment = cbind(bbScore, bbKeep)

#colnames should be date, year, score, text
colnames(bbSentiment)

#find the mean sentiment over the period:
bbSentiment$mean = mean(bbSentiment$score)
#Calculate sum and store it in bbSum
bbSum = bbSentiment$score

#Centre data by subtracting bbSentiment$mean from bbSum
bbSentiment$centred = (bbSentiment$score - bbSentiment$mean)/bbSentiment$mean

#label observations above and below zero as "positive" and "negative" respectively. If its NA we call it zero.

bbSentiment$positive[bbSentiment$centred>0] = 1
bbSentiment$positive[is.na(bbSentiment$positive)] = 0
bbSentiment$negative[bbSentiment$centred<0] = 1
bbSentiment$negative[is.na(bbSentiment$negative)] = 0


#Get a sense for how balanced the data is:
sum(bbSentiment$positive)

sum(bbSentiment$negative)

#Histograms of raw score
ggplot(bbSentiment, aes(x = score)) + geom_histogram()

ggplot(bbSentiment, aes(x = centred)) + geom_histogram()


#Boxplots

ggplot(bbSentiment, aes(x = year, y = centred, group = bbText$year)) + geom_boxplot(
  aes(fill = bbSentiment$year)) + xlab("Year") + ylab("Sentiment Centred") + ggtitle ("Economic Sentiment 
                                                                                      - Beige book")

#We can use the folling to highlight recessionary periods:
rect2001 = data.frame (
  xmin=2001, xmax=2002, ymin=-Inf, ymax=Inf)

rect2007 = data.frame (
  xmin=2007, xmax=2009, ymin=-Inf, ymax=Inf)

#Another boxplot

ggplot(bbSentiment, aes(year, y=centred, group=year)) + 
  geom_boxplot() + 
  geom_rect(data=rect2001, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf), 
            fill='red', alpha=0.2,inherit.aes = FALSE) + 
  geom_rect(data=rect2007, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2,
                                       inherit.aes = FALSE) + 
xlab("Date") + ylab("Sentiment(Centered)") + ggtitle("Economic Sentiment - Beige Book (1996-
2010)")
