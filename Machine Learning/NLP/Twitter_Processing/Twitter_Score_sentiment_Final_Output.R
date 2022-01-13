library(syuzhet)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(ROAuth)
library(stringr)
library("httr")
library("jsonlite")
library(dplyr)
library("openssl")
library("httpuv")
library(rtweet)


ckey <- "NP0fC5PHRmSeKcLRqsfuPs7e1"
skey <- "sjFpvWMZQlBCYxhyPk6IRD7Hm3E2RTz47I4Xup3Gud2D8UTrxu"
Bearertoken <- "AAAAAAAAAAAAAAAAAAAAAKmuXwEAAAAAshE6S1rSKV2fDY%2BSe2NM%2F35FqmQ%3D50k8KfTIpoyb4JMEzkUN5kHkPPs2y6CEJ4dEnJmzMaihrGhNoR"
AccessTok <- "1480126935300820992-q2lz7s4fR9P2K3324AtxkzsKcV24c4"
sAccessToken <- "v2YZKTUEdPokgoEi0UUgVWjogMF8g0QJwhCMHp3n70QzQ"


token <- create_token(app = "NLPDataGather",
                      consumer_key = ckey,
                      consumer_secret = skey)

### --------------------------------------------------------------------------------

##Functions 
cleanTweets = function(object.with.tweets){
   # list to dataframe
   df.tweets <- as.data.frame(object.with.tweets)
   ### - ----
   # Removes RT
   df.tweets$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
   
   #Remove non-ASCII characters
   Encoding(df.tweets$text_clean) = "latin1"
   iconv(df.tweets$text_clean, "latin1", "ASCII", sub = "")
   
   # Removes @<twitter handle>
   df.tweets$text_clean = gsub("@\\w+", "", df.tweets$text_clean)
   # Removes punctuations
   df.tweets$text_clean = gsub("[[:punct:]]", "", df.tweets$text_clean)
   # Removes numbers
   df.tweets$text_clean = gsub("[[:digit:]]", "", df.tweets$text_clean)
   # Removes html links
   df.tweets$text_clean = gsub("http\\w+", "", df.tweets$text_clean)
   # Removes unnecessary spaces
   df.tweets$text_clean = gsub("[ \t]{2,}", "", df.tweets$text_clean)
   df.tweets$text_clean = gsub("^\\s+|\\s+$", "", df.tweets$text_clean)
   # Fix for error related to formatting 'utf8towcs'"
   df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
   return(df.tweets)
}

searchThis = function(search_string,number.of.tweets = 100, include_rts = FALSE, lang = "en")
{
   search_tweets(q = search_string, 
                 n = number.of.tweets,
                 include_rts = include_rts,
                 lang = lang)
}


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
   require(plyr)
   require(stringr)
   list=lapply(sentences, function(sentence, pos.words, neg.words)
   {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
   }, pos.words, neg.words)
   score_new=lapply(list, `[[`, 1)
   pp1=score=lapply(list, `[[`, 2)
   nn1=score=lapply(list, `[[`, 3)
   
   scores.df = data.frame(score=score_new, text=sentences)
   positive.df = data.frame(Positive=pp1, text=sentences)
   negative.df = data.frame(Negative=nn1, text=sentences)
   
   list_df=list(scores.df, positive.df, negative.df)
   return(list_df)
}

### ---------------------------------------------------------------------
df.tweets = searchThis("Winner", n=150)

#Adding words to positive and negative databases
pos.words = scan("C:/Users/Adam/Documents/Adam's File/negative-words.txt", what='character', comment.char=';')
neg.words = scan("C:/Users/Adam/Documents/Adam's File/positive-words.txt", what='character', comment.char=';')
pos.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')

#Extracting textual part of the tweets

#sample=NULL  #Initialising  #We can get the text from df$text, which are the cleand tweets
#for (tweet in sindhu.tweets)
#sample = c(sample,tweet$getText())

#Removing emoticons

#s <- searchTwitter('#emoticons', cainfo="cacert.pem")
df <- cleanTweets(df.tweets)

# Clean the tweets
result = score.sentiment(df$text_clean, pos.words, neg.words)


 library(reshape)
#Creating a copy of result data frame
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
 table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

#Making percentage columns

p=table_final$Positive/(table_final$Positive+table_final$Negative)
p[ is.nan(p) ] <- 0
table_final$Postive_percentage=p
rename(table_final, c("Positive_percentage"=Pos_percent"))
n=table_final$Positive/(table_final$Positive+table_final$Negative)
n[ is.nan(n) ] <- 0
table_final$Neg_percent=n


#Creating Histogramm

hist(table_final$Score, colour=rainbow(10))
hist(table_final$Positive, colour=rainbow(10))
hist(table_final$Negative, colour=rainbow(10))

#Creating Pie Chart

install.packages("plotrix")
library(plotrix)

 slices <- c(sum(table_final$Positive), sum(table_final$Negative))
lbls <- c("Positive", "Negative")
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Sentiment Analysis")
pie3D(slices, labels = lbls, explode=0.0, col=rainbow(length(lbls)), main="Sentiment Analysis")

#Creating Pie chart with percentages for degree of positive,neagtive,neutral

Sc= table_final$Score
good<- sapply(table_final$Score, function(Sc) Sc > 0 && Sc <= 3)
pos1=table_final$Score[good]
pos1_len=length(pos1)

vgood<- sapply(table_final$Score, function(Sc) Sc > 3 && Sc < 5)
pos2=table_final$Score[vgood]
pos2_len=length(pos2)

vvgood<- sapply(table_final$Score, function(Sc) Sc >= 6)
pos3=table_final$Score[vvgood]
pos3_len=length(pos3)

Sc= table_final$Score
bad<- sapply(table_final$Score, function(Sc) Sc < 0 && Sc >= -3)
neg1=table_final$Score[bad]
neg1_len=length(neg1)

vbad<- sapply(table_final$Score, function(Sc) Sc < -3 && Sc >= -5)
neg2=table_final$Score[vbad]
neg2_len=length(neg2)

vvbad<- sapply(table_final$Score, function(Sc) Sc <= -6)
neg3=table_final$Score[vvbad]
neg3_len=length(neg3)

neutral= sapply(table_final$Score, function(Sc) Sc == 0)
neu=table_final$Score[neutral]
neu_len=length(neu)

slices1 <- c(pos1_len,neg3_len, neg1_len, pos2_len,  neg2_len, neu_len, pos3_len)
lbls1 <- c( "Good","Awful","Unsatisfactory", "Great", "Poor", "Neutral", "Outstanding")
pct=round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1, pct) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
  	main="No. of tweets with particular sentiment")

#WORDCLOUD

#install.packages("wordcloud")
library(wordcloud)

#install.packages("tm")
library(tm)

earthquake.tweets=searchThis("earthquake", lang="en", n=1500)
df <- cleanTweets(earthquake.tweets)
earthquake_text <- sapply(df$text_clean,function(row) iconv(row, "latin1", "ASCII", sub=""))

 #str(earthquake_text) -> gives character vector

quake_corpus = Corpus(VectorSource(earthquake_text))

#inspect(quake_corpus[1])

#clean text

quake_clean = tm_map(quake_corpus, removePunctuation)
quake_clean = tm_map(quake_clean, content_transformer(tolower))
quake_clean = tm_map(quake_clean, removeWords, stopwords("english"))
quake_clean = tm_map(quake_clean, removeNumbers)
quake_clean = tm_map(quake_clean, stripWhitespace)

#cleaning most frequent words
#italy_clean = tm_map(quake_clean, removeWords, c ("Italy", "earthquake"))
wordcloud(quake_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(4,0.5))


## Human_testing 

write.csv(table_final, "sentiment_predictor.csv")
