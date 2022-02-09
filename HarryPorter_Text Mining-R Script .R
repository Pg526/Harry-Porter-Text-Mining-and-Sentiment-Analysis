
#Installing and loading the R packages
install.packages("dplyr")
library(dplyr)
install.packages("dplR")
library(dplR) # dplyr package is used for data manipulation; it uses pipes: %>%
install.packages("tm")
library(tm) # contains the stopwords dictionary
#install.packages("textstem")
library(textstem) # used for stemming and lemmatization
library(tidytext)
install.packages("RSentiment")
library(RSentiment)
install.packages("wordcloud")
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("radarchart")
library(radarchart)
install.packages("reshape2")
library(reshape2)
install.packages("wordcloud")
library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("tm")
library(tidyverse)
library(lexicon)
library(textclean)
library(textshape)
library(tokenizers)
install.packages("backports")
library(backports)
install.packages("NLP")
library(NLP)
library(RColorBrewer)
install.packages("akc")
library(akc)

# Calc overall polarity score
install.packages("sentimentr")
library(sentimentr)
install.packages("magrittr")
library(magrittr)
library(lexicon)
install.packages("syuzhet")
install.packages("fastshp", repos = "https://rforge.net", type = "source")
library(fastshp)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggplot")
library(ggplot2)
install.packages("quickPlot")
library(quickPlot)

##########################################
############# All dialogues #############
#########################################


#Extract data
text <- read.csv("Script.csv", stringsAsFactors = FALSE)
#LowerCase
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(listener_lower = tolower(Listener))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
#No Number
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(listener_noNumber = gsub('[[:digit:]]','',listener_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
#No Stopword
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(listener_noStopword = gsub(stopwords_regex,'',listener_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
#No Punctuation
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(listener_noPunc = gsub('[[:punct:]]','',listener_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
#No whiteSpace
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(listener_noWhitespace = gsub('\\s+',' ',listener_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
#Lemmatization
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(listener_lemma = lemmatize_strings(listener_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
#text <- text %>% mutate(speaker_stem = stem_strings(Speaker_lemma))
#text <- text %>% mutate(conversation_stem = stem_strings(conversation_lemma))
#Removed stem function as it is replacing "y" with "i" and also deleting "s" in the end  of the word

#text <- text %>% select((speaker_lemma), (conversation_lemma))
#text <- text %>% rename(speaker = speaker_lemma)
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)

text_df <- tibble(text)
#Add line number column
text_df <-  text_df %>% mutate(linenumber = row_number())

#Tokenize #Install or load package tidytext
text_df <-  text_df %>% unnest_tokens(words, conversation)
#?unnest_tokens
#unnest_tokens(text_df,text, words)
#text_df <-  text_df %>% tokenize_words(text_df$conversation)
#tidytext::unnest_tokens(read.csv("chennai_reviews.csv", stringsAsFactors = FALSE)


#Remove stop words
#text_df <- text_df %>% anti_join(stop_words, by = "words")

#Syuzhet Polarity
overall_polarity <- get_sentiment(text_df$words, method = "syuzhet", path_to_tagger = NULL,cl = NULL, language = "english", lexicon = NULL)
head(overall_polarity)
summary(overall_polarity)

# bing
overall_bing <- get_sentiment(text_df$words, method="bing")
head(overall_bing)
summary(overall_bing)
#affin
overall_affin <- get_sentiment(text_df$words, method="afinn")
head(overall_affin)
summary(overall_affin)

#Emotional Valence

text_sentiment <- get_sentiment(text_df$words)
plot(
  text_sentiment, 
  type="l", 
  main="Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

#compare the first row of each vector using sign function
rbind(
  sign(head(overall_polarity)),
  sign(head(overall_bing)),
  sign(head(overall_affin))
)

d<-get_nrc_sentiment(text_df$words)
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:4071])) #check the number of observation and ammend it in the syntax
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
qplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
colorbar <- brewer.pal(8, "Set3") 
barplot(
  #sort(colSums(prop.table(d[, 1:8]))), 
  colSums(prop.table(d[, 1:8])),
  horiz = TRUE, 
  #cex.names = 1
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

#Plot 3: Stack Plot for positive negative
td_new2<-td_new[9:10,]
td_new3 <- td_new2 %>% mutate(percent = count / sum(count) * 100)

SP <- ggplot(td_new3, aes(fill=sentiment, y=percent, x="")) + 
  geom_bar(position="fill", stat="identity")
SP


#Plot 4: Positive-Negative Horizontal bar
#Change columns to 9:10 to get positive negative
td_new2<-td_new[9:10,]

bp<- ggplot(td_new2, aes(x="", y=count, fill=sentiment))+
  geom_bar(width = 1, stat = "identity")
bp


############################################
############# Harry dialogues ##############
############################################

#Understanding emotions
#Only Harry dialogues
#Extract data
#text <- read.csv("Script.csv", stringsAsFactors = FALSE)
text <- read.csv("Harry_script.csv", stringsAsFactors = FALSE)

#Harry<- subset(text, Speaker== "HARRY")

text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
#text <- text %>% mutate(speaker_stem = stem_strings(Speaker_lemma))
#text <- text %>% mutate(conversation_stem = stem_strings(conversation_lemma))
#Removed stem function as it is replacing "y" with "i" and also deleting "s" in the end  of the word

#text <- text %>% select((speaker_lemma), (conversation_lemma))
#text <- text %>% rename(speaker = speaker_lemma)
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)

text_df <- tibble(text)
#Add line number column
text_df <-  text_df %>% mutate(linenumber = row_number())
#Tokenize
text_df <-  text_df %>% unnest_tokens(words, conversation)
#?unnest_tokens
#unnest_tokens(text_df,text, words)
#text_df <-  text_df %>% tokenize_words(text_df$conversation)
#tidytext::unnest_tokens(read.csv("chennai_reviews.csv", stringsAsFactors = FALSE)


#Remove stop words
#text_df <- text_df %>% anti_join(stop_words, by = "words")

overall_polarity <- get_sentiment(text_df$words, method = "syuzhet", path_to_tagger = NULL,cl = NULL, language = "english", lexicon = NULL)
head(overall_polarity)
summary(overall_polarity)

# bing
overall_bing <- get_sentiment(text_df$words, method="bing")
head(overall_bing)
summary(overall_bing)
#affin
overall_affin <- get_sentiment(text_df$words, method="afinn")
head(overall_affin)
summary(overall_affin)

#compare the first row of each vector using sign function
rbind(
  sign(head(overall_polarity)),
  sign(head(overall_bing)),
  sign(head(overall_affin))
)

d<-get_nrc_sentiment(text_df$words)
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:499]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
qplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
colorbar <- brewer.pal(8, "Set3") 
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  #cex.names = 1
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",col = colorbar
)

#Plot three Positive-Negative
#Plot 3: Stack Plot for positive negative
td_new2<-td_new[9:10,]
td_new3 <- td_new2 %>% mutate(percent = count / sum(count) * 100)

SP <- ggplot(td_new3, aes(fill=sentiment, y=percent, x="")) + 
  geom_bar(position="fill", stat="identity")
SP


#Plot 4: Positive-Negative Horizontal bar
#Change columns to 9:10 to get positive negative
td_new2<-td_new[9:10,]

bp<- ggplot(td_new2, aes(x="", y=count, fill=sentiment))+
  geom_bar(width = 1, stat = "identity")
bp

############################################
############# Ron dialogues ##############
############################################

#Understanding emotions
#Only Harry dialogues
#Extract data
#text <- read.csv("Script.csv", stringsAsFactors = FALSE)
text <- read.csv("Ron.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
#text <- text %>% mutate(speaker_stem = stem_strings(Speaker_lemma))
#text <- text %>% mutate(conversation_stem = stem_strings(conversation_lemma))
#Removed stem function as it is replacing "y" with "i" and also deleting "s" in the end  of the word

#text <- text %>% select((speaker_lemma), (conversation_lemma))
#text <- text %>% rename(speaker = speaker_lemma)
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)

text_df <- tibble(text)
#Add line number column
text_df <-  text_df %>% mutate(linenumber = row_number())
#Tokenize
text_df <-  text_df %>% unnest_tokens(words, conversation)
#?unnest_tokens
#unnest_tokens(text_df,text, words)
#text_df <-  text_df %>% tokenize_words(text_df$conversation)
#tidytext::unnest_tokens(read.csv("chennai_reviews.csv", stringsAsFactors = FALSE)


#Remove stop words
#text_df <- text_df %>% anti_join(stop_words, by = "words")

overall_polarity <- get_sentiment(text_df$words, method = "syuzhet", path_to_tagger = NULL,cl = NULL, language = "english", lexicon = NULL)
head(overall_polarity)
summary(overall_polarity)

# bing
overall_bing <- get_sentiment(text_df$words, method="bing")
head(overall_bing)
summary(overall_bing)
#affin
overall_affin <- get_sentiment(text_df$words, method="afinn")
head(overall_affin)
summary(overall_affin)

#compare the first row of each vector using sign function
rbind(
  sign(head(overall_polarity)),
  sign(head(overall_bing)),
  sign(head(overall_affin))
)

d<-get_nrc_sentiment(text_df$words)
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:340]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
qplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
colorbar <- brewer.pal(8, "Set3") 
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  #cex.names = 1
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",col = colorbar
)

#Plot three Positive-Negative
#Plot 3: Stack Plot for positive negative
td_new2<-td_new[9:10,]
td_new3 <- td_new2 %>% mutate(percent = count / sum(count) * 100)

SP <- ggplot(td_new3, aes(fill=sentiment, y=percent, x="")) + 
  geom_bar(position="fill", stat="identity")
SP


#Plot 4: Positive-Negative Horizontal bar
#Change columns to 9:10 to get positive negative
td_new2<-td_new[9:10,]

bp<- ggplot(td_new2, aes(x="", y=count, fill=sentiment))+
  geom_bar(width = 1, stat = "identity")
bp


############################################
############# Hermione dialogues ##############
############################################

#Understanding emotions
#Only Harry dialogues
#Extract data
#text <- read.csv("Hermione.csv", stringsAsFactors = FALSE)
text <- read.csv("Hermione.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
#text <- text %>% mutate(speaker_stem = stem_strings(Speaker_lemma))
#text <- text %>% mutate(conversation_stem = stem_strings(conversation_lemma))
#Removed stem function as it is replacing "y" with "i" and also deleting "s" in the end  of the word

#text <- text %>% select((speaker_lemma), (conversation_lemma))
#text <- text %>% rename(speaker = speaker_lemma)
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)

text_df <- tibble(text)
#Add line number column
text_df <-  text_df %>% mutate(linenumber = row_number())
#Tokenize
text_df <-  text_df %>% unnest_tokens(words, conversation)
#?unnest_tokens
#unnest_tokens(text_df,text, words)
#text_df <-  text_df %>% tokenize_words(text_df$conversation)
#tidytext::unnest_tokens(read.csv("chennai_reviews.csv", stringsAsFactors = FALSE)


#Remove stop words
#text_df <- text_df %>% anti_join(stop_words, by = "words")

overall_polarity <- get_sentiment(text_df$words, method = "syuzhet", path_to_tagger = NULL,cl = NULL, language = "english", lexicon = NULL)
head(overall_polarity)
summary(overall_polarity)

# bing
overall_bing <- get_sentiment(text_df$words, method="bing")
head(overall_bing)
summary(overall_bing)
#affin
overall_affin <- get_sentiment(text_df$words, method="afinn")
head(overall_affin)
summary(overall_affin)

#compare the first row of each vector using sign function
rbind(
  sign(head(overall_polarity)),
  sign(head(overall_bing)),
  sign(head(overall_affin))
)

d<-get_nrc_sentiment(text_df$words)
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:324]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
qplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
colorbar <- brewer.pal(8, "Set3") 
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  #cex.names = 1
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",col = colorbar
)

#Plot three Positive-Negative
#Plot 3: Stack Plot for positive negative
td_new2<-td_new[9:10,]
td_new3 <- td_new2 %>% mutate(percent = count / sum(count) * 100)

SP <- ggplot(td_new3, aes(fill=sentiment, y=percent, x="")) + 
  geom_bar(position="fill", stat="identity")
SP


#Plot 4: Positive-Negative Horizontal bar
#Change columns to 9:10 to get positive negative
td_new2<-td_new[9:10,]

bp<- ggplot(td_new2, aes(x="", y=count, fill=sentiment))+
  geom_bar(width = 1, stat = "identity")
bp



############################################
############# Cedric dialogues ##############
############################################

#Extract data
text <- read.csv("Cedric_script.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
#text <- text %>% mutate(speaker_stem = stem_strings(Speaker_lemma))
#text <- text %>% mutate(conversation_stem = stem_strings(conversation_lemma))
#Removed stem function as it is replacing "y" with "i" and also deleting "s" in the end  of the word

#text <- text %>% select((speaker_lemma), (conversation_lemma))
#text <- text %>% rename(speaker = speaker_lemma)
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)

text_df <- tibble(text)
#Add line number column
text_df <-  text_df %>% mutate(linenumber = row_number())
#Tokenize
text_df <-  text_df %>% unnest_tokens(words, conversation)
#?unnest_tokens
#unnest_tokens(text_df,text, words)
#text_df <-  text_df %>% tokenize_words(text_df$conversation)
#tidytext::unnest_tokens(read.csv("chennai_reviews.csv", stringsAsFactors = FALSE)


#Remove stop words
#text_df <- text_df %>% anti_join(stop_words, by = "words")

#Syuzhet Polarity
overall_polarity <- get_sentiment(text_df$words, method = "syuzhet", path_to_tagger = NULL,cl = NULL, language = "english", lexicon = NULL)
head(overall_polarity)
summary(overall_polarity)

# bing
overall_bing <- get_sentiment(text_df$words, method="bing")
head(overall_bing)
summary(overall_bing)
#affin
overall_affin <- get_sentiment(text_df$words, method="afinn")
head(overall_affin)
summary(overall_affin)

#compare the first row of each vector using sign function
rbind(
  sign(head(overall_polarity)),
  sign(head(overall_bing)),
  sign(head(overall_affin))
)

d<-get_nrc_sentiment(text_df$words)
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:69]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
qplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
colorbar <- brewer.pal(8, "Set3") 
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  #cex.names = 1
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",col = colorbar
)
#Plot 3: Stack Plot for positive negative
td_new2<-td_new[9:10,]
td_new3 <- td_new2 %>% mutate(percent = count / sum(count) * 100)

SP <- ggplot(td_new3, aes(fill=sentiment, y=percent, x="")) + 
  geom_bar(position="fill", stat="identity")
SP


#Plot 4: Positive-Negative Horizontal bar
#Change columns to 9:10 to get positive negative
td_new2<-td_new[9:10,]

bp<- ggplot(td_new2, aes(x="", y=count, fill=sentiment))+
  geom_bar(width = 1, stat = "identity")
bp

############################################
############# Mad eye dialogues ##############
############################################

#Extract data
text <- read.csv("Madeye_Script.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
#text <- text %>% mutate(speaker_stem = stem_strings(Speaker_lemma))
#text <- text %>% mutate(conversation_stem = stem_strings(conversation_lemma))
#Removed stem function as it is replacing "y" with "i" and also deleting "s" in the end  of the word

#text <- text %>% select((speaker_lemma), (conversation_lemma))
#text <- text %>% rename(speaker = speaker_lemma)
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)

text_df <- tibble(text)
#Add line number column
text_df <-  text_df %>% mutate(linenumber = row_number())
#Tokenize
text_df <-  text_df %>% unnest_tokens(words, conversation)
#?unnest_tokens
#unnest_tokens(text_df,text, words)
#text_df <-  text_df %>% tokenize_words(text_df$conversation)
#tidytext::unnest_tokens(read.csv("chennai_reviews.csv", stringsAsFactors = FALSE)


#Remove stop words
#text_df <- text_df %>% anti_join(stop_words, by = "words")

#Syuzhet polarity
overall_polarity <- get_sentiment(text_df$words, method = "syuzhet", path_to_tagger = NULL,cl = NULL, language = "english", lexicon = NULL)
head(overall_polarity)
summary(overall_polarity)

# bing
overall_bing <- get_sentiment(text_df$words, method="bing")
head(overall_bing)
summary(overall_bing)
#affin
overall_affin <- get_sentiment(text_df$words, method="afinn")
head(overall_affin)
summary(overall_affin)

#compare the first row of each vector using sign function
rbind(
  sign(head(overall_polarity)),
  sign(head(overall_bing)),
  sign(head(overall_affin))
)

d<-get_nrc_sentiment(text_df$words)
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:394]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
qplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
colorbar <- brewer.pal(8, "Set3") 
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  #cex.names = 1
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",col = colorbar
)

#Plot 3: Stack Plot for positive negative
td_new2<-td_new[9:10,]
td_new3 <- td_new2 %>% mutate(percent = count / sum(count) * 100)

SP <- ggplot(td_new3, aes(fill=sentiment, y=percent, x="")) + 
  geom_bar(position="fill", stat="identity")
SP


#Plot 4: Positive-Negative Horizontal bar
#Change columns to 9:10 to get positive negative
td_new2<-td_new[9:10,]

bp<- ggplot(td_new2, aes(x="", y=count, fill=sentiment))+
  geom_bar(width = 1, stat = "identity")
bp


########################################
#########  Experiment   ################
########################################

#Speaker_uni <- unique(text$speaker)
#number <- 
#Speak <- subset.data.frame(Speaker_uni, number)

speakers_unique <- data.frame(Speaker = unique(text$Speaker), Length = length(text$Speaker))
#Harry<- subset(text, Speaker== "HARRY")


#####################################
####  Emotion Comparison   ##########
#####################################


text <- read.csv("Script.csv", stringsAsFactors = FALSE)
#LowerCase
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(listener_lower = tolower(Listener))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
#No Number
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(listener_noNumber = gsub('[[:digit:]]','',listener_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
#No Stopword
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(listener_noStopword = gsub(stopwords_regex,'',listener_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
#No Punctuation
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(listener_noPunc = gsub('[[:punct:]]','',listener_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
#No whiteSpace
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(listener_noWhitespace = gsub('\\s+',' ',listener_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
#Lemmatization
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(listener_lemma = lemmatize_strings(listener_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
#text <- text %>% mutate(speaker_stem = stem_strings(Speaker_lemma))
#text <- text %>% mutate(conversation_stem = stem_strings(conversation_lemma))
#Removed stem function as it is replacing "y" with "i" and also deleting "s" in the end  of the word

#text <- text %>% select((speaker_lemma), (conversation_lemma))
#text <- text %>% rename(speaker = speaker_lemma)
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)

text_df <- tibble(text)
#Add line number column
text_df <-  text_df %>% mutate(linenumber = row_number())

#Tokenize #Install or load package tidytext
text_df <-  text_df %>% unnest_tokens(words, conversation)
d<-get_nrc_sentiment(text_df$words)
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(count = rowSums(td[2:4071])) #check the number of observation and ammend it in the syntax
#Transformation and cleaning
#names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#emotions <- data.frame(td_new2, td_new3$count)
#Plot two - count of words associated with each sentiment, expressed as a percentage
colorbar <- brewer.pal(8, "Set3") 
barplot(
  #sort(colSums(prop.table(d[, 1:8]))), 
  colSums(prop.table(d[, 1:8])),
  horiz = TRUE, 
  #cex.names = 1
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
###   Harry emotions  ##############
####################################
text <- read.csv("Harry_script.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_harry<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_harry))
td_new <- data.frame(count = rowSums(td[2:499])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_harry <- colSums(prop.table(d_harry[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_harry, horiz = TRUE,
  cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
  axes = TRUE,
  las = 1,
  main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

##############
##Ron emotions
##############
text <- read.csv("Ron.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_ron<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_ron))
td_new <- data.frame(count = rowSums(td[2:340])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_ron <- colSums(prop.table(d_ron[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_ron, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)


####################################
## #     Dumbeldore emotions  ######
####################################
text <- read.csv("Dumbeldore.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_dumbeldore<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_dumbeldore))
td_new <- data.frame(count = rowSums(td[2:726])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_dumbeldore <- colSums(prop.table(d_dumbeldore[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_dumbeldore, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
#####   Hermione emotions   ########
####################################
text <- read.csv("Hermione.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_hermione<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_hermione))
td_new <- data.frame(count = rowSums(td[2:324])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_hermione <- colSums(prop.table(d_hermione[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_hermione, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)


####################################
######    Mad Eye emotions  ########
####################################
text <- read.csv("Madeye_script.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_madeye<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_madeye))
td_new <- data.frame(count = rowSums(td[2:394])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_madeye <- colSums(prop.table(d_madeye[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_madeye, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
#####     Barty emotions      ######
####################################
text <- read.csv("Barty.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_barty<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_barty))
td_new <- data.frame(count = rowSums(td[2:180])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_barty <- colSums(prop.table(d_barty[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_barty, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
####       Cedric emotions    ######
####################################
text <- read.csv("Cedric_script.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_cedric<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_cedric))
td_new <- data.frame(count = rowSums(td[2:69])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_cedric <- colSums(prop.table(d_cedric[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_cedric, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
######     Voldemort emotions   ####
####################################
text <- read.csv("Voldemort.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_voldemort<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_voldemort))
td_new <- data.frame(count = rowSums(td[2:218])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_voldemort <- colSums(prop.table(d_voldemort[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_voldemort, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
#####   McGonnagal emotions   ######
####################################
text <- read.csv("Mcgonagall.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_mcgonagall<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_mcgonagall))
td_new <- data.frame(count = rowSums(td[2:175])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_mcgonagall <- colSums(prop.table(d_mcgonagall[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_mcgonagall, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
######      Arthur emotions    ####
####################################
text <- read.csv("Arthur.csv", stringsAsFactors = FALSE)
text <- text %>% mutate(speaker_lower = tolower(Speaker))
text <- text %>% mutate(conversation_lower = tolower(Conversation))
text <- text %>% mutate(speaker_noNumber = gsub('[[:digit:]]','',speaker_lower))
text <- text %>% mutate(conversation_noNumber = gsub('[[:digit:]]','',conversation_lower))
text <- text %>% mutate(speaker_noStopword = gsub(stopwords_regex,'',speaker_noNumber))
text <- text %>% mutate(conversation_noStopword = gsub(stopwords_regex,'',conversation_noNumber))
text <- text %>% mutate(speaker_noPunc = gsub('[[:punct:]]','',speaker_noStopword))
text <- text %>% mutate(conversation_noPunc = gsub('[[:punct:]]','',conversation_noStopword))
text <- text %>% mutate(speaker_noWhitespace = gsub('\\s+',' ',speaker_noPunc))
text <- text %>% mutate(conversation_noWhitespace = gsub('\\s+',' ',conversation_noPunc))
text <- text %>% mutate(speaker_lemma = lemmatize_strings(speaker_noWhitespace))
text <- text %>% mutate(conversation_lemma = lemmatize_strings(conversation_noWhitespace))
text <- text %>% select(conversation_lemma)
text <- text %>% rename(conversation = conversation_lemma)
text_df <- tibble(text)
text_df <-  text_df %>% mutate(linenumber = row_number())
text_df <-  text_df %>% unnest_tokens(words, conversation)
d_arthur<-get_nrc_sentiment(text_df$words)
td<-data.frame(t(d_arthur))
td_new <- data.frame(count = rowSums(td[2:87])) 
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_arthur <- colSums(prop.table(d_arthur[, 1:8]))
colorbar <- brewer.pal(8, "Set3") 
barplot(td_arthur, horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
######  Comparison emotions  ######
####################################

barplot(d_arthur[,1:8], horiz = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        axes = TRUE,
        las = 1,
        main = "Analysis of Emotions", xlab="Percentage",col = colorbar
)

####################################
#### Word Cloud ####################
####################################

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- readLines("script.csv")
text

docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

