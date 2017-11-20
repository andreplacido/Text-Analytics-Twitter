
###################################################
# Criando Analises com dados do Twitter
###################################################

#Carregando as bibliotecas
library("ROAuth");
library("base64enc");
library("twitteR");
library("streamR");

#Carregando os parametros de configuração  
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
options(httr_oauth_cache=T)


#Carregando as credenciais
consumer_key <- "consumer_key"
consumer_secret <-"consumer_secret"
access_token <-"access_token"
access_secret <-"access_secret"

## Twitter authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token,
                    access_secret)

#Carregando as bibliotecas
library("ROAuth");
library("twitteR");

tweets <- userTimeline("g1", n = 3200)

#Confirmando a quantidade de Tweets
n.tweet <- length(tweets)

#Transformando em Data Frame
tweets.df <- twListToDF(tweets)

#exportando dados para o Excel
#install.packages("rJava")
#install.packages("xlsx")
library(rJava)
library(xlsx)

write.xlsx(tweets.df, file="D://Td2i//Td2i//Projetos//TextAnalysis//tl2.g1.xlsx")

#####################################################
# Text Cleansing
#####################################################

#Retirando acentuacao das palavras
library(stringi)
dados_str<-stri_trans_general(tweets.df$text, "Latin-ASCII")



library(tm)
myCorpus <- VCorpus(VectorSource(dados_str))

myCorpus <- tm_map(myCorpus, content_transformer(tolower) )
myCorpus <- tm_map(myCorpus, function(x) gsub('http[[:alnum:]]*','',x)) #removendo URLs
myCorpus <- tm_map(myCorpus, function(x) gsub("[^[:alpha:][:space:]]*", "", x))
myCorpus <- tm_map(myCorpus, removeNumbers) #removendo números
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeWords, c(stopwords("portuguese")))
# incluindo minhas stopwords
myStopwords <- c("rockinrio", "que", "ainda", "pra", "ficar" , "porque", "faz", "como","tcoedfdfwtyt",
                 "<U+FFFD><U+FFFD><U+FFFD><U+FFFD>", "攼㹤愼㸰戼㹤攼㹤戼㸲㠼㹢攼㹤愼㸰戼㹤攼㹤戼㸲㠼㹢")
myCorpus <- tm_map(myCorpus,removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, stripWhitespace)

myCorpusCopy <- myCorpus

##########################################
#Matriz para a contagem de termos
##########################################
corpus_text <- tm_map(myCorpus, PlainTextDocument)
tdm <- TermDocumentMatrix(corpus_text, control = list(minWordLength = 1 ))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=30) #filtra a quantidade de palavras pela qtd de frequencia

df <- data.frame(term = names(term.freq), freq = term.freq)

###########################################
# Visualização Gráfico de frequencia
##########################################

library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

###############################################
# Word Cloud
###############################################

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

#################################################
# NEtwork of Terms
################################################

freq.term <-findFreqTerms(tdm, lowfreq = 50) 

library(graph)
library(Rgraphviz)
plot(tdm, term = freq.term, corThreshold = 0.1)



