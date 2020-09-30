setwd("~/Reto")
#C:\Users\Maria Paula Arbelaez\Documents\Reto

library(jsonlite)
library(lubridate)
library(ggplot2)
library(wordcloud)
library(stringr)
library(tidytext)
library(dplyr)
library(tidyverse)
library(knitr)
library(data.table)
library(tokenizers)
library(tidytext)
library(SnowballC)
library(tm)
library(pluralize)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(fastcluster)
library(dendextend)

library(reshape2)
library(knitr)

####### CARGA DE DATOS #########

your_json <- "News_Category_Dataset_v2.JSON"
unpacked_json <- jsonlite::stream_in(textConnection(readLines(your_json, n=100000)),verbose=F)

########## PRE-PROCESAMIENTO ##########

Data <-  unpacked_json$headline
Data=as.data.frame(Data)
colnames(Data)<- c("headline")
Data$short_description= unpacked_json$short_description

Data$headline=tolower(Data$headline)
Data$short_description=tolower(Data$short_description)

Data$headline <- gsub("[-_#,.'$/:]", "", Data$headline)
Data$short_description <- gsub("[-_#,.'$/:]", "", Data$short_description)
Data$headline <- str_replace_all(Data$headline,"[[:digit:]]", " ") 
Data$short_description <- str_replace_all(Data$short_description,"[[:digit:]]", " ")

Data$short_description <- stripWhitespace(Data$short_description)
Data$headline <- stripWhitespace(Data$headline)

Data$category <-  unpacked_json$category
Data$authors <-  unpacked_json$authors

GrownThrut=as.data.frame(table(Data$category))
colnames(GrownThrut)<- c("Clasificacion")


#Encontrar los autores con más publicaciones
authors<-as.data.frame(table(Data$authors))
authors1= authors %>% filter(authors$Freq>=500)
colnames(authors1)<- c("authors","Freq")

#Filitrar las publicaciones que no tienen autores
authors1 = authors1 %>% filter(!authors== "")
authors1=  authors1[with(authors1, order(-authors1$Freq)), ]
authors2 = merge(authors1,Data, by=c("authors"))


lista_stopwords <- c("me", "my", "myself", "we", "our", "ours", "ourselves",
                     "you","your", "yours", "yourself", "yourselves", "he", "him","his",
                     "himself", "she", "her", "hers", "herself", "it", "its", "itself",
                     "they", "them", "their", "theirs", "themselves", "what", "which",
                     "who", "whom", "this", "that", "these", "those", "am", "is", "are",
                     "was", "were", "be", "been", "being", "have", "has", "had",
                     "having", "do", "does", "did", "doing", "a", "an", "the", "and",
                     "but", "if", "or", "because", "as", "until", "while", "of", "at",
                     "by", "for", "with", "about", "against", "between", "into",
                     "through", "during", "before", "after", "above", "below", "to",
                     "from", "up", "down", "in", "out", "on", "off", "over", "under",
                     "again", "further", "then", "once", "here", "there", "when",
                     "where", "why", "how", "all", "any", "both", "each", "few", "more",
                     "most", "other", "some", "such", "no", "nor", "not", "only", "own",
                     "same", "so", "than", "too", "very", "s", "t", "can", "will",
                     "just", "don", "should", "now", "d", "ll", "m", "o", "re", "ve",
                     "y", "ain", "aren", "couldn", "didn", "doesn", "hadn", "hasn",
                     "haven", "isn", "ma", "mightn", "mustn", "needn", "shan",
                     "shouldn", "wasn", "weren", "won", "wouldn","i","as","us","an","i'm","say","said","hi","like")
lista_stopwords=as.data.frame(lista_stopwords)
colnames(lista_stopwords) <- c("list")

############### AUTHORS ###################
i=1
AKK=0
while (i <= nrow(authors1)){
  
  A= authors2 %>% filter(authors==authors1$authors[i])
  AK1=singularize(unlist(tokenize_words(A$headline)))
  AK2=singularize(unlist(tokenize_words(A$short_description)))
  AKK1<- as.data.frame(matrix(AK1,byrow=TRUE,ncol=1))
  AKK2<- as.data.frame(matrix(AK2,byrow=TRUE,ncol=1))
  AKK[i]= rbind(AKK1, AKK2)
  
  AKK[i]= rbind(AKK1, AKK2)
  
  i=i+1
}

#Obtener datos de los autores - carmbar manuelamente 

FF=as.data.frame(matrix(unlist(AKK[10]),byrow=TRUE,ncol=1))
colnames(FF)<- c("cate")
authorsFINAL1= FF %>% filter(!(FF$cate %in% lista_stopwords$list))
colnames(authorsFINAL1)<- c("authorsFINAL1")

tabla <- table(authorsFINAL1)
tabla <- data_frame(word = names(tabla), count = as.numeric(tabla))
tabla <- arrange(tabla, desc(count))
tabla2= tabla$word

op=Data %>% filter() %>% select( category)
op = Data %>% filter(authors1[10,1]== Data$authors)
op1=as.data.frame(table(op$category))
op1=  op1[with(op1, order(-op1$Freq)), ]

####### Grafícas ######
op1[1:5, ] %>%
  ggplot(aes(x=reorder(Var1,Freq), Freq)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = Freq)) + 
  coord_flip() + 
  labs(title = "Publicaciones",  x = "Categorías", y = "Numero de publicaciones")


wordcloud(
  words = tabla$word, 
  freq = tabla$count, 
  max.words = 50, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)


tabla[1:5, ] %>%
  ggplot(aes(x=reorder(word,count), count)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = count)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes",  x = "Palabras", y = "Número de usos")




tokens <- tabla %>% unnest_tokens(word, word)

Sentimientos=  tokens[1:100,1] %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) 



Emocion=as.character(tokens[1:100,1])
d=0
d<-get_nrc_sentiment(Emocion)
td<-data.frame(t(d))
colnames(td) <- c("count")
td <- arrange(td, -count)
td_new<-td$count[1:3]
td_new = as.data.frame(td_new)
colnames(td_new) <- c("count")
sentiment=rownames(td)
sentiment=sentiment[1:3]

quickplot(sentiment, data=td_new, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Classification")



############### CATEGORY ###################

j=1
BCC=0
while (j <= nrow(GrownThrut)){
  
  B= Data %>% filter(category==GrownThrut$Clasificacion[j])
  BC1=singularize(unlist(tokenize_words(B$headline)))
  BC2=singularize(unlist(tokenize_words(B$short_description)))
  BCC1<- as.data.frame(matrix(BC1,byrow=TRUE,ncol=1))
  BCC2<- as.data.frame(matrix(BC2,byrow=TRUE,ncol=1))
  
  BCC[j]= rbind(BCC1, BCC2)
  #colnames(AKK) <- c(authors3$authors[i])
  
  j=j+1
  
  
}


#Obtener datos de las categorias - cambiar manuelamente 

Category=as.data.frame(matrix(unlist(BCC[8]),byrow=TRUE,ncol=1))
colnames(Category)<- c("Catego")
cateFINAL= Category %>% filter(!(Category$Catego %in% lista_stopwords$list))



tabla <- table(cateFINAL)
tabla <- data_frame(word = names(tabla), count = as.numeric(tabla))
tabla <- arrange(tabla, desc(count))
tabla2= tabla$word


tabla[1:5, ] %>%
  ggplot(aes(x=reorder(word,count), count)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = count)) + 
  coord_flip() + 
  labs(title = "Palabras mas comunes",  x = "palabras", y = "Numero de palabras")


wordcloud(
  words = tabla$word, 
  freq = tabla$count, 
  max.words = 100, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)


tabla[1:5, ] %>%
  ggplot(aes(x=reorder(word,count), count)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = count)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes",  x = "Palabras", y = "Número de usos")




tokens <- tabla %>% unnest_tokens(word, word)
#tokens[1:100,1]
Sentimientos=  tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

Emocion=as.character(tokens)
d=0
d<-get_nrc_sentiment(Emocion)
td<-data.frame(t(d))
colnames(td) <- c("count")
td <- arrange(td, -count)
td_new<-td$count[1:3]
td_new = as.data.frame(td_new)
colnames(td_new) <- c("count")
sentiment=rownames(td)
sentiment=sentiment[1:3]
quickplot(sentiment, data=td_new, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Classification")
