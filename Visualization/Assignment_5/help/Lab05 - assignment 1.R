#-------------- Loading packages and reading data
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)

#---------- 1.1
#-------- Creating data.frame source to be used as the input for corpus function
Five <- read.table(file = 'Five.txt',header = FALSE, sep = '\n')
Five$doc_id <- 1:nrow(Five)
colnames(Five)[1] = 'text'
Five_source <- DataframeSource(x = Five[,c(2,1)])

OneTwo <- read.table(file = 'OneTwo.txt',header = FALSE, sep = '\n')
OneTwo$doc_id <- 1:nrow(OneTwo)
colnames(OneTwo)[1] = 'text'
OneTwo_source <- DataframeSource(x = OneTwo[,c(2,1)])

#-------- Creating corpus
Five_corpus <- SimpleCorpus(x = Five_source)
OneTwo_corpus <- SimpleCorpus(x = OneTwo_source)

#-------- Transformation
#-- Five
Tran_Five_corpus <- tm_map(x = Five_corpus,FUN = removePunctuation)
Tran_Five_corpus <- tm_map(x = Five_corpus,FUN = removeWords, stopwords('english'))
Tran_Five_corpus <- tm_map(x = Five_corpus,FUN = stripWhitespace)
Tran_Five_corpus <- tm_map(x = Five_corpus,FUN = removeNumbers)
#-- OneTwo
Tran_OneTwo_corpus <- tm_map(x = OneTwo_corpus,FUN = removePunctuation)
Tran_OneTwo_corpus <- tm_map(x = OneTwo_corpus,FUN = removeWords, stopwords('english'))
Tran_OneTwo_corpus <- tm_map(x = OneTwo_corpus,FUN = stripWhitespace)
Tran_OneTwo_corpus <- tm_map(x = OneTwo_corpus,FUN = removeNumbers)

#-------- Creating Term Document Matrix
Five_Term_Matrix <-  as.matrix(TermDocumentMatrix(Tran_Five_corpus))
OneTwo_Term_Matrix <- as.matrix(TermDocumentMatrix(Tran_OneTwo_corpus))

#-------- Sorting Data and Creating data frame for word cloud
sorted_Five <- sort(rowSums(Five_Term_Matrix), decreasing = TRUE)
s_df_Five <- data.frame(word = names(sorted_Five), freq = sorted_Five)

sorted_OneTwo <- sort(rowSums(OneTwo_Term_Matrix), decreasing = TRUE)
s_df_OneTwo <- data.frame(word = names(sorted_OneTwo), freq = sorted_OneTwo)

#--------- Creating color pallet
pallet <- brewer.pal(9,'Set1')
pallet <- pallet[9:1]

#--------- WordCloud
#-- Five
wordcloud(words = s_df_Five$word, freq = s_df_Five$freq
          , min.freq = 3, max.words = 100, random.order = FALSE
          , random.color = FALSE, colors = pallet, scale = c(7,0.5))

#-- OneTwo
wordcloud(words = s_df_OneTwo$word, freq = s_df_OneTwo$freq
          , min.freq = 3, max.words = 100, random.order = FALSE
          , random.color = FALSE, colors = pallet, scale = c(7,0.5))

#------------ 1.2

























































































































































































