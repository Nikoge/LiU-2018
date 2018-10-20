# TASK 1.1
library("tm")
library("wordcloud")
library("RColorBrewer")


# Read the text file
filePath_five = "Five.txt"
filePath_onetwo = "oneTwo.txt"
data_five = read.table(filePath_five, header=F, sep='\n')
data_onetwo = read.table(filePath_onetwo, header=F, sep='\n')

# set ids
data_five$doc_id = 1:length(data_five)
colnames(data_five)[1] = "text"
data_onetwo$doc_id = 1:nrow(data_onetwo)
colnames(data_onetwo)[1] = "text"

# Create corpus
corpus_five = Corpus(DataframeSource(data_five)) 
corpus_onetwo = Corpus(DataframeSource(data_onetwo)) 

# Remove punctuations
corpus_five = tm_map(corpus_five, removePunctuation)
corpus_onetwo = tm_map(corpus_onetwo, removePunctuation)

# Remove stopwords
corpus_five = tm_map(corpus_five, function(x) removeWords(x, stopwords("english")))
corpus_onetwo = tm_map(corpus_onetwo, function(x) removeWords(x, stopwords("english")))

# Create term doc matrix
tdm_five = TermDocumentMatrix(corpus_five) 
tdm_onetwo = TermDocumentMatrix(corpus_onetwo) 
m_five = as.matrix(tdm_five)
m_onetwo = as.matrix(tdm_onetwo)


# calculate freqs
v_five = sort(rowSums(m_five),decreasing=TRUE) 
v_onetwo = sort(rowSums(m_onetwo),decreasing=TRUE) 

# create dataframe from the freqs and colnames
d_five <- data.frame(word = names(v_five),freq=v_five) 
d_onetwo <- data.frame(word = names(v_onetwo),freq=v_onetwo) 

# create color palette
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1:2)] #Create palette of colors

# wordcloud for five stars
wordcloud(d_five$word, d_five$freq, 
          scale=c(9,.2),
          min.freq=3,
          max.words=120, 
          random.order=F, 
          rot.per=.15, 
          colors=pal, 
          vfont=c("sans serif","plain"))

# wordcloud for one and two stars
wordcloud(d_onetwo$word, d_onetwo$freq, 
          scale=c(9,.2),
          min.freq=3,
          max.words=120, 
          random.order=F, 
          rot.per=.15, 
          colors=pal, 
          vfont=c("sans serif","plain"))


