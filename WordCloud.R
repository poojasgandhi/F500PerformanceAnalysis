library(tm) library(ggplot2) library(reshape2) library(wordcloud) library(RWeka)

#To calculate n-grams with weka 
options(mc.cores=1)

#To Load corpus from input files. 
corpus = VCorpus(DirSource("C:/Users/sntil/Documents/R/Project/Analytics/"))

#length(corpus) # Data cleansing - by removing numbers, stopwords and punctuation marks 
corpus.ng = tm_map(corpus,removeWords,c(stopwords(),'services')) 
corpus.ng = tm_map(corpus.ng,removePunctuation) 
corpus.ng = tm_map(corpus.ng,removeNumbers)

# creating a TDM-(TextDocumentMatrix) that uses as terms # the bigrams that appear in the corpus. 
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) 
tdm.bigram = TermDocumentMatrix(corpus.ng,control = list(tokenize = BigramTokenizer))

# Extract the frequency of each bigram 
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE) 
freq.df = data.frame(word=names(freq), freq=freq)

#head(freq.df, 20)
#plotting the word cloud 
wordcloud(freq.df$word,freq.df$freq,scale=c(3,.6),min.freq = 1,max.words=500,random.order = F,res=400,colors = brewer.pal(9,'Dark2'))