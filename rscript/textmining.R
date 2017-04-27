#' @title Text Mining

rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

getwd()
setwd(file.path("TextMining"))
options(scipen=999)

library(jiebaR)
library(text2vec)

## Import related files and cleaning... ####

topic <- "華夏"
source<- "LineQ"
#topic <- "SENIORHIGH"
#source<- "ptt"

list.files(file.path("..", "WebCrawler", "output", topic))
folder <- paste(list.files(file.path("..", "WebCrawler", "output", topic)) , 
                topic, sep = "_")
inputFolderDate <- list.files(file.path("..", "WebCrawler", "output", topic))

dat_articles <- read.csv(paste0(file.path("..", "WebCrawler", "output", topic,
                                          inputFolderDate, paste0(source, "_", topic, "_articles.csv"))), 
                stringsAsFactors = FALSE)
toMatch      <- c("學", "校")
#toMatch      <- "佛光"

if(source=="LineQ"){
  dat_articles <- unique(dat_articles)
  dat_articles <- dat_articles[grepl(paste(toMatch,collapse="|"), dat_articles$Classification), ]
  #dat_articles <- dat_articles[grepl(paste(toMatch,collapse="|"), dat_articles$Content), ]
  dat_replies <- read.csv(paste0(file.path("..", "WebCrawler", "output", topic,
                                           inputFolderDate, paste0(source, "_", topic, "_articles.csv"))), 
                          stringsAsFactors = FALSE)
  dat_replies <- unique(dat_replies)
  dat_replies <- dat_replies[dat_replies$uId %in% dat_articles$uId,]
}else if(source=="ptt"){
  dat_articles <- unique(dat_articles)
  #dat_articles <- dat_articles[grepl(paste(toMatch,collapse="|"), dat_articles$Classification), ]
  dat_articles <- dat_articles[grepl(paste(toMatch,collapse="|"), dat_articles$Content), ]
  dat_replies <- read.csv(paste0(file.path("..", "WebCrawler", "output", topic,
                                           inputFolderDate, paste0(source, "_", topic, "_articles.csv"))), 
                          stringsAsFactors = FALSE)
  dat_replies <- unique(dat_replies)
  dat_replies <- dat_replies[dat_replies$uId %in% dat_articles$uId,]
}


## Combine and mining ?
## Or just remain seperated ?
articles <- c(dat_articles$Content, dat_replies$Replies)
articles <- toupper(articles)
tmp <- articles
articles <- c(articles, tmp)
## END #####

## JiebaR segmentation ####
cutter = worker("tag", bylines = T)

#for(xj in 1:length(word_DB)){
#  new_user_word(cutter,word_DB[xj],"n")
#}

get_noun <- function(x){
  stopifnot(inherits(x,"character"))
  index <- names(x) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")
  x[index]
}

rtn_Jieba = {}
rtn_noun_Jieba = {}

print("Start using cutter...")
rtn_words <- sapply(articles, function(x) cutter <=x)

##Data extraction
data_ep <- function(x){
  a.token <- itoken(x)
  a.vocab <- create_vocabulary(a.token, ngram=c(1, 1))
  class(a.vocab$vocab)
  a.vocab$vocab$terms <- a.vocab$vocab$terms %>% iconv(., "utf8")
  a.vocab$vocab       <- a.vocab$vocab[order(-a.vocab$vocab$terms_counts),]
  
  x_cdf <- a.vocab$vocab
  
  ##Remove words which nchar==1.
  x_cdf <- x_cdf[which(nchar(x_cdf$terms)>1),]
  
  return(x_cdf[order(-x_cdf$doc_counts),])
}

rtnDF <- data_ep(rtn_words)

dir.create(paste0(".\\output\\", folder, "\\AfterJieba"), recursive = TRUE, showWarnings = FALSE)
write.csv(rtnDF, paste0(".\\output\\", folder, "\\AfterJieba\\", topic, "_Jieba.csv"),row.names=F)
## End #####

###
###
###
###



## Clustering terms, using k-means #####

library(text2vec)

# Create iterator over tokens
tokens <- rtn_words#articles
# Create vocabulary. Terms will be unigrams (simple words).
vocab <- create_vocabulary(itoken(tokens))
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
# We provide an iterator to create_vocab_corpus function
it <- itoken(tokens)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
                               # don't vectorize input
                               grow_dtm = FALSE, 
                               # use window of 5 for context words
                               skip_grams_window = 5L)
tcm <- create_tcm(it, vectorizer)
fit <- glove(tcm = tcm,
             word_vectors_size = 50,
             x_max = 10, learning_rate = 0.2,
             num_iters = 15)

word.vec <- fit$word_vectors$w_i + fit$word_vectors$w_j
rownames(word.vec) = rownames(tcm)
#Encoding(rownames(word.vec)) = 'UTF-8'

word.vec.norm = sqrt(rowSums(word.vec ^ 2))

pca = prcomp(word.vec, scale. = T, center = T)
rownames(pca$x) = rownames(word.vec)

#i = which(rownames(pca$x)=="r")
#j = which(rownames(pca$x)=='python')
#plot(pca$x[i[-j] ,2], pca$x[i[-j] ,3], type = 'b', lty = 2, pch = 'x', col = 'red')#, xlim = c(0.8,-2), ylim = c(4,-1))
#text(pca$x[i[-j] ,2], pca$x[i[-j] ,3], labels = rownames(pca$x)[i[-j]], pos = 4)

##here
w <- pca$x
write.csv(w, "tmp.csv")
w <<- fread("tmp.csv")
class(w)
#w <- data.frame(w,stringsAsFactors = F)
names(w)[1] = 'word'
stopifnot(is.character(w$word))
w$word <- iconv(w$word, "UTF-8")
w <- w[nchar(w$word)>1]

i = which(names(w) == 'word')
setDF(w)

k <- kmeans(w[,-i], 100, iter.max = 100) ##10 clusters
## <- kmeans(w[,-i, with = F], 300, iter.max = 100)
w$k = k$cluster
#table(w$k)
w[w$k == w$k[which(w$word=='佛光')],]$word
## End ######


## Association between terms ####

library(tm)
space_tokenizer=function(x){
  unlist(strsplit(as.character(x[[1]]),'[[:space:]]+'))
}
jieba_tokenizer=function(d){
  unlist(segment(d[[1]], cutter))
}
CNCorpus = function(d.vec){
  
  doc<-VCorpus(VectorSource(d.vec ))
  doc<-unlist(tm_map(doc,jieba_tokenizer),recursive=F)
  doc<-lapply(doc,function(d)paste(d,collapse=' '))
  
  Corpus(VectorSource(doc))
}

s.corpus <- CNCorpus(rtn_words)
control.list=list(wordLengths=c(2,Inf),tokenize=space_tokenizer)
s.dtm <- DocumentTermMatrix(s.corpus, control=control.list)
dim(s.dtm)
terms_assoc <- findAssocs(s.dtm, topic, 0.1)

assoc_df <- data.frame("topic" = names(terms_assoc), "terms" =  names(terms_assoc[[1]]), "correlation" = terms_assoc[[1]])
write.csv(assoc_df,  paste0(".\\output\\", folder, "\\AfterJieba\\", topic, "_corr.csv"), row.names = F)
## End #########