
# Similarity function of textRank
simil_textRank <- function(x,y){
  # b is the intersection of common words, k and p are the length of two sentences  
  b <-Reduce(intersect, list(x,y))
  c<- length(b)-1
  m <- 0
  n <- 0
  k <- length(x)
  p <- length(y)
  
  for(i in 1:k){
    if(x[i]!=""){
      m <- m+1
    }
  }
  for(j in 1:p){
    if(y[j]!=""){
      n <- n+1
    }
  }
  func <- c / (log(m)+log(n))
  return(func)
  
}


#MAIN FRAME
textRank<-function(){
  
  library(NLP)
  library(tm)
  library(openNLP)
  
  doc <- readLines("text.txt")
  doc <- as.String(doc)
  
  # Word and sentence token annotator 
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  
  doc_annotations <- annotate(doc, list(sent_ann, word_ann))
  bio_doc <- AnnotatedPlainTextDocument(doc, doc_annotations)
  
  # Sentence boundaries in text
  sentence.boundaries<-annotate(doc,list(sent_ann))
  # Extract sentences
  sentences<-doc[sentence.boundaries]
  
  # Pre-processing
  corp <- Corpus(VectorSource(sents(bio_doc)))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removeWords, stopwords("english"))
  corp <- tm_map(corp, stemDocument)
  
  j<-length(sents(bio_doc))
  corp_list<-list()
  
  for (i in 1:j){
    corp_list[[length(corp_list)+1]]<-corp[[i]]
  }
  
 
  p <- length(sents(bio_doc))
  m <- matrix(NA, nrow=p, ncol=p)
  simil_trank <- as.data.frame(m)
  

  for (i in 1:p){
    for(j in 1:p){
      simil_trank[i,j]= simil_textRank(corp_list[[i]],corp_list[[j]])
    }
  }
  
  # Calculate pagerank algorithm 
  M = t(simil_trank / rowSums(simil_trank))
  n = nrow(M)
  U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
  beta=0.85
  A = beta*M+(1-beta)*U
  A[is.na(A)] <- 0
  A[is.infinite(A)] <- 0
  e = eigen(A)
  v <- e$vec[,1]
  v <- as.numeric(v) / sum(as.numeric(v))
  
  #Find N most highest value location in a vector and its value
  highest<- order(v, decreasing = T)[1:10]
  v[highest]
  
  # Create list from highest eigenvalue sentences
  summ_list<- list()
  m<-length(highest)
  
  for(i in 1:m){
    x<-highest[i] 
    summ_list[[length(summ_list)+1]]<- sentences[x]
  }
  
  d<-lapply(summ_list, cat,"", file="summary_textRank.txt", append=TRUE)


}
