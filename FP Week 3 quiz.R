https://stackoverflow.com/questions/50635341/removing-stop-words-from-corpus-in-r-is-too-slow
https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html
https://cran.r-project.org/web/packages/ngram/vignettes/ngram-guide.pdf
https://www.youtube.com/watch?v=IFhDlHKRHno
https://www.rdocumentation.org/packages/ANLP/versions/1.3/topics/predict_Backoff
http://www.hlt.utdallas.edu/~sanda/courses/NLP/Lecture06.pdf

https://kenbenoit.net/pdfs/text_analysis_in_R.pdf
http://www.cbs.dtu.dk/courses/27610/regular-expressions-cheat-sheet-v2.pdf
https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf

http://one-line-it.blogspot.com/2013/03/r-javalangoutofmemoryerror-java-heap.html

options(java.parameters = "-Xmx2048m")
library(parallel)
library(doParallel)
library(quanteda)
library(tidyr)
library(plyr)


#Prep for parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

f <- file.path("/Users/user/datasciencecoursera/Course 10 - Capstone/final/en_US/", 
               c("en_US.blogs.txt","en_US.news.txt"))

entire_text<- lapply(f, readLines)
 # herecomesthejackpotquestion inadvance what are you doing new years eve
 # readLines("/Users/user/datasciencecoursera/Course 10 - Capstone/final/en_US/en_US.news.txt")
entire_text<-unlist(entire_text[1:2], recursive = FALSE)

clean_corpus<-function(entiretext){
cleaned_text<-gsub("[^[:alnum:][:space:]']", " ", entiretext)
cleaned_text<-gsub("[[:digit:]]", " ", cleaned_text)
cleaned_text<-gsub(" ' ", " ", cleaned_text)
cleaned_text<-gsub(" s ", " ", cleaned_text)
cleaned_text<-gsub(" th ", " ", cleaned_text)
cleaned_text<-tolower(cleaned_text) 
return(cleaned_text)
}
#news<-readLines("/Users/user/datasciencecoursera/Course 10 - Capstone/final/en_US/en_US.news.txt")
#twitter<-readLines("/Users/user/datasciencecoursera/Course 10 - Capstone/final/en_US/en_US.twitter.txt")

#text_blog<-VCorpus(VectorSource(blog))

#Cleaner<-function(x){
#toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
cleaned_text<-clean_corpus(entire_text)

entire_corpus_quant <- corpus(cleaned_text)

entire_tokens_1<-tokens(entire_corpus_quant,what="word",remove_punct = TRUE,remove_separators = TRUE,remove_hyphens = FALSE,ngrams=1)


entire_tokens_1<- tokens_ngrams(entire_tokens_1,n = 1)
entire_tokens_dfm1<-dfm(entire_tokens_1)
entire_tokens_dfm1<-dfm_trim(entire_tokens_dfm1,min_termfreq = 10)
entire_uni<-data.frame(textstat_frequency(entire_tokens_dfm1))
saveRDS(entire_uni, file = "Uni-Grams.rds")


entire_tokens_2<- tokens_ngrams(entire_tokens_1,n = 2,concatenator = " ")
entire_tokens_dfm2<-dfm(entire_tokens_2)
entire_bi<-data.frame(textstat_frequency(entire_tokens_dfm2))
saveRDS(entire_bi, file = "Bi-Grams.rds")

 
entire_tokens_3<- tokens_ngrams(entire_tokens_1,n = 3,concatenator = " ")
entire_tokens_dfm3<-dfm(entire_tokens_3)
entire_tri<-data.frame(textstat_frequency(entire_tokens_dfm3))
saveRDS(entire_tri, file = "Tri-Grams.rds")

entire_tokens_4<- tokens_ngrams(entire_tokens_1,n = 4,concatenator = " ")
entire_tokens_dfm4<-dfm(entire_tokens_4)
entire_quad<-data.frame(textstat_frequency(entire_tokens_dfm4))
saveRDS(entire_quad, file = "Quad-Grams.rds")

uniGrams<-readRDS("Uni-Grams.rds")
names(uniGrams)[names(uniGrams)=="feature"] <- "y"
uniGrams$group<-"U"
saveRDS(uniGrams,file = "1-Grams.rds")

biGrams<-readRDS("Bi-Grams.rds")
Bi_1 <- data.frame(separate(biGrams,feature, into = c("stem", "y"), sep = " "))
Bi_1$group<-"B"
saveRDS(Bi_1,file = "2-Grams.rds")

triGrams<-readRDS("Tri-Grams.rds")
Tri_1 <- data.frame(separate(triGrams,feature, into = c("x", "y","z"), sep = " "))
Tri_2 <- unite(Tri_1,stem, c("x","y"),  sep = " ")
names(Tri_2)[names(Tri_2)=="z"] <- "y"
Tri_2$group<-"T"
saveRDS(Tri_2,file = "3-Grams.rds")

quadGrams<-readRDS("Quad-Grams.rds")
Quad_1<-data.frame(separate(quadGrams,feature, into = c("w","x", "y","z"), sep = " "))
Quad_2<-unite(Quad_1,stem, c("w","x","y"),  sep = " ")
names(Quad_2)[names(Quad_2)=="z"] <- "y"
Quad_2$group<-"Q"
saveRDS(Quad_2,file = "4-Grams.rds")


all_data<-readRDS("4-Grams.rds")
temp<-readRDS("3-Grams.rds")
all_data<-rbind.fill(all_data,temp)

temp<-readRDS("2-Grams.rds")
all_data<-rbind.fill(all_data,temp)

temp<-readRDS("1-Grams.rds")
all_data<-rbind.fill(all_data,temp)

saveRDS(all_data, file = "All-Data.rds")

word_inputs<-function(words)
{
  
  cleaned_words<-gsub("[^[:alnum:][:space:]']", "", words)
  cleaned_words<-gsub("[[:digit:]]", " ", cleaned_words)
  cleaned_words<-gsub(" ' ", " ", cleaned_words)
  cleaned_words<-gsub(" s ", " ", cleaned_words)
  cleaned_words<-gsub(" th ", " ", cleaned_words)
  cleaned_words<-tolower(cleaned_words)
  
  entire_quant <- corpus(cleaned_words)
  
  clean_string<-texts(entire_quant, spacer = "  ")
  cleaned<- strsplit(clean_string, " ")[[1]]
  
  if(length(cleaned)>3)
  {
    cleaned<- unlist(tail(cleaned,3),recursive = FALSE)
    cleaned2<-paste(cleaned, collapse = " ")
    quad_gram_to_test<-cleaned2
    
    cleaned3<- unlist(tail(cleaned,2),recursive = FALSE)
    cleaned4<-paste(cleaned3, collapse = " ")
    tri_gram_to_test<-cleaned4
    
    cleaned5<- unlist(tail(cleaned,1),recursive = FALSE)
    cleaned6<-paste(cleaned5, collapse = " ")
    bi_gram_to_test<-cleaned6
    
    words_to_test<-list(quad_gram_to_test,tri_gram_to_test,bi_gram_to_test)
    return (words_to_test)
  }
  
  else if(length(cleaned)==3)
  {
    quad_gram_to_test<-cleaned_words
    
    cleaned3<- unlist(tail(cleaned,2),recursive = FALSE)
    cleaned4<-paste(cleaned3, collapse = " ")
    tri_gram_to_test<-cleaned4
    
    cleaned5<- unlist(tail(cleaned,1),recursive = FALSE)
    cleaned6<-paste(cleaned5, collapse = " ")
    bi_gram_to_test<-cleaned6
    
    words_to_test<-list(quad_gram_to_test,tri_gram_to_test,bi_gram_to_test)
    return (words_to_test)
  }
  
  else if(length(cleaned)==2)
  {
    tri_gram_to_test<-cleaned_words
    
    cleaned5<- unlist(tail(cleaned,1),recursive = FALSE)
    cleaned6<-paste(cleaned5, collapse = " ")
    bi_gram_to_test<-cleaned6
    
    words_to_test<-list(tri_gram_to_test,bi_gram_to_test)
    return (words_to_test)
  }
  
  else if(length(cleaned)==1)
  {
    uni_gram_to_test<-cleaned_words
    return (uni_gram_to_test)
  }
  else
    return ("Please enter some text")
  
}
 
most_likely<-function(thelist,alldata)
{
  if(length(thelist)==3){
    #piece<-paste(thelist[[1]],sep = " ",collapse = " ")
    index1<-which(thelist[[1]]==alldata$stem & max(alldata$docfreq) & alldata$group=="Q")
    
    if( is.na(index1)||length(index1)==0){
      index2<-which(thelist[[2]]==alldata$stem & max(alldata$docfreq) & alldata$group=="T")
      
      if( is.na(index2)||length(index2)==0){
        index3<-which(thelist[[3]]==alldata$stem & max(alldata$docfreq) & alldata$group=="B")
        
        if(is.na(index3)||length(index3)==0){
          index4<-which(max(alldata$docfreq) & alldata$group=="U")
          predicted_word<-alldata[index4,2]
          return(predicted_word)}
        
        predicted_word<-alldata[index3,2]
        return(predicted_word)}
      
      predicted_word<-alldata[index2,2]
      return(predicted_word)} 
    
    predicted_word<-alldata[index1,2]
    return(predicted_word)} else if (length(thelist)==2){
      #piece<-paste(thelist[[2]],sep = " ",collapse = " ")
      index1<-which(thelist[[2]]==alldata$stem & max(alldata$docfreq) & alldata$group=="T")
      
      if( is.na(index1)||length(index1)==0){
        index2<-which(thelist[[3]]==alldata$stem & max(alldata$docfreq) & alldata$group=="B")
        
        if( is.na(index2)||length(index2)==0){
          index3<-which(max(alldata$docfreq) & alldata$group=="U")
          predicted_word<-alldata[index3,2]
          return(predicted_word)}
        
        predicted_word<-alldata[index2,2]
        return(predicted_word)}
      
      predicted_word<-alldata[index1,2]
      return(predicted_word)} else if (length(thelist)==1){
        index2<-which(thelist[[3]]==alldata$stem & max(alldata$docfreq) & alldata$group=="B")
        
        if( is.na(index2)||length(index2)==0){
          index3<-which(max(alldata$docfreq) & alldata$group=="U")
          predicted_word<-alldata[index3,2]
          return(predicted_word)}
        
        predicted_word<-alldata[index2,2]
        return(predicted_word)}else {return("Nothing entered")}}

the_list<-word_inputs("")
most_likely(the_list,all_data)







##Setting TrainControl for models to be used
fitControl <- trainControl(method = "cv",number = 10,allowParallel= TRUE)

inBuild<-createDataPartition(y=all_data$y,p=0.7,list = FALSE)
validation<-all_data[-inBuild,]
buildData<-all_data[inBuild,]

inTrain<-createDataPartition(y=buildData$y,p=0.7,list = FALSE)
training<-buildData[inTrain,]
testing<-buildData[-inTrain,]

fit.rweka <- train(y~., data=training, method="J48",trControl=fitControl)
#resultJ48 <- J48(y~., training)
##Setting TrainControl for models to be used
## Use 10 fold cross-validation.
e <- evaluate_Weka_classifier(m,
                              cost = matrix(c(0,2,1,0), ncol = 2),
                              numFolds = 10, complexity = TRUE,
                              seed = 123, class = TRUE)
#Error in .jcall(att, "I", "addStringValue", k) : 
#Java Exception <no description because toString() failed>.jcall(att, "I", "addStringValue", k)<S4 object of class "jobjRef">

stopCluster()