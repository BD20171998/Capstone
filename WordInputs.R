


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
  
  if(length(cleaned)>4)
  {
    
    cleaned0<- unlist(tail(cleaned,4),recursive = FALSE)
    cleaned1<-paste(cleaned0, collapse = " ")
    pent_gram_to_test<-cleaned1
    
    cleaned<- unlist(tail(cleaned,3),recursive = FALSE)
    cleaned2<-paste(cleaned, collapse = " ")
    quad_gram_to_test<-cleaned2
    
    cleaned3<- unlist(tail(cleaned,2),recursive = FALSE)
    cleaned4<-paste(cleaned3, collapse = " ")
    tri_gram_to_test<-cleaned4
    
    cleaned5<- unlist(tail(cleaned,1),recursive = FALSE)
    cleaned6<-paste(cleaned5, collapse = " ")
    bi_gram_to_test<-cleaned6
    
    words_to_test<-paste(pent_gram_to_test,quad_gram_to_test,tri_gram_to_test,bi_gram_to_test,sep = ",")
    return (words_to_test)
  }
  
  else if(length(cleaned)==4)
  {
    pent_gram_to_test<-cleaned_words
    
    cleaned1<- unlist(tail(cleaned,3),recursive = FALSE)
    cleaned2<-paste(cleaned1, collapse = " ")
    quad_gram_to_test<-cleaned2
    
    cleaned3<- unlist(tail(cleaned,2),recursive = FALSE)
    cleaned4<-paste(cleaned3, collapse = " ")
    tri_gram_to_test<-cleaned4
    
    cleaned5<- unlist(tail(cleaned,1),recursive = FALSE)
    cleaned6<-paste(cleaned5, collapse = " ")
    bi_gram_to_test<-cleaned6
    
    words_to_test<-paste(pent_gram_to_test,quad_gram_to_test,tri_gram_to_test,bi_gram_to_test,sep = ",")
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
    
    words_to_test<-paste(quad_gram_to_test,tri_gram_to_test,bi_gram_to_test,sep = ",")
    return (words_to_test)
  }
  
  else if(length(cleaned)==2)
  {
    tri_gram_to_test<-cleaned_words
    
    cleaned5<- unlist(tail(cleaned,1),recursive = FALSE)
    cleaned6<-paste(cleaned5, collapse = " ")
    bi_gram_to_test<-cleaned6
    
    words_to_test<-paste(tri_gram_to_test,bi_gram_to_test,sep = ",")
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