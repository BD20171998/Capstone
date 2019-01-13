all_data<-readRDS("All-Data (trimmed).rds")

most_likely_word<-function(thelist,alldata)
{
  unknown_word<-"???"
  
  if(length(thelist)==4){
    
    qry1 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[1]])    
    predicted_word_P<-sqldf(qry1)
    
    qry2 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[2]])    
    predicted_word_Q<-sqldf(qry2)
    
    qry3 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[3]])    
    predicted_word_T<-sqldf(qry3)
    
    qry4 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[4]])    
    predicted_word_B<-sqldf(qry4)
    
    predicted<-ifelse(nrow(predicted_word_P)!=0 && is.data.frame(predicted_word_P),predicted_word_P,
                      ifelse(nrow(predicted_word_Q)!=0 && is.data.frame(predicted_word_Q),predicted_word_Q,
                             ifelse(nrow(predicted_word_T)!=0 && is.data.frame(predicted_word_T),predicted_word_T,
                                    ifelse(nrow(predicted_word_B)!=0 && is.data.frame(predicted_word_B),predicted_word_B,
                                           unknown_word))))
    
    return(predicted)}else if(length(thelist)==3){
      qry1 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[1]])    
      predicted_word_Q<-sqldf(qry1)
      
      qry2 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[2]])    
      predicted_word_T<-sqldf(qry2)
      
      qry3 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[3]])    
      predicted_word_B<-sqldf(qry3)
      
      predicted<-ifelse(nrow(predicted_word_Q)!=0 && is.data.frame(predicted_word_Q),predicted_word_Q,
                               ifelse(nrow(predicted_word_T)!=0 && is.data.frame(predicted_word_T),predicted_word_T,
                                      ifelse(nrow(predicted_word_B)!=0 && is.data.frame(predicted_word_B),predicted_word_B,
                                             unknown_word)))
      
      return(predicted)}else if(length(thelist)==2){
        qry1 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[1]])    
        predicted_word_T<-sqldf(qry1)
        
        qry2 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[2]])    
        predicted_word_B<-sqldf(qry2)
        
        predicted<-ifelse(nrow(predicted_word_T)!=0 && is.data.frame(predicted_word_T),predicted_word_T,
                      ifelse(nrow(predicted_word_B)!=0 && is.data.frame(predicted_word_B),predicted_word_B,
                                        unknown_word))
  
        return(predicted)} else if (length(thelist)==1){
        
          qry1 = sprintf("select y from all_data WHERE stem LIKE '%s' ORDER BY docfreq DESC LIMIT 1 ", thelist[[1]])    
          predicted_word_B<-sqldf(qry1)
          predicted<-ifelse(nrow(predicted_word_B)!=0 && is.data.frame(predicted_word_B),predicted_word_B,
            unknown_word)
            return(predicted)} else{ return(unknown_word)}}