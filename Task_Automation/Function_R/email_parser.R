

dir <- "C:/Users/Adam/Documents/Adam's File/email_test/"


###########################################
library(dplyr)
library(data.table)
library(stringr)
## Choose a dir ##


  listfiles <- list.files(path = dir, pattern=".msg")
  
  message.files <-  data_frame(filename = listfiles)
  
  message.files <- message.files  %>%
    mutate(filepath = paste0(dir, filename))
  
  list <- message.files$filepath
  rm(listfiles, message.files)
  

tableofemails  <- c()   

for(i  in 1:length(list)) {
  
  onerow.emailparser <- function(email_list_object) {
    
    sender <- email_list_object$sender$sender_name
    
    subject <- email_list_object$subject
    
    date <- email_list_object$headers$Date
    
    ##listdf <- do.call(rbind.data.frame, em)
    
    ##emaildf <- do.call(rbind.data.frame, email_1$body)
    
    Un1 <- unique(email_list_object$body$text)
    text <- data.frame(Text=Un1, Numchar=nchar(Un1)) %>%
      filter(Numchar > 2) 
    
    data <-cbind(text,sender) 
    
    data <- cbind(data,subject)
    
    data <- cbind(data, date)
    
    return(data)
  }
  
  library(msgxtractr)
  
  filepath <- list[i]
  
  email_list_object <-  msgxtractr::read_msg(filepath)
  
  data <- onerow.emailparser(email_list_object)
  
  tableofemails<- rbind(tableofemails,
                        data)
  
  cleanText = function(col){
    
    #Remove non-ASCII characters
    Encoding(col) = "latin1"
    iconv(col, "latin1", "ASCII", sub = "")
    # Removes html links
    col = gsub("http\\w+", "", col)
    # Removes unnecessary spaces
    col = gsub("[ \t]{2,}", "", col)
    col = gsub("^\\s+|\\s+$", "", col)
    # Fix for error related to formatting 'utf8towcs'"
    col <- str_replace_all(col,"[^[:graph:]]", " ")
    return(col)
    
    
  }
  
  
  tableofemails$Text <- cleanText(tableofemails$Text)
  
  
  rm(data, email_list_object)
  
  
  
  
}

library(parsedate)

tableofemails$date <-  parse_date(tableofemails$date)


get_sentiment_prediction <- function(x) {
  library(sentimentr)
  x <- sentimentr::get_sentences(x)
  x <- sentiment(x) %>%
    mutate(polarity_level = ifelse(sentiment == 0, "Neutral",
                                   ifelse(sentiment > 0.2, "Positive","Negative")))
}



sentiment <- get_sentiment_prediction(tableofemails)

#computed weighted mean by a particular unit (same as "proc means data=____; by ___; weight ____; run;" in SAS)#
weighted.avg.by.element <- sapply(split(sentiment,sentiment$element_id), #divide the data by month (or individual or whatever)
                                function(sentiment) weighted.mean(sentiment$sentiment,w=sentiment$sentiment))


sentimenter <- as.data.frame(weighted.avg.by.element)

test <- merge(tableofemails, sentimenter, all.x = TRUE)

#
#checking:

  
  
  

##install.packages("parsedate")

##use length ?? ##

write.csv(tableofemails, paste0(dir,"emaildataset"))

