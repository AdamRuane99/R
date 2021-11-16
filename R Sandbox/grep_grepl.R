##grepl 
#grep 


text <- "Hi There do you know who you are voting for ?"
grepl('voting', text)
##returns true if in text 
grepl('dog',text)


v <- c('a', 'b', 'c', 'd', 'd')
v


grepl('b', v)

grep('b', v)
