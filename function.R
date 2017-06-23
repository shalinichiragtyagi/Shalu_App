library(tm)

MAX_N_GRAMS = 3
TOP_FREQ = 5

# Load data files to run the app
load("unigram.Rda")
load("bigram.Rda")
load("trigram.Rda")
load("fourgram.Rda")

msg <- ""

# Functions to clean input parameter
removeWhiteSpaces <- function(str) {
  return( gsub("\\s+"," ",str) )
}
removeProfanity <- function(str) {
  dfProf <- read.csv("file:///C:/Users/Shali/Documents/courseradata/full-list-of-bad-words-banned-by-google-txt-file/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt",sep = "\n",header = FALSE, stringsAsFactors = FALSE)
  nWords <- nrow(dfProf)
  for(i in 1:nWords) 
    str <- gsub(dfProf[i,1],"*",str, perl=TRUE) 
  return(str)
}


clean <- function(input) {
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  input <- removeWhiteSpaces(input)
  input <- tolower(input) 
  input <- removeProfanity(input)
}

# Example of how function works:
# get_last_words( "If I only want the last three words it returns this", 3 ) ==> "it returns this"
get_last_words <- function(lst, n) {
  x <- as.data.frame(strsplit(lst, " "))
  colnames(x) <- c("words")
  len <- nrow(x)
  if(len<n) n<-len 
  lst <- paste(x[ (len-(n-1)) : len ,], collapse=" ")
  return(lst)
}

# Heuristcs applyed before returning to the server interface
contractions <- function(token) {
  token <- gsub("^s\\s","\'s ",token)
  token <- gsub("^n\\s","\'n ",token)
  token <- gsub("^d\\s","\'d ",token)
  token <- gsub("^t\\s","\'t ",token)
  token <- gsub("^ve\\s","\'ve ",token)
  token <- gsub("^ll\\s","\'ll ",token)
  token <- gsub("^re\\s","\'re ",token)
  return(token)
}

# Returns the most frequent words from unigram table in a vector
get_top <- function(n=TOP_FREQ) {
  load("unigram.Rda")
  nrows <- nrow(unigram.df)
  vTop <- vector("list", length=25)
  for(i in seq_len(25))
    vTop[i] <- as.character(unigram.df[i,1])
  return(head(vTop,n))
}

# Function to find the most probable word in a n-gram dataframe
find_in_grams <- function(lastW, n) {
  lastW <- paste0('^',lastW,' ')
  
  # subset 'n-gram' dataframe to find the most probable occurrences 
  if(n==3)
    dfsub <- subset(fourgram.df, grepl(lastW, fourgram.df$word))
  else
    if(n==2)
      dfsub <- subset(trigram.df, grepl(lastW, trigram.df$word))
    else
      if(n==1)
        dfsub <- subset(bigram.df, grepl(lastW, bigram.df$word))
      
      # If there are matches, return the 5 most frequent words
      if(nrow(dfsub) > 0) {
        top5words <- head(contractions(gsub(lastW,"",dfsub$word)),TOP_FREQ)
        msg <<- sprintf("Next word was predicted with %1d-gram dataframe.",(n+1))
        return( gsub("[[:space:]]"," ",top5words) )
      }
      else{
        n <- n - 1;
        if(n > 0) { 
          lastW <- substr(lastW,2,nchar(lastW))
          find_in_grams( get_last_words(lastW,2), n )
        }
        else {
          lastW <- substr(lastW,2,nchar(lastW))
          msg <<- paste("Next word not found in 2, 3 or 4-grams dataframes.\nReturning the",TOP_FREQ,"most frequent words of uni-gram.")
          return(get_top(TOP_FREQ))
        }
      }
}

predict_model <- function(user_input) {
  return( find_in_grams( get_last_words(user_input, MAX_N_GRAMS), n=MAX_N_GRAMS) )
}