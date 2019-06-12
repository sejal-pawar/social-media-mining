library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library("twitteR")
library("ROAuth")
download.file(url= "http://curl.haxx.se/ca/cacert.pem", destfile= "cacert.pem")
credentials <- OAuthFactory$new(consumerKey=CONSUMER_KEY,
                                consumerSecret=CONSUMER_SECRET,
                                requestURL='https://api.twitter.com/oauth/request_token',
                                accessURL='https://api.twitter.com/oauth/access_token',
                                authURL='https://api.twitter.com/oauth/authorize')
credentials$handshake(cainfo="cacert.pem")
save(credentials, file="twitter_authentication.Rdata")
load("twitter_authentication.Rdata")
setup_twitter_oauth(credentials$consumerKey, credentials$consumerSecret, credentials$oauthKey, credentials$oauthSecret)
tweets <- searchTwitter('#GST', n=1000, lang="en")

GetSentiment <- function(){
  
  # tokenize
  tokens <- twListToDF(tweets) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative)
  
  # return our sentiment dataframe
  return(sentiment)
  
}
GetSentiment()
