library("syuzhet")
library("twitteR")
library("ROAuth")
library("tidyverse")
library("ggmap")
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
n = 1000
tweets <- searchTwitter('#MeToo', geocode='20.593684,78.96288,10000km', n=n, lang="en")
tweets_df = do.call("rbind",lapply(tweets,as.data.frame))
tweets_df <- select(tweets_df, text, screenName)
tweets_df$location <- NA
for (user in tweets_df$screenName[1:n]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(tweets_df[tweets_df$screenName==user,]$location <- getUser(user)$location)
}
tweets_df <- cbind(tweets_df, sentiment=(sapply(1:nrow(tweets_df), function(i) sum(get_sentiment(get_sentences(tweets_df[i,1]), "bing")))))
tweets_df <- select(tweets_df, text, location, sentiment)
tweets_df <- tweets_df[tweets_df$location != "" & !is.na(tweets_df$location),]
View(tweets_df)

tweets_df$lat <-NA
tweets_df$lng <-NA
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key=API_KEY)
}

for (name in tweets_df$location[1:n]){ #get the coordinate data for the first 100 tweets via Google Map API.
  rowid<-which(tweets_df$location == name)
  print(paste0("getting the coordinates for:",name,", rowid is:",rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:",geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lngis:", geodata$results[[1]]$geometry$location[[2]]))
    tweets_df[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    tweets_df[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

tweets_df <- tweets_df[!is.na(tweets_df$lat),]

tweets_df$lng = as.numeric(as.character(tweets_df$lng))
tweets_df$lat = as.numeric(as.character(tweets_df$lat))
latitude <- c(6.75351,35.50870)
longitude <- c(68.16238,97.39556)
india_map <- get_map(location = c(lon=mean(longitude), lat=mean(latitude)), zoom=5, maptype="terrain", source="google")
ggmap(india_map) + geom_point(data=tweets_df , aes(x = lng, y = lat), color="red", size=5, alpha=0.5)

tweets_positive <- tweets_df[tweets_df$sentiment > 0,]
tweets_neutral <- tweets_df[tweets_df$sentiment == 0,]
tweets_negative <- tweets_df[tweets_df$sentiment < 0,]
View(tweets_neutral)

ggmap(india_map) + geom_point(data=tweets_positive , aes(x = lng, y = lat), color="green", size=5, alpha=0.5) + geom_point(data=tweets_neutral , aes(x = lng, y = lat), color="blue", size=5, alpha=0.5) + geom_point(data=tweets_negative , aes(x = lng, y = lat), color="red", size=5, alpha=0.5)
