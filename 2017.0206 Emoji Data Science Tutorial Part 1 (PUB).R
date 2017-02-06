#### Read the companion walkthrough here: prismoji.com/emoji-data-science-in-r-tutorial ####

# set up Twitter Authentication
library(twitteR)
library(reshape)

###### GRAB TWEETS, PROCESS, AND WRITE TO DISK ######
# authenticate with twitter: get your credentials by creating an app at apps.twitter.com
api_key <- 'XXX'
api_secret <- 'XXX'
access_token <- 'XXX'
access_token_secret <- 'XXX'
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# 2017.0202, #nobannowall
set.seed(20170202); ht <- '#nobannowall'; 
tweets.raw <- searchTwitter(ht, n = 1000, lang = 'en', since = '2017-01-29', until = '2017-01-29');
df <- twListToDF(strip_retweets(tweets.raw, strip_manual = TRUE, strip_mt = TRUE)); df$hashtag <- ht; df$created <- as.POSIXlt(df$created); df$text <- iconv(df$text, 'latin1', 'ASCII', 'byte'); df$url <- paste0('https://twitter.com/', df$screenName, '/status/', df$id); df <- rename(df, c(retweetCount = 'retweets'));
df.a <- subset(df, select = c(text, created, url, latitude, longitude, retweets, hashtag));
nrow(df.a); head(df.a);
setwd('.../PRISMOJI/tutorial/');
write.csv(df.a, paste0('tweets.cleaned_', format(min(df.a$created), '%m%d'), '-', format(max(df.a$created), '%m%d'), '_', ht, '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(df.a), '.csv'), row.names = FALSE);
tweets <- df; tweets$z <- 1; tweets$created <- as.POSIXlt(tweets$created); nrow(tweets); min(tweets$created); max(tweets$created); median(tweets$created);