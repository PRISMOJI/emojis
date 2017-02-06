#### Read the companion walkthrough here: prismoji.com/emoji-data-science-in-r-tutorial ####

library(plyr)
library(ggplot2)
library(splitstackshape)
library(stringr)

####### READ IN SAVED TWITTER DATA
setwd('.../PRISMOJI/tutorial/');
fnames <- c(
  'tutorial_tweets_raw'
);
fnames <- paste0(fnames, '.csv'); df <- do.call(rbind.fill, lapply(fnames, read.csv));
df$username <- substr(substr(df$url, 21, nchar(as.character(df$url))), 1, nchar(substr(df$url, 21, nchar(as.character(df$url))))-26);
tweets.full <- df; tweets.full$X <- NULL; tweets.full$z <- 1; 
#### sanity checking
tweets.full$created <- as.POSIXlt(tweets.full$created); min(tweets.full$created); max(tweets.full$created); median(tweets.full$created); nrow(tweets.full); length(unique(tweets.full$username))
## dedupe dataset by url
tweets.dupes <- tweets.full[duplicated(tweets.full$url), ]; nrow(tweets.full); nrow(tweets.dupes); # test <- subset(tweets.full, url %in% tweets.dupes$url); test <- test[with(test, order(url)), ];
tweets <- tweets.full[!duplicated(tweets.full$url), ]; tweets <- arrange(tweets, url); row.names(tweets) <- NULL; tweets$tweetid <- as.numeric(row.names(tweets)); nrow(tweets);
tweets.final <- tweets;
## dedupe dataset by username
# tweets.dupes <- tweets.full[duplicated(tweets.full$username), ]; nrow(tweets.full); nrow(tweets.dupes); # test <- subset(tweets, url %in% tweets.dupes$url); test <- test[with(test, order(url)), ];
# tweets <- tweets.full[!duplicated(tweets.full$username), ]; tweets <- arrange(tweets, url); row.names(tweets) <- NULL; tweets$tweetid <- as.numeric(row.names(tweets)); nrow(tweets);

#### READ IN EMOJI DICTIONARIES
setwd('.../PRISMOJI/tutorial/');
emdict.la <- read.csv('emoticon_conversion_noGraphic.csv', header = F); #Lauren Ancona; https://github.com/laurenancona/twimoji/tree/master/twitterEmojiProject
emdict.la <- emdict.la[-1, ]; row.names(emdict.la) <- NULL; names(emdict.la) <- c('unicode', 'bytes', 'name'); emdict.la$emojiid <- row.names(emdict.la);
emdict.jpb <- read.csv('emDict.csv', header = F) #Jessica Peterka-Bonetta; http://opiateforthemass.es/articles/emoticons-in-R/
emdict.jpb <- emdict.jpb[-1, ]; row.names(emdict.jpb) <- NULL; names(emdict.jpb) <- c('name', 'bytes', 'rencoding'); emdict.jpb$name <- tolower(emdict.jpb$name);
emdict.jpb$bytes <- NULL;
## merge dictionaries
emojis <- merge(emdict.la, emdict.jpb, by = 'name');  emojis$emojiid <- as.numeric(emojis$emojiid); emojis <- arrange(emojis, emojiid);

###### FIND TOP EMOJIS FOR A GIVEN SUBSET OF THE DATA
tweets <- tweets.final;
# tweets <- subset(tweets.final, hashtag %in% c('#womensmarch'));
## create full tweets by emojis matrix
df.s <- matrix(NA, nrow = nrow(tweets), ncol = ncol(emojis)); 
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets$text, ignore.case = T, useBytes = T));
rownames(df.s) <- 1:nrow(df.s); colnames(df.s) <- 1:ncol(df.s); df.t <- data.frame(df.s); df.t$tweetid <- tweets$tweetid;
# merge in hashtag data from original tweets dataset
df.a <- subset(tweets, select = c(tweetid, hashtag)); 
df.u <- merge(df.t, df.a, by = 'tweetid'); df.u$z <- 1; df.u <- arrange(df.u, tweetid); 
tweets.emojis.matrix <- df.u;
## create emoji count dataset
df <- subset(tweets.emojis.matrix)[, c(2:843)]; count <- colSums(df > -1);
emojis.m <- cbind(count, emojis); emojis.m <- arrange(emojis.m, desc(count));
emojis.count <- subset(emojis.m, count > 1); emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets)), 1); emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets) + 1);
emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));
# print summary stats
subset(emojis.count.p, rank <= 10);
num.tweets <- nrow(tweets); df.t <- rowSums(tweets.emojis.matrix[, c(2:843)] > -1); num.tweets.with.emojis <- length(df.t[df.t > 0]); num.emojis <- sum(emojis.count$count);
min(tweets$created); max(tweets$created); median(tweets$created);
num.tweets; num.tweets.with.emojis; round(100 * (num.tweets.with.emojis / num.tweets), 1); num.emojis; nrow(emojis.count);

##### MAKE BAR CHART OF TOP EMOJIS IN NEW DATASET
df.plot <- subset(emojis.count.p, rank <= 10); xlab <- 'Rank'; ylab <- 'Overall Frequency (per 1,000 Tweets)';
setwd('.../PRISMOJI/tutorial/ios_9_3_emoji_files');
df.plot <- arrange(df.plot, name);
imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG); g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); df.plot$xsize <- k; df.plot$ysize <- k; #df.plot$xsize <- k * (df.plot$dens / max(df.plot$dens)); df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens));
df.plot <- arrange(df.plot, name);
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g1;
setwd('.../PRISMOJI/tutorial/');
png(paste0('emoji_barchart_', as.Date(min(tweets$created)), '_', as.Date(max(tweets$created)), '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(tweets), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g1; dev.off();

##### CREATE MASTER DATASET OF ORIGINAL TWEETS appended with array of emojis
## EMOJIS: create reduced tweets+emojis matrix
df.s <- data.frame(matrix(NA, nrow = nrow(tweets), ncol = 2)); names(df.s) <- c('tweetid', 'emoji.ids'); df.s$tweetid <- 1:nrow(tweets);
system.time(df.s$emoji.ids <- apply(tweets.emojis.matrix[, c(2:843)], 1, function(x) paste(which(x > -1), sep = '', collapse = ', '))); 
system.time(df.s$num.emojis <- sapply(df.s$emoji.ids, function(x) length(unlist(strsplit(x, ', '))))); 
df.s.emojis <- subset(df.s, num.emojis > 0);
df.s.nonemojis <- subset(df.s, num.emojis == 0); df.s.nonemojis$emoji.names <- '';
# convert to long, only for nonzero entries
df.l <- cSplit(df.s.emojis, splitCols = 'emoji.ids', sep = ', ', direction = 'long')
map <- subset(emojis, select = c(emojiid, name)); map$emojiid <- as.numeric(map$emojiid);
df.m <- merge(df.l, map, by.x = 'emoji.ids', by.y = 'emojiid'); df.m <- arrange(df.m, tweetid); df.m <- rename(df.m, c(name = 'emoji.name'));
tweets.emojis.long <- subset(df.m, select = c(tweetid, emoji.name));
df.n <- aggregate(emoji.name ~ tweetid, paste, collapse = ', ', data = df.m);
## merge back with original tweets dataset
df.f <- merge(df.s.emojis, df.n, by = 'tweetid'); df.f <- rename(df.f, c(emoji.name = 'emoji.names'));
df.g <- rbind(df.f, df.s.nonemojis); df.g <- arrange(df.g, tweetid);
df.h <- merge(tweets, df.g, by = 'tweetid', all.x = TRUE); df.h$emoji.ids <- NULL; df.h$tweetid <- as.numeric(df.h$tweetid); df.h <- arrange(df.h, tweetid);
tweets.emojis <- df.h;

#### MAKE TWO WAY PLOT FOR A SET OF MUTUALLY EXCLUSIVE SUBSETS OF THE DATA
df.1 <- subset(tweets.emojis, grepl(paste(c('#womensmarch'), collapse = '|'), tolower(tweets.emojis$text)));
df.2 <- subset(tweets.emojis, grepl(paste(c('#theresistance'), collapse = '|'), tolower(tweets.emojis$text)));
nrow(df.1); nrow(df.2);
# dataset 1
df.a <- subset(subset(df.1, emoji.names != ''), select = c(tweetid, emoji.names)); df.a$emoji.names <- as.character(df.a$emoji.names);
df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ',')))); names(df.b) <- c('var', 'freq'); df.b$var <- trimws(df.b$var, 'both'); df.b <- subset(df.b, var != '');
df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x)); df.c <- df.c[with(df.c, order(-freq)), ]; row.names(df.c) <- NULL;
df.d <- subset(df.c, freq > 1); df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1); df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1); df.d$rank <- as.numeric(row.names(df.d)); df.d <- rename(df.d, c(var = 'name'));
df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank)); 
df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.1$text, '#\\w+'))))), -Freq)$Var1[1]);
df.e[1:10, ]; emojis.count.1 <- df.e;
# dataset 2
df.a <- subset(subset(df.2, emoji.names != ''), select = c(tweetid, emoji.names)); df.a$emoji.names <- as.character(df.a$emoji.names);
df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ',')))); names(df.b) <- c('var', 'freq'); df.b$var <- trimws(df.b$var, 'both'); df.b <- subset(df.b, var != '');
df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x)); df.c <- df.c[with(df.c, order(-freq)), ]; row.names(df.c) <- NULL;
df.d <- subset(df.c, freq > 1); df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1); df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1); df.d$rank <- as.numeric(row.names(df.d)); df.d <- rename(df.d, c(var = 'name'));
df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank));
df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.2$text, '#\\w+'))))), -Freq)$Var1[1]);
df.e[1:10, ]; emojis.count.2 <- df.e;
# combine datasets and create final dataset
names(emojis.count.1)[-1] <- paste0(names(emojis.count.1)[-1], '.1'); names(emojis.count.2)[-1] <- paste0(names(emojis.count.2)[-1], '.2'); 
df.a <- merge(emojis.count.1, emojis.count.2, by = 'name', all.x = TRUE, all.y = TRUE);
df.a[, c(2:4, 6:8)][is.na(df.a[, c(2:4, 6:8)])] <- 0; df.a <- df.a[with (df.a, order(-dens.1)), ];
df.a$index <- ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.1 > df.a$dens.2), round(100 * ((df.a$dens.1 / df.a$dens.2) - 1), 0),
                     ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.2 > df.a$dens.1), -1 * round(100 * ((df.a$dens.2 / df.a$dens.1) - 1), 0), NA));
df.a$logor <- log(df.a$dens.sm.1 / df.a$dens.sm.2);
df.a$dens.mean <- 0.5 * (df.a$dens.1 + df.a$dens.2);
k <- 50; df.b <- subset(df.a, (rank.1 <= k | rank.2 <= k) & 
                          (freq.1 >= 5 | freq.2 >= 5) & 
                          (freq.1 > 0 & freq.2 > 0) & dens.mean > 0); nrow(df.b);
df.c <- subset(df.b, select = c(name, dens.1, dens.2, freq.1, freq.2, dens.mean, round(logor, 2)));
df.c <- df.c[with(df.c, order(-logor)), ]; row.names(df.c) <- NULL; nrow(df.c); df.c;
emojis.comp.p <- df.c;
rbind(head(emojis.comp.p), tail(emojis.comp.p))

##### PLOT TOP EMOJIS SCATTERPLOT: FREQ VS VALENCE  
## read in custom emojis
setwd('.../PRISMOJI/tutorial/ios_9_3_emoji_files');
df.t <- arrange(emojis.comp.p, name);
imgs <- lapply(paste0(df.t$name, '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob);
## make plot  
df.t <- arrange(emojis.comp.p, logor)
xlab <- paste0('Emoji Valence: Log Odds Ratio (', paste0(unique(emojis.count.2$ht), ' <--> ', unique(emojis.count.1$ht), ')'));
ylab <- 'Overall Frequency (Per 1,000 Tweets)'
k <- 8 # size parameter for median element
xsize <- (k/100) * (max(df.t$logor) - min(df.t$logor)); ysize <- (k/100) * (max(df.t$dens.mean) - min(df.t$dens.mean));
df.t$xsize <- xsize; df.t$ysize <- ysize;
df.t$dens.m <- ifelse(df.t$dens.mean > median(df.t$dens.mean), round(sqrt((df.t$dens.mean / min(df.t$dens.mean))), 2), 1);
df.t$xsize <- df.t$dens.m * df.t$xsize; df.t$ysize <- df.t$dens.m * df.t$ysize;
df.t <- arrange(df.t, name);
g1 <- ggplot(df.t, aes(jitter(logor), dens.mean)) +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.t$xsize[i], xmax = x+0.5*df.t$xsize[i], 
                      ymin = y-0.5*df.t$ysize[i], ymax = y+0.5*df.t$ysize[i])},
    jitter(df.t$logor), df.t$dens.mean, seq_len(nrow(df.t))) +
  scale_x_continuous(limits = c(1.15 * min(df.t$logor), 1.15 * max(df.t$logor))) +
  scale_y_continuous(limits = c(0, 1.20 * max(df.t$dens.mean))) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
setwd('.../PRISMOJI/tutorial/');
png(paste0('emojis.comp.p_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g1; dev.off();
