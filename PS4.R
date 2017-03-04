library(rvest) 
library(htmltab)

wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

WikiData <- html_table(temp[2])
WikiData <- as.data.frame(WikiData)[-(1:2),]
WikiData <- apply(WikiData, 2, function(x) gsub('\\[a\\]', '', x))
WikiData <- apply(WikiData, 2, function(x) gsub('\\%', '', x))
WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)] <- apply(WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)], 
                                            2, function(x) gsub(',', '', x))
WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)] <- apply(WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)], 
                                             2, function(x) gsub('−', '-', x))
WikiData <- as.data.frame(WikiData)
WikiData <- WikiData[order(WikiData$Election), ]
WikiData[, c(3, 11)] <- apply(WikiData[, c(3, 11)], 2, 
                              function(x) gsub('^(.+?),', '', x))

WikiData$WinLast <- unlist(lapply(strsplit(WikiData[ , 3], split = ' '), 
                                  function(x) tail(x, 1)))
WikiData$RunLast <- unlist(lapply(strsplit(WikiData[ , 11], split = ' '), 
                                  function(x) tail(x, 1)))
# PLOTTING

PointColors <- as.character(WikiData$Winner...party.1)
PointColors[PointColors == 'Dem.'] <- 'blue'
PointColors[PointColors == 'Rep.'] <- 'red'
PointColors[PointColors == 'Whig'] <- 'yellow'
PointColors[PointColors == 'D.-R.'] <- 'green'

plot(x = as.numeric(WikiData$Election), y = as.numeric(WikiData$Popular.vote), 
     type = 'l', ylim = c(0,100), lty = 1, col = 'blue')
abline(v = 21)
lines(x = WikiData$Election, y = WikiData$Electoral.College.1, lty = 2, col = 'red')
lines(x = WikiData$Election, y = WikiData$Turnout.2., lty = 3, col = 'green')

plot(x = as.numeric(WikiData$Popular.vote), 
     y = as.numeric(WikiData$Electoral.College.1),
     col = PointColors, pch = 19)

plot(x = as.numeric(WikiData$Election), 
     y = as.numeric(WikiData$Popular.vote),
     col = PointColors, pch = 19)

# MERGING

wikiURL <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

WikiData2 <- htmltab(wikiURL, which = 3)

WikiData2[, 7] <- gsub( " .*$", "", WikiData2[, 7])
WikiData2[106, 7] <- '42'

WikiData2 <- aggregate(as.numeric(WikiData2[, 7]) ~ WikiData2[, 1] + WikiData2[, 3], FUN = sum) 
colnames(WikiData2) <- c('Year', 'Candidate', 'Votes')

WikiData2$CandLast <- unlist(lapply(strsplit(WikiData2$Candidate, split = ' '), 
                                  function(x) tail(x, 1)))
WikiData2[WikiData2$CandLast == 'II', 'CandLast'] <- rep('Stevenson', 2)

MergeWinners <- merge(x = WikiData, y = WikiData2[,c(1,3,4)], 
           by.x = c('Election.1', 'WinLast'), 
           by.y = c('Year', 'CandLast'))
MergedData <- merge(x = MergeWinners, y = WikiData2[,c(1,3,4)], 
                     by.x = c('Election.1', 'RunLast'), 
                     by.y = c('Year', 'CandLast'))
MergedData[, c(16, 17)]
