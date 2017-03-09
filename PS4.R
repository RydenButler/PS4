############
### PS 4 ###
############

library(rvest) 
library(htmltab)

### SCRAPING: STEP 1

# store url
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# create object containing scraped tables
temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

# subset the table containing the relevant data
WikiData <- html_table(temp[2])
# convert table to data.frame class and remove leading rows of heading
WikiData <- as.data.frame(WikiData)[-(1:2),]
# remove characters from numeric columns
WikiData <- apply(WikiData, 2, function(x) gsub('\\[a\\]', '', x))
WikiData <- apply(WikiData, 2, function(x) gsub('\\%', '', x))
WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)] <- apply(WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)], 
                                            2, function(x) gsub(',', '', x))
WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)] <- apply(WikiData[, c(1, 2, 6, 7, 8, 9, 10, 13)], 
                                             2, function(x) gsub('−', '-', x))
# remove name before comma. Original data of format "Washington, George Washington"
WikiData[, c(3, 11)] <- apply(WikiData[, c(3, 11)], 2, 
                              function(x) gsub('^(.+?),', '', x))
# convert data from matrix to dataframe
WikiData <- as.data.frame(WikiData)
# order data by election number (and effectively by date)
WikiData <- WikiData[order(WikiData$Election), ]
# extract last name of winner in each election. store as new column
# last names are useful for merging with other datasets
WikiData$WinLast <- unlist(lapply(strsplit(as.character(WikiData[ , 3]), 
                                           split = ' '), function(x) tail(x, 1)))
# extract last name of runner-up in each election. store as new column
WikiData$RunLast <- unlist(lapply(strsplit(as.character(WikiData[ , 11]), 
                                           split = ' '), function(x) tail(x, 1)))

### PLOTTING

# save colors corresponding to each winning candidate based on party
PointColors <- as.character(WikiData$Winner...party.1)
PointColors[PointColors == 'Dem.'] <- 'blue'
PointColors[PointColors == 'Rep.'] <- 'red'
PointColors[PointColors == 'Whig'] <- 'yellow2'
PointColors[PointColors == 'D.-R.'] <- 'forestgreen'

PointNames <- WikiData$WinLast

# set graphical parameters
pdf(file = "TrumpElectoralVictory.pdf", width = 10)
par(mfrow = c(1,2),
    oma = c(5,4,3,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
# plot electoral votes over time
plot(x = as.numeric(as.character(WikiData$Election.1)), 
     y = as.numeric(as.character(WikiData$Electoral.College.1)), 
     col = c(PointColors[-49], 'orange'), pch = 20, ylim = c(30,100), 
     main = 'Electoral Vote')
# plot line at 50 percent
abline(h = 50)

# plot line at Trump's vote precent
abline(h = as.numeric(as.character(WikiData$Electoral.College.1))[49], 
       lty = 2, col = 'orange')

# plot popular vote over time
plot(x = as.numeric(as.character(WikiData$Election.1)), 
     y = as.numeric(as.character(WikiData$Popular.vote)), 
     col = c(PointColors[-49], 'orange'), pch = 20, 
     ylim = c(30,100), yaxt = 'n', main = 'Popular Vote')
# plot line at 50 percent
abline(h = 50)

# plot line at Trump's vote precent
abline(h = as.numeric(as.character(WikiData$Popular.vote))[49], 
       lty = 2, col = 'orange')
# add axis title
title(xlab = "Election Year",
      ylab = "Percent Vote",
      outer = TRUE, line = 2)

legend(x = 'topleft', bty = 'n', pch = 20,
       legend = c('Democrat','Repuplican', 'Democratic Republican', 'Whig', 'Trump'), 
       col = c('blue', 'red', 'forestgreen', 'yellow2', 'orange'))

dev.off()
# The plot saved in the file TrumpElectoralVictory.pdf indicates the percentage
# of electoral votes (left panel) and the percentage of popular votes (right panel)
# that each winning candidate received in the presidential elections from 
# 1824-2016. The left panel reveals that, despite claims to the contrary, 
# Trump's electoral victory is neither particularly large, nor is it comparable
# to the electoral landslide of Ronald Reagan. Of note, all elections since 1824
# have resulted in a presidential victor who received a majority of electoral votes.
# The right plot indicates that Trump's share of the popular vote, while among the 
# lowest since the 1824 election is not historically low. We can see that, in recent 
# history, President Clinton received a lower percentage of the popular vote than 
# Trump received in the most recent election. Moreover, we can note the presence 
# of numerous presidents who won the presidential contest despite receiving less 
# than 50% of the popular vote. Combined, we might notice the disparity between 
# percentages in the electoral and popular votes that winning candidates. Though
# this disparity is expected given the winner-take-all format of most states' 
# electoral college apportionment rules, it's still worth noting that apparent
# electoral landslides for FDR, Reagan and Bush still fail to garner more than
# 65% of the popular vote.


### SCRAPING: STEP 2 & MERGING

# store url
wikiURL <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

# extract table using htmltab function in htmltab package
# this function effectively handles column values that span multiple rows
# note that the warning here indicates the removal of an empty column
WikiData2 <- htmltab(wikiURL, which = 3)

# extract the number of electoral votes for each candidate per election
WikiData2[, 7] <- gsub( " .*$", "", WikiData2[, 7])
# manually recode the number of electoral votes for this observation
# the associated value in the Wikipedia table notes that the total is 42
WikiData2[106, 7] <- '42'

# sum all electoral votes for each candidate in any given election
WikiData2 <- aggregate(as.numeric(WikiData2[, 7]) ~ WikiData2[, 1] + WikiData2[, 3], FUN = sum) 
# name columns in aggregated dataset
colnames(WikiData2) <- c('Year', 'Candidate', 'Votes')

# create new variable containing candidate last names
WikiData2$CandLast <- unlist(lapply(strsplit(WikiData2$Candidate, split = ' '), 
                                  function(x) tail(x, 1)))
# manually recode observations of II as Stevenson (here Adlai Stevenson II)
WikiData2[WikiData2$CandLast == 'II', 'CandLast'] <- rep('Stevenson', 2)

# Merge winner electoral votes with first dataset
MergeWinners <- merge(x = WikiData, y = WikiData2[,c(1,3,4)], 
           by.x = c('Election.1', 'WinLast'), 
           by.y = c('Year', 'CandLast'))
# Merge wunner-up electoral votes with first dataset
MergedData <- merge(x = MergeWinners, y = WikiData2[,c(1,3,4)], 
                     by.x = c('Election.1', 'RunLast'), 
                     by.y = c('Year', 'CandLast'))
# Reformat merged data to resemble original table w/ two extra columns
MergedData <- MergedData[, c(4, 1, 5:17)]
# Name each variable something reasonable
colnames(MergedData) <- c('ElectionNumber', 'ElectionYear', 
                          'WinnerName', 'WinnerParty', 
                          'ElectoralCollegeRatio', 'ElectoralCollegePercent',
                          'PopularVotePercent', 'PopularVotePercentMargin',
                          'PopularVotes', 'PopularVotesMargin', 
                          'RunnerUpName', 'RunnerUpParty', 'TurnoutPercent', 
                          'WinnerElectoralVotes', 'RunnerUpElectoralVotes')
# save merged data to .Rdata file
save(MergedData, file = 'MergedData.Rdata')
