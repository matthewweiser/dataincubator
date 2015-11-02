setwd("C:\\Users\\Matt\\Desktop\\SportsStats\\MLB_2015SeasonPlays")
Sims2010_v8 <- read.table("C:/Users/Matt/Desktop/SportsStats/Data/Baseball/GamedaySimulationResults/ResultsFinal_GAMEDAY_generateRegressionPredictors_2010_1.3000_v5_todaysLineup.txt", sep="\t", stringsAsFactors=FALSE, header=TRUE)
Sims2010_v8$HomeTeam[Sims2010_v8$HomeTeam=="FLA"]  <- "MIA"
Sims2010_v8$AwayTeam[Sims2010_v8$AwayTeam=="FLA"]  <- "MIA"
Sims2011_v8 <- read.table("C:/Users/Matt/Desktop/SportsStats/Data/Baseball/GamedaySimulationResults/ResultsFinal_GAMEDAY_generateRegressionPredictors_2011_1.3000_v5_todaysLineup.txt", sep="\t", stringsAsFactors=FALSE, header=TRUE)
Sims2011_v8$HomeTeam[Sims2011_v8$HomeTeam=="FLA"]  <- "MIA"
Sims2011_v8$AwayTeam[Sims2011_v8$AwayTeam=="FLA"]  <- "MIA"
Sims2012_v8 <- read.table("C:/Users/Matt/Desktop/SportsStats/Data/Baseball/GamedaySimulationResults/ResultsFinal_GAMEDAY_generateRegressionPredictors_2012_1.3000_v5_todaysLineup.txt", sep="\t", stringsAsFactors=FALSE, header=TRUE)
Sims2013_v8 <- read.table("C:/Users/Matt/Desktop/SportsStats/Data/Baseball/GamedaySimulationResults/ResultsFinal_GAMEDAY_generateRegressionPredictors_2013_1.3000_v5_todaysLineup.txt", sep="\t", stringsAsFactors=FALSE, header=TRUE)
Sims2014_v8 <- read.table("C:/Users/Matt/Desktop/SportsStats/Data/Baseball/GamedaySimulationResults/ResultsFinal_GAMEDAY_generateRegressionPredictors_2014_1.3000_v5_todaysLineup.txt", sep="\t", stringsAsFactors=FALSE, header=TRUE)
Sims <- rbind(Sims2010_v8, Sims2011_v8, Sims2012_v8, Sims2013_v8, Sims2014_v8)

#figure 1- get the basic multiplicative park factors for each team, for year by year; order them by median; plot:
allTeams <- unique(Sims$HomeTeam)
fullList <- list()
allMedian <- c()
for(i in 1:length(allTeams) ){
  fullTab <- c()
  filter <- Sims2010_v8$HomeTeam == allTeams[i]
  runs_atHome <- sum(Sims2010_v8$HomeScore[filter] + Sims2010_v8$AwayScore[filter])/sum(filter)
  filter <- Sims2010_v8$AwayTeam == allTeams[i]
  runs_atAway <- sum(Sims2010_v8$HomeScore[filter] + Sims2010_v8$AwayScore[filter])/sum(filter)
  newRow <- c( runs_atHome/runs_atAway , allTeams[i])
  fullTab <- rbind(fullTab, newRow)
  filter <- Sims2011_v8$HomeTeam == allTeams[i]
  runs_atHome <- sum(Sims2011_v8$HomeScore[filter] + Sims2011_v8$AwayScore[filter])/sum(filter)
  filter <- Sims2011_v8$AwayTeam == allTeams[i]
  runs_atAway <- sum(Sims2011_v8$HomeScore[filter] + Sims2011_v8$AwayScore[filter])/sum(filter)
  newRow <- c( runs_atHome/runs_atAway , allTeams[i])
  fullTab <- rbind(fullTab, newRow)
  filter <- Sims2012_v8$HomeTeam == allTeams[i]
  runs_atHome <- sum(Sims2012_v8$HomeScore[filter] + Sims2012_v8$AwayScore[filter])/sum(filter)
  filter <- Sims2012_v8$AwayTeam == allTeams[i]
  runs_atAway <- sum(Sims2012_v8$HomeScore[filter] + Sims2012_v8$AwayScore[filter])/sum(filter)
  newRow <- c( runs_atHome/runs_atAway , allTeams[i])
  fullTab <- rbind(fullTab, newRow)
  filter <- Sims2013_v8$HomeTeam == allTeams[i]
  runs_atHome <- sum(Sims2013_v8$HomeScore[filter] + Sims2013_v8$AwayScore[filter])/sum(filter)
  filter <- Sims2013_v8$AwayTeam == allTeams[i]
  runs_atAway <- sum(Sims2013_v8$HomeScore[filter] + Sims2013_v8$AwayScore[filter])/sum(filter)
  newRow <- c( runs_atHome/runs_atAway , allTeams[i])
  fullTab <- rbind(fullTab, newRow)
  filter <- Sims2014_v8$HomeTeam == allTeams[i]
  runs_atHome <- sum(Sims2014_v8$HomeScore[filter] + Sims2014_v8$AwayScore[filter])/sum(filter)
  filter <- Sims2014_v8$AwayTeam == allTeams[i]
  runs_atAway <- sum(Sims2014_v8$HomeScore[filter] + Sims2014_v8$AwayScore[filter])/sum(filter)
  newRow <- c( runs_atHome/runs_atAway , allTeams[i])
  fullTab <- rbind(fullTab, newRow)
  allMedian <- c(allMedian, median(as.numeric(fullTab[,1] )) )
  fullList[[i]] <- fullTab
}
ORD <- order(allMedian)
orderedTab <- c()
for(i in 1:length(ORD)){
  orderedTab <- rbind(orderedTab , fullList[[ORD[i] ]] )
}
library(gplots)
ggplot(  ) +








# plot 2: generate data matrix to be fed to cluster 3.0 and treeview, consisting of 2014 end of year raw stats by player:
Events <- read.table(   )






