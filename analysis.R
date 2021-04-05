library(ggplot2)
library(randomForest)
library(ROCR)
library(Metrics)

setwd("C:/Users/kenne/OneDrive/Desktop/Data Science/Analysis/Football")

#Set Pitch Length & Width
pitch_l = 114
pitch_w = 74
country <- c("England","France","Germany","Italy","Spain")

#### Full JSON data files can be downloaded from https://figshare.com/collections/Soccer_match_event_dataset/4415000/5
#### Or skip to curated data files below

### Events Data
# Note: Shots/goals data do not reflect the true number of shots/goals in the match due to tagging inaccuracies
#
# library(rjson)
# shots <- NULL
# for(j in 1:5){
#   events <- fromJSON(file = paste("events/events_",country[j],".json", sep=""))
#   for(i in 1:length(events)){
#     if (events[[i]][1] != 10) next
#     events_i <- events[[i]]
#     
#     # Check for goal (tag id = 101)
#     goal = sum(as.data.frame(events_i$tags) == 101)
#     
#     events_i <- within(events_i, rm(tags))
#     events_i <- as.data.frame(events_i)
#     events_i <- cbind(events_i, goal, country[j])
#     shots <- rbind(shots, events_i)
#   }
# }
# events <- NULL
# colnames(shots)[16] <- "league"
# shots$positions.y <- 101 - shots$positions.y # reflect horizontally
# head(shots)
#
## Adjust shots dataset to reflect opposite team
#
# for(i in 1:dim(shots)[1]){
#   hometeam = matches$home.teamId[which(matches$wyId==shots$matchId[i])]
#   awayteam = matches$away.teamId[which(matches$wyId==shots$matchId[i])]
#   if(shots$teamId[i] == hometeam){
#     shots$teamId.opp[i] <- awayteam
#   } else {
#     shots$teamId.opp[i] <- hometeam
#   }
# }
# write.csv(shots,"shots.csv", row.names = F)

### Matches Data
#
# matches <- NULL
# for(j in 1:5){
#   m <- fromJSON(file = paste("matches/matches_",country[j],".json", sep=""))
#   for(i in 1:length(m)){
#     m_i <- m[[i]]
#     m_i_home <- as.data.frame(within(m_i$teamsData[[1]], rm(formation)))
#     colnames(m_i_home) <- paste(m_i_home$side, colnames(m_i_home), sep=".")
#     m_i_away <- as.data.frame(within(m_i$teamsData[[2]], rm(formation)))
#     colnames(m_i_away) <- paste(m_i_away$side, colnames(m_i_away), sep=".")
#     
#     m_i <- as.data.frame(within(m_i, rm(teamsData,referees)))
#     m_i <- cbind(m_i, m_i_home, m_i_away, league = country[j])
#     matches <- rbind(matches, m_i)
#   }
# }
# m <- NULL
# matches <- matches[order(matches$wyId),]
# write.csv(matches,"matches.csv", row.names = F)

#### Teams Data
#
# t <- fromJSON(file = "teams.json")
# teams <- NULL
# for(i in 1:length(t)){
#   teams_i <- t[[i]]
#   teams_i <- as.data.frame(teams_i)
#   teams <- rbind(teams, teams_i)
# }
# teams[2] <- lapply(teams[2], as.character)
# teams$name[4] <- "Deportivo Alaves"
# teams$name[11] <- "Atletico Madrid"
# teams$name[18] <- "Leganes"
# teams$name[32] <- "Bayern Munchen"
# teams$name[59] <- "Saint-Etienne"
# teams$name[77] <- "Koln"
# teams$name[83] <- "Deportivo La Coruna"
# teams$name[97] <- "Malaga"
# write.csv(teams,"teams.csv", row.names = F)

#### Players Data
# pp <- fromJSON(file = "players.json")
# players <- NULL
# for(i in 1:length(pp)){
#      players_i <- pp[[i]]
#      if (is.null(players_i$currentTeamId)) next
#      players_i <- as.data.frame(players_i)
#      players <- rbind(players, players_i)
#    }
# write.csv(players,"players.csv", row.names = F)

#### Reading from CSV files (pre-processed data)

shots <- read.csv("shots.csv")
matches <- read.csv("matches.csv")
teams_all <- read.csv("teams.csv")
players <- read.csv("players.csv")


plot_density <- function (shots_XX, title = "", col_primary = "dark grey", col_secondary = "white", density = T, col_lim = 0.0018){
  m <- ggplot(shots_XX, aes(x = positions.x, y = positions.y)) +
    ggtitle(title) +
    #geom_point() +
    xlim(50, 101.5) +
    ylim(0, 100) +
    theme(plot.title = element_text(size=30),
          legend.title = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#f1faee"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  if(density){
    m <- m +
      #geom_density_2d_filled() + 
      stat_density_2d(geom = "polygon", contour = TRUE,
                      aes(fill = after_stat(level)), colour = "dark grey",
                      bins = 10) +
      #scale_fill_distiller(palette = palette_colour, direction = 1, limits = c(0,0.002*600/dim(shots_XX)[1])) +
      scale_fill_gradientn(colours = c(col_secondary, col_primary),limits = c(0 ,col_lim), na.value = col_primary)  
  } else{
    #set.seed(125)
    #shots_XX$positions.x <- shots_XX$positions.x + rnorm(dim(shots_XX)[1], 0, 0.15)
    #shots_XX$positions.y <- shots_XX$positions.y + rnorm(dim(shots_XX)[1], 0, 0.15)
    m <- m +
      geom_point(data = shots_XX, mapping = aes(x = positions.x, y = positions.y), color = col_primary, size = 2.0) +
      geom_point(data = shots_XX[shots_XX$goal==1,], mapping = aes(x = positions.x, y = positions.y), color = "red", size = 2.4)
  }
  m +
    geom_segment(aes(x = 100, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2+44)*100/pitch_w, xend = 100, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
    geom_segment(aes(x = 100, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2+20)*100/pitch_w, xend = 100, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
    geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100)) +
    geom_segment(aes(x = 50, y = 100, xend = 100, yend = 100)) +
    geom_segment(aes(x = 100, y = 100, xend = 100, yend = 0)) +
    geom_segment(aes(x = 100, y = 0, xend = 50, yend = 0)) +
    geom_segment(aes(x = 100, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2-4)*100/pitch_w)) +
    geom_segment(aes(x = 101.5, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2+4)*100/pitch_w)) +
    geom_segment(aes(x = 101.5, y = (pitch_w/2+4)*100/pitch_w, xend = 100, yend = (pitch_w/2+4)*100/pitch_w)) +
    geom_point(aes(x = 100-12*100/pitch_l, y = 50), size = 2)
}


#############################################
##            Random Forest                ##
#############################################

shots_RF <- shots

# A sample plot of 405 shots (1% of all shots in dataset), with red points indicating goals
set.seed(52391)
shots_samples <- sample(dim(shots_RF)[1], round(dim(shots_RF)[1]*0.01))
plot_density(shots[shots_samples,], density = F)


# Add in additional info for RF: Player position, Shot Distance, and Shot Angle
players_pos <- players[,c("wyId", "role.name")]
colnames(players_pos)[1] <- "playerId"
shots_RF <- merge(shots_RF, players_pos, by = "playerId")
shots_RF <- shots_RF[order(shots_RF$matchId,shots_RF$matchPeriod,shots_RF$eventSec),]
shots_RF <- shots_RF[,c("goal","positions.y","positions.x","matchPeriod","eventSec","teamId","teamId.opp","role.name")]

shots_RF$distance.to.goal <- (((101-shots_RF$positions.x)/100*pitch_l)^2 + ((50-shots_RF$positions.y)/100*pitch_w)^2)^(1/2)
seg_b <- (((101-shots_RF$positions.x)/100*pitch_l)^2 + ((55-shots_RF$positions.y)/100*pitch_w)^2)^(1/2)
seg_c <- (((101-shots_RF$positions.x)/100*pitch_l)^2 + ((45-shots_RF$positions.y)/100*pitch_w)^2)^(1/2)
shots_RF$angle <- acos((seg_b^2 + seg_c^2 - 7.4^2) / (2*seg_b*seg_c))*180/pi
shots_RF <- shots_RF[,-c(2,3)]

colnames(teams_all)[3] <- "teamId"
teams_all <- teams_all[teams_all$type=="club",c(2,3)]


# Split into Training and Test samples
set.seed(125)
n_samples <- dim(shots_RF)[1]
shots_samples <- sample(n_samples)
n_validation <- 5
score_auc <- NULL
score_rmse <- NULL



for (i in 1:n_validation){
  # Split into 5 sets for cross validation
  shots_samples_i <- shots_samples[(round(n_samples/n_validation*(i-1))+1):round(n_samples/n_validation*(i))]
  

  # Calculate each team's goal/shot ratio (offence and defence)
  shots_RF_train <- shots_RF[-shots_samples_i,]
  for(j in 1:nrow(teams_all)){
    shots_team_j <- shots_RF_train[shots_RF_train$teamId == teams_all$teamId[j],]
    teams_all$team_offence[j] <- sum(shots_team_j$goal) / nrow(shots_team_j)
    shots_team_opp_j <- shots_RF_train[shots_RF_train$teamId.opp == teams_all$teamId[j],]
    teams_all$opp_defence[j] <- sum(shots_team_opp_j$goal) / nrow(shots_team_opp_j)
  }
  teams_off <- teams_all[,c("teamId","team_offence")]
  teams_def <- teams_all[,c("teamId","opp_defence")]
  colnames(teams_def)[1] <- "teamId.opp"
  shots_RF2 <- merge(shots_RF, teams_off, by = "teamId")
  shots_RF2 <- merge(shots_RF2, teams_def, by = "teamId.opp")
  shots_RF2 <- shots_RF2[,-c(1,2)]

  # Obtain Training & Test datasets
  shots_RF_train <- shots_RF2[-shots_samples_i,]
  shots_RF_test <- shots_RF2[shots_samples_i,-1]
  labels <- shots_RF2[shots_samples_i,1]
  
  # Random Forest algorithm
  rf <- randomForest(y = shots_RF_train$goal, 
                     x = shots_RF_train[,-1], 
                     ntree = 1000, 
                     nodesize = 300,
                     importance = T)
  
  # Obtain test predictions
  predictions <- predict(rf,shots_RF_test)
  # Obtain AUC score
  pred <- prediction(predictions, labels)
  perf.auc <- performance(pred,'auc')
  score_auc <- c(score_auc, as.numeric(perf.auc@y.values))
  # Obtain RMSE score
  score_rmse <- c(score_rmse, rmse(labels, predictions))
}

# Average scores over 5 cross-validations
sum((predictions > 0.5) == labels) / length(predictions)
mean(score_auc)
mean(score_rmse)

# Plot importance
varImpPlot(rf)
importance(rf)


# Plot ROC
pred <- prediction(predictions, labels)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col=rainbow(10))
abline(0,1)

# Plot Precision-Recall Curve
perf2 <- performance(pred, "prec", "rec")
plot(perf2, col=rainbow(10), ylim=c(0,1))
abline(h=sum(labels)/length(labels))



# Compare RF against performance of its constituent trees

score_rmse2 <- rep(0,1000)
for (i in 1:n_validation){
  
  # Split into 5 sets for cross validation
  shots_samples_i <- shots_samples[(round(n_samples/n_validation*(i-1))+1):round(n_samples/n_validation*(i))]
  
  
  # Calculate each team's goal/shot ratio (offence and defence)
  shots_RF_train <- shots_RF[-shots_samples_i,]
  for(j in 1:nrow(teams_all)){
    shots_team_j <- shots_RF_train[shots_RF_train$teamId == teams_all$teamId[j],]
    teams_all$team_offence[j] <- sum(shots_team_j$goal) / nrow(shots_team_j)
    shots_team_opp_j <- shots_RF_train[shots_RF_train$teamId.opp == teams_all$teamId[j],]
    teams_all$opp_defence[j] <- sum(shots_team_opp_j$goal) / nrow(shots_team_opp_j)
  }
  teams_off <- teams_all[,c("teamId","team_offence")]
  teams_def <- teams_all[,c("teamId","opp_defence")]
  colnames(teams_def)[1] <- "teamId.opp"
  shots_RF2 <- merge(shots_RF, teams_off, by = "teamId")
  shots_RF2 <- merge(shots_RF2, teams_def, by = "teamId.opp")
  shots_RF2 <- shots_RF2[,-c(1,2)]
  
  # Obtain Training & Test datasets
  shots_RF_train <- shots_RF2[-shots_samples_i,]
  shots_RF_test <- shots_RF2[shots_samples_i,-1]
  labels <- shots_RF2[shots_samples_i,1]
  
  for (i in 1:1000){
    rf2 <- randomForest(y = shots_RF_train$goal, 
                       x = shots_RF_train[,-1], 
                       ntree = 1, 
                       nodesize = 300,
                       importance = T)
    
    predictions2 <- predict(rf2,shots_RF_test)
    score_rmse2[i] <- score_rmse2[i] + rmse(labels, predictions2)
  }
}
score_rmse2 <- score_rmse2 / 5
summary(score_rmse2)
hist(score_rmse2, 100, xlim = c(0.285,0.301), ylab = "Number of Trees", xlab = "RMSE", main = "Histogram of RMSE of 1000 Decision Trees")
abline(v=mean(score_rmse), col = "red")


# Obtain Goal Probabilities from Random Forest based on shot's position
rf3 <- randomForest(y = shots_RF$goal, 
                    x = shots_RF[,c("distance.to.goal","angle","role.name")], 
                    ntree = 1000, 
                    nodesize = 50)

goalprob <- NULL
for(i in 51:100){
  for(j in 1:100){
    distance.to.goal <- (((101-i)/100*pitch_l)^2 + ((50-j)/100*pitch_w)^2)^(1/2)
    seg_b <- (((101-i)/100*pitch_l)^2 + ((55-j)/100*pitch_w)^2)^(1/2)
    seg_c <- (((101-i)/100*pitch_l)^2 + ((45-j)/100*pitch_w)^2)^(1/2)
    angle <- acos((seg_b^2 + seg_c^2 - 7.4^2) / (2*seg_b*seg_c))*180/pi
    role.name = "Forward"
    goalprob <- rbind(goalprob, cbind(j, i, predict(rf3,data.frame(distance.to.goal,angle,role.name))))
  }
}
colnames(goalprob) <- c("y","x","prob")
goalprob <- as.data.frame(goalprob)

# Plot Heatmap of Goal Probabilities
ggplot(goalprob, aes(y = y, x = x, fill = prob)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#ffffff"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_tile(stat = "identity") +
  scale_fill_distiller(palette = "Spectral") +
  geom_segment(aes(x = 100, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2+44)*100/pitch_w, xend = 100, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
  geom_segment(aes(x = 100, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2+20)*100/pitch_w, xend = 100, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100)) +
  geom_segment(aes(x = 50, y = 100, xend = 100, yend = 100)) +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 0)) +
  geom_segment(aes(x = 100, y = 0, xend = 50, yend = 0))

ggplot(goalprob , aes(x = x, y = y)) +
  geom_raster(aes(fill = prob), interpolate=TRUE) +
  #scale_fill_gradient2(low="#4590b8", mid="#fffbb8", high="#d6404f", 
  #                      midpoint=0.5, limits=range(goalprob$prob)) +
  scale_fill_gradientn(colours=c("#4590b8","#c9e897","#fffbb8","#fd9a60","#d6404f"),limits=range(goalprob$prob)) +
  geom_segment(aes(x = 100, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2+44)*100/pitch_w, xend = 100, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
  geom_segment(aes(x = 100, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2+20)*100/pitch_w, xend = 100, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100)) +
  geom_segment(aes(x = 50, y = 100, xend = 100, yend = 100)) +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 0)) +
  geom_segment(aes(x = 100, y = 0, xend = 50, yend = 0))



