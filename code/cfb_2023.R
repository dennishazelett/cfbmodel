setwd("~/REPOS/cfbmodel")
#load(".RData")
#BiocManager::install("ggridges")
options(mc.cores = parallel::detectCores())
library("rstan")
library("ggplot2")
library("ggridges")
library("ggthemes")
rstan_options(auto_write = TRUE)

allgames <- read.csv("data/data2023.csv")
table(allgames$Week)

PAC12 <- c("Oregon","Utah", "USC", "Washington", "UCLA", "Arizona State", "Oregon State", "Stanford", "Arizona", "California", "Colorado", "Washington State")
B1G <- c("Ohio State", "Michigan", "Minnesota", "Wisconsin", "Indiana", "Rutgers", "Iowa", "Purdue", "Michigan State", "Nebraska",  "Penn State", "Maryland", "Northwestern", "Illinois")
SEC <- c("LSU", "Alabama", "Auburn", "Georgia", "Florida", "Kentucky", "Tennessee", "Arkansas", "Texas A&M", "Missouri", "South Carolina", "Vanderbilt", "Ole Miss", "Mississippi State")
ACC <- c("Florida State", "Clemson", "Miami", "Duke", "Wake Forest", "North Carolina", "Syracuse", "Georgia Tech", "North Carolina State", "Louisville", "Pitt", "Notre Dame", "Boston College", "Virginia", "Virginia Poly")
BIG12 <- c("Baylor", "Iowa State", "Kansas", "Kansas State", "Oklahoma", "Oklahoma State", "TCU", "Texas", "Texas Tech", "WVU")
American <- c("Cincinnati", "South Florida", "East Carolina", "Temple", "Houston", "Tulane", "Memphis", "Tulsa", "Navy", "UCF", "SMU")
CUSA <- c("Charlotte", "Rice", "Florida Atlantic", "UAB", "Florida International", "UTEP", "Louisiana Tech", "UT San Antonio", "Middle Tennessee", "Western Kentucky", "North Texas")
MAC <- c("Ball State", "Akron", "Central Michigan", "Bowling Green", "Eastern Michigan", "Buffalo", "Northern Illinois", "Kent State", "Toledo", "Miami (OH)", "Western Michigan", "Ohio")
MW <- c("Fresno State", "Hawai'i", "Nevada", "UNLV", "San Diego State", "San Jose State", "Air Force", "Boise State", "Colorado State", "New Mexico", "Utah State", "Wyoming")
SunBelt <- c("Arkansas State", "Louisiana", "Louisiana Monroe", "South Alabama", "Southern Miss", "Texas State", "Troy", "Appalachian State", "Coastal Carolina", "Georgia Southern", "Georgia State", "James Madison", "Marshall", "Old Dominion")
Independents <- c("West Point", "BYU", "Liberty", "New Mexico State", "Notre Dame", "Connecticut", "UMass")
P5 <- c(PAC12, B1G, SEC, ACC, BIG12)
G5 <- c(American, CUSA, MAC, MW, SunBelt)
FBS <- c(P5, G5)

fbsgames <- allgames[allgames$Home.Division == "fbs" & allgames$Away.Division == "fbs",]
nrow(allgames)
nrow(fbsgames)

thisweek <- 12
# exclude games with NA in away team scores
cfb <- subset(fbsgames, Week<=thisweek)

# exclude neutral site games
cfb <- subset(cfb, Neutral.Site=="false")

nrow(cfb)

#need some code to exclude tie games
factor(sort(unique(c(cfb$Away.Team, cfb$Home.Team))))

#head(cfb)
#names(cfb)
teams <- unique(c(
  as.character(cfb$Away.Team), 
  as.character(cfb$Home.Team)))

teams <- factor(teams[order(teams)])
teamsidx <- as.integer(teams)

away_teams <- integer(nrow(cfb))
for (i in seq_along(cfb$Away.Team)) {
  away_teams[i] <- which(teams %in% as.character(cfb$Away.Team[i]))
}
home_teams <- integer(nrow(cfb))
for (i in seq_along(cfb$Home.Team)) {
  home_teams[i] <- which(teams %in% as.character(cfb$Home.Team[i]))
}

#home_points_q123 <- rowSums(cfb[, 16:18])
#away_points_q123 <- rowSums(cfb[, 27:29])

newgames <- subset(subset(fbsgames, Week==thisweek+1), Neutral.Site=="false")
nrow(newgames)

at = newgames$Away.Team

ati = integer(length(at))
for (i in seq_along(ati)) {
  ati[i] <- which(teams %in% newgames$Away.Team[i])
}
ht = newgames$Home.Team
hti = integer(length(ht))
for (i in seq_along(hti)) {
  hti[i] <- which(teams %in% newgames$Home.Team[i])
}

hti %in% ati

cfb_data <- list(
  ngames = nrow(cfb),
  nteams = length(teams),
  npreds = length(ati),
  awayscore = cfb$Away.Points,
  homescore = cfb$Home.Points,
  # awayscore = away_points_q123,
  # homescore = home_points_q123,
  awayteam = away_teams,
  hometeam = home_teams,
  ati = ati,
  hti = hti
)

modelstring <-"
data {
  int<lower=0> ngames;
  int<lower=0> nteams;
  int npreds;
  int<lower=0> awayscore[ngames];
  int<lower=0> homescore[ngames];
  int<lower=0> awayteam[ngames];
  int<lower=0> hometeam[ngames];
  int ati[npreds];
  int hti[npreds];
}
parameters {
  real<lower=0> offense[nteams];
  real defense[nteams];
  // real homeadv[nteams];
  real homeadv;
  real<lower=0> stdev;
}
model {
  for (j in 1:nteams) {
    offense[j] ~ normal(20, 10);
    defense[j] ~ normal(0, 7);
    // homeadv[j] ~ normal(0, 7);
    homeadv ~ normal(3, 7);
  }
  stdev ~ gamma(3.0, 1.0);
  for (i in 1:ngames) {
    awayscore[i] ~ normal(offense[awayteam[i]] 
      - defense[hometeam[i]], stdev);
    homescore[i] ~ normal(offense[hometeam[i]]
      - defense[awayteam[i]]
      // + homeadv[hometeam[i]], stdev);
      + homeadv, stdev);
  }
}
generated quantities {
  real away_pred[npreds];
  real home_pred[npreds];
  real diff[npreds];
  for (i in 1:npreds) {
    away_pred[i] = offense[ati[i]] - defense[hti[i]];
    home_pred[i] = offense[hti[i]] - defense[ati[i]] + homeadv;
    diff[i] = away_pred[i] - home_pred[i];
  }
}
"
gc()
posterior <- stan(
  model_code = modelstring,
  data = cfb_data,
  iter = 110000,
  warmup = 10000,
  thin = 250,
  # control = list(
  #   adapt_delta = 0.85,
  #   max_treedepth = 12),
  chains = 4
)
# 
# 
gc()
rm(res)
res <- rstan::extract(posterior)
#str(res)
teamscore <- apply(res$offense, FUN = median, MARGIN = 2)+apply(res$defense, FUN = median, MARGIN = 2)
#homeadv <- apply(res$homeadv, FUN = median, MARGIN = 2)
homeadv <- mean(res$homeadv)

cfbstat <- data.frame(
  team = teams,
  teamscore = round(teamscore, 1),
  offense = round(apply(res$offense, FUN = median, MARGIN = 2), 1),
  defense = round(apply(res$defense, FUN = median, MARGIN = 2), 1))
  # homeadv = round(homeadv, 1))
# 
cfbstat <- cfbstat[order(cfbstat$teamscore, decreasing = T),]
row.names(cfbstat) <- 1:nrow(cfbstat)
cfbstat[1:25,]
#P12rank <- cfbstat[cfbstat$team %in% PAC12,]
#B1Grank <- cfbstat[cfbstat$team %in% B1G,]
#cfbstat[cfbstat$team %in% BIG12,]
#cfbstat[cfbstat$team %in% SEC,]
#cfbstat[cfbstat$team %in% Independents,]
#cfbstat[cfbstat$team %in% ACC,]
# 
cfbpred <- data.frame(
  visitor = at,
  home = ht,
  vis_pred = round(apply(res$away_pred, FUN = median, MARGIN = 2), 0),
  hom_pred = round(apply(res$home_pred, FUN = median, MARGIN = 2), 0),
  ovrund = round(apply(res$away_pred, FUN = median, MARGIN = 2) +
                   apply(res$home_pred, FUN = median, MARGIN = 2), 0),
  spread = round(apply(res$diff, FUN = median, MARGIN = 2),0),
  low = round(apply(
    res$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.025)), 0),
  hi  = round(apply(
    res$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.975)), 0),
  home_win_prob = apply(
    res$diff, MARGIN = 2,
    FUN = function(x) paste(
      round(sum(x<0)/length(x) * 100, 1),
      "%"))
  )
# 
cfbpred
# 
# compare <- data.frame(
#   preds = c(cfbpred$vis_pred, cfbpred$hom_pred),
#   obsv  = c(cfbpred$vis_score, cfbpred$hom_score),
#   team  = factor(rep(c("visitor", "home"), each=35)))
# 
# ggplot(compare, aes(x = preds, y=obsv, colour = team)) +
#   geom_point(size=3) +
#   theme_minimal() +
#   scale_color_manual(values = c("#f1a340", "#998ec3")) +
#   ylab("Final Score") + xlab("Predicted") +
#   ggtitle("Week 12 score predictions")
# 
# ggplot(cfbpred, aes(x=spread, y=vis_score - hom_score)) +
#   geom_point(size=3) +
#   theme_minimal() +
#   coord_fixed(xlim = c(-55, 40), ylim = c(-55, 40)) +
#   xlab("predicted spread") + ylab("away team - home team") +
#   ggtitle("week 12 margin of victory")

save(cfbpred, file = paste("data/week", thisweek+1, "preds.rda", sep = ''))


p <- pretty(c(0,1 ), 100)
y <- 180*(1-p)-100*p # Utah at -180
y <- -145*p + 100 * (1-p) # USC at +145
plot(p, y)

teamname <- "Hawai'i"
cfb[cfb$Home.Team==teamname | cfb$Away.Team==teamname,]

P12rank$offense[3]
P12preds <- matrix(nrow = 12, ncol = 12)
for (i in 1:12) {
  for (j in 1:12) {
    homescore <- P12rank$offense[i] - P12rank$defense[j] + homeadv
    awayscore <- P12rank$offense[j] - P12rank$defense[i]
    result <- awayscore - homescore
    P12preds[ i, j ] <- result
  }
}

P12preds <- round(P12preds, digits = 2)
rownames(P12preds) <- P12rank$team
colnames(P12preds) <- P12rank$team
heatmap(round(P12preds, digits = 2), Rowv = NULL, Colv = "Rowv")
warnings()
image(rownames(P12preds), colnames(P12preds), P12preds)

