setwd("~/REPOS/cfbmodel")
#load(".RData")
#BiocManager::install("ggridges")
options(mc.cores = parallel::detectCores())
library("rstan")
library("ggplot2")
library("ggridges")
library("ggthemes")
rstan_options(auto_write = TRUE)

allgames <- read.csv("data/data2022.csv")
table(allgames$week)

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

fbsgames <- allgames[allgames$home_team %in% FBS & allgames$away_team %in% FBS,]
nrow(allgames)
nrow(fbsgames)

thisweek <- 6
# exclude games with NA in away team scores
cfb <- subset(fbsgames, week<=thisweek)

# exclude neutral site games
cfb <- subset(cfb, neutral_site=="false")

nrow(cfb)

#need some code to exclude tie games
factor(sort(unique(c(cfb$away_team, cfb$home_team))))

#head(cfb)
#names(cfb)
teams <- unique(c(
  as.character(cfb$away_team), 
  as.character(cfb$home_team)))
teams <- factor(teams[order(teams)])
unique(as.integer(cfb$home_team))

away_teams <- integer(nrow(cfb))
for (i in seq_along(cfb$away_team)) {
  away_teams[i] <- which(teams %in% as.character(cfb$away_team[i]))
}
home_teams <- integer(nrow(cfb))
for (i in seq_along(cfb$away_team)) {
  home_teams[i] <- which(teams %in% as.character(cfb$home_team[i]))
}

home_points_q123 <- rowSums(cfb[, 16:18])
away_points_q123 <- rowSums(cfb[, 27:29])

newgames <- subset(subset(fbsgames, week==thisweek+1), neutral_site=="false")
nrow(newgames)

at = newgames$away_team
ati = as.integer(as.factor(newgames$away_team))
ht = newgames$home_team
hti = as.integer(as.factor(newgames$home_team))

cfb_data <- list(
  ngames = nrow(cfb),
  nteams = length(teams),
  npreds = length(ati),
  awayscore = cfb$away_points,
  homescore = cfb$home_points,
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
  real homeadv[nteams];
  real<lower=0> stdev;
}
model {
  for (j in 1:nteams) {
    offense[j] ~ normal(20, 10);
    defense[j] ~ normal(0, 7);
    homeadv[j] ~ normal(0, 7);
  }
  stdev ~ gamma(3.0, 1.0);
  for (i in 1:ngames) {
    awayscore[i] ~ normal(offense[awayteam[i]] 
      - defense[hometeam[i]], stdev);
    homescore[i] ~ normal(offense[hometeam[i]]
      - defense[awayteam[i]]
      + homeadv[hometeam[i]], stdev);
  }
}
generated quantities {
  real away_pred[npreds];
  real home_pred[npreds];
  real diff[npreds];
  for (i in 1:npreds) {
    away_pred[i] = offense[ati[i]] - defense[hti[i]];
    home_pred[i] = offense[hti[i]] - defense[ati[i]] + homeadv[hti[i]];
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
res <- rstan::extract(posterior)

teamscore <- apply(res$offense, FUN = median, MARGIN = 2)+apply(res$defense, FUN = median, MARGIN = 2)
homeadv <- apply(res$homeadv, FUN = median, MARGIN = 2)
cfbstat <- data.frame(
  team = teams,
  teamscore = round(teamscore, 1),
  offense = round(apply(res$offense, FUN = median, MARGIN = 2), 1),
  defense = round(apply(res$defense, FUN = median, MARGIN = 2), 1),
  homeadv = round(homeadv, 1))
# 
cfbstat <- cfbstat[order(cfbstat$teamscore, decreasing = T),]
row.names(cfbstat) <- 1:nrow(cfbstat)
cfbstat[1:50,]
cfbstat[cfbstat$team %in% PAC12,]
cfbstat[cfbstat$team %in% B1G,]
cfbstat[cfbstat$team %in% SEC,]
cfbstat[cfbstat$team %in% Independents,]
cfbstat[cfbstat$team %in% ACC,]
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

p <- pretty(c(0,1 ), 100)
y <- 180*(1-p)-100*p # Utah at -180
y <- -145*p + 100 * (1-p) # USC at +145
plot(p, y)
