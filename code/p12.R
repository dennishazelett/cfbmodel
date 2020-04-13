setwd("~/Projects/NON_WORK/p12proj")
#load(".RData")
#BiocManager::install("ggridges")
library("rstan")
library("ggplot2")
library("ggridges")
library("ggthemes")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

allgames <- read.csv("p12re2019.txt")
p12 <- subset(allgames, week <= 11)
head(p12)

teams <- unique(c(
  as.character(p12$awayteam), 
  as.character(p12$hometeam)))
teams <- factor(teams[order(teams)])
#unique(as.integer(p12$hometeam, levels=levels(teams)))
awayteams <- integer(nrow(p12))
for (i in seq_along(p12$awayteam)) {
  awayteams[i] <- which(teams %in% as.character(p12$awayteam[i]))
}
hometeams <- integer(nrow(p12))
for (i in seq_along(p12$awayteam)) {
  hometeams[i] <- which(teams %in% as.character(p12$hometeam[i]))
}


at = c(
  "KS", "AZ", "IN", "AL", "MIS", "WI", "FL", "NAVY", "MEMPH", "WF",
  "TX", "GA", "OHS", "MN", "CIN", "LAS", "OK", "UCLA", "NM", "SC", 
  "ST", "AZS", "EMI", "WMI", "BOWL", "NIL", "BUFF", "NC", "LAT", "FREST",
  "ALS", "UMASS", "TCU", "TULN", "VMI")
ht = c(
  "OKS", "OR", "PAS", "MSS", "MI", "NE", "MO", "NODA", "HOUS", "CLEM",
  "IAS", "AUB", "R", "IA", "SFL", "MS", "BAYL", "UT", "BOIS", "CA",
  "WAS", "ORS", "AKR", "OH", "MIOH", "TOL", "KENT", "PITT", "MARSH", "SDS",
  "FLS", "NORW", "TXT", "TEMP", "ARMY")

ati <- integer(length(at))
for (i in seq_along(at)) {
  ati[i] <- which(teams %in% at[i])
}
hti <- integer(length(ht))
for (i in seq_along(ht)) {
  hti[i] <- which(teams %in% ht[i])
}


cfb_data <- list(
  ngames = nrow(p12),
  nteams = length(teams),
  npreds = length(ati),
  awayscore = p12$awayscore,
  homescore = p12$homescore,
  awayteam = awayteams,
  hometeam = hometeams,
  # OT = p12$OT,
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
  int<lower=0> OT[ngames];
  int ati[npreds];
  int hti[npreds];
}
parameters {
  real<lower=0> offense[nteams];
  real defense[nteams];
  real homeadv[nteams];
  real<lower=0> otscore;
  real<lower=0> stdev;
}
model {
  for (j in 1:nteams) {
    offense[j] ~ normal(20, 10);
    defense[j] ~ normal(0, 7);
    homeadv[j] ~ normal(0, 7);
  }
  otscore ~ normal(7, 1);
  stdev ~ gamma(3.0, 1.0);
  for (i in 1:ngames) {
    awayscore[i] ~ normal(offense[awayteam[i]] 
      - defense[hometeam[i]]
      + otscore * OT[i], stdev);
    homescore[i] ~ normal(offense[hometeam[i]]
      - defense[awayteam[i]]
      + homeadv[hometeam[i]]
      + otscore * OT[i], stdev);
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

# posterior <- stan(
#   model_code = modelstring,
#   data = cfb_data,
#   iter = 110000,
#   warmup = 10000,
#   thin = 250,
#   # control = list(
#   #   adapt_delta = 0.85,
#   #   max_treedepth = 12),
#   chains = 4
# )
# 
# 
# res <- rstan::extract(posterior)
# 
# teamscore <- colMeans(res$offense)+colMeans(res$defense)
# homeadv <- colMeans(res$homeadv)
# mean(res$otscore)
# cfbstat <- data.frame(
#   team = teams, 
#   teamscore = round(teamscore, 1), 
#   offense = round(colMeans(res$offense), 1),
#   defense = round(colMeans(res$defense), 1),
#   homeadv = round(homeadv, 1))
# 
# cfbstat <- cfbstat[order(cfbstat$teamscore, decreasing = T),]
# row.names(cfbstat) <- 1:nrow(cfbstat)
# cfbstat[1:25,]
# pac12 <- c("OR","UT", "SC", "WA", "UCLA", "AZS", "ORS", "ST", "AZ", "CA", "CO", "WAS")
# B1G <- c("OHS", "MI", "MN", "WI", "IN", "R", "IA", "PURD", "MIS", "NE",  "PAS", "MD", "NORW", "IL")
# SEC <- c("LAS", "AL", "AUB", "GA", "FL", "KY", "TN", "AR", "TAM", "MO", "SCARO", "VAND", "MS", "MSS")
# cfbstat[cfbstat$team %in% pac12,]
# cfbstat[cfbstat$team %in% B1G,]
# cfbstat[cfbstat$team %in% SEC,]
# 
# cfbpred <- data.frame(
#   visitor = at,
#   home = ht,
#   vis_pred = round(colMeans(res$away_pred), 0),
#   hom_pred = round(colMeans(res$home_pred), 0),
#   ovrund = round(colMeans(res$away_pred) + 
#                    colMeans(res$home_pred), 0),
#   spread = round(colMeans(res$diff),0),
#   low = round(apply(
#     res$diff, MARGIN = 2,
#     FUN = function(x) quantile(x, 0.025)), 0),
#   hi  = round(apply(
#     res$diff, MARGIN = 2,
#     FUN = function(x) quantile(x, 0.975)), 0),
#   home_win_prob = apply(
#     res$diff, MARGIN = 2, 
#     FUN = function(x) paste(
#       round(sum(x<0)/length(x) * 100, 1),
#       "%"))
#   )
# 
# cfbpred$vis_score <- c(13,  6, 27, 38, 10, 37, 23, 52, 45,  3, 
#                        21, 21, 56, 19, 20, 58, 34,  3,  9, 41, 
#                        22, 34, 42, 37,  3, 31, 27, 27, 10,  7,
#                        12,  6, 33, 21,  6)
# cfbpred$hom_score <- c(31, 34, 34,  7, 44, 21,  6, 20, 27, 52, 
#                        23, 14, 21, 23, 17, 37, 31, 49, 42, 17, 
#                        49, 35, 14, 34, 44, 28, 30, 27, 31, 17,
#                        49, 45, 31, 29, 47)
# 
# cfbpred
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
  


########################################################
###################### WEEK 14 #########################
########################################################


modelstring2 <-"
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
transformed data {
  real mean_score;
  mean_score = (sum(awayscore) + sum(homescore)) / (2.0 * ngames);
}
parameters {
  real offense[nteams];
  real defense[nteams];
  real homeoff[nteams];
  real homedef[nteams];
  real<lower=0> stdev;
}
model {
  for (j in 1:nteams) {
    offense[j] ~ normal(0, 10);
    defense[j] ~ normal(0, 10);
    homeoff[j] ~ normal(0, 10);
    homedef[j] ~ normal(0, 10);
  }
  stdev ~ gamma(3.0, 1.0);
  for (i in 1:ngames) {
    awayscore[i] ~ normal(mean_score
      + offense[awayteam[i]] 
      - defense[hometeam[i]]
      - homedef[hometeam[i]], stdev);
    homescore[i] ~ normal(mean_score
      + offense[hometeam[i]]
      - defense[awayteam[i]]
      + homeoff[hometeam[i]], stdev);
  }
}
generated quantities {
  real away_pred[npreds];
  real home_pred[npreds];
  real diff[npreds];
  for (i in 1:npreds) {
    away_pred[i] = mean_score + offense[ati[i]] - defense[hti[i]] - homedef[hti[i]];
    home_pred[i] = mean_score + offense[hti[i]] - defense[ati[i]] + homeoff[hti[i]];
    diff[i] = away_pred[i] - home_pred[i];
  }
}
"

## since I've updated my model, I need to re-run week 12

week12_posterior <- stan(
  model_code = modelstring2,
  data = cfb_data,
  iter = 110000,
  warmup = 10000,
  thin = 400,
  # control = list(
  #   adapt_delta = 0.85,
  #   max_treedepth = 12),
  chains = 4
)

res <- rstan::extract(week12_posterior)

teamscore <- colMeans(res$offense)+colMeans(res$defense)
offhomeadv <- colMeans(res$homeoff)
defhomeadv <- colMeans(res$homedef)

cfbstat <- data.frame(
  team = teams, 
  teamscore = round(teamscore, 1), 
  homescore = round(teamscore + offhomeadv + defhomeadv, 1),
  offense = round(colMeans(res$offense), 1),
  defense = round(colMeans(res$defense), 1),
  offhomeadv = round(offhomeadv, 1),
  defhomeadv = round(defhomeadv, 1))

cfbstat <- cfbstat[order(cfbstat$teamscore, decreasing = T),]
row.names(cfbstat) <- 1:nrow(cfbstat)
cfbstat$week <- 11

cfbpred <- data.frame(
  visitor = at,
  home = ht,
  vis_pred = round(colMeans(res$away_pred), 0),
  hom_pred = round(colMeans(res$home_pred), 0),
  ovrund = round(colMeans(res$away_pred) + 
                   colMeans(res$home_pred), 0),
  spread = round(colMeans(res$diff),0),
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

cfbpred$vis_score <- c(13,  6, 27, 38, 10, 37, 23, 52, 45,  3, 
                       21, 21, 56, 19, 20, 58, 34,  3,  9, 41, 
                       22, 34, 42, 37,  3, 31, 27, 27, 10,  7,
                       12,  6, 33, 21,  6)
cfbpred$hom_score <- c(31, 34, 34,  7, 44, 21,  6, 20, 27, 52, 
                       23, 14, 21, 23, 17, 37, 31, 49, 42, 17, 
                       49, 35, 14, 34, 44, 28, 30, 27, 31, 17,
                       49, 45, 31, 29, 47)
cfbpred$week <- 11

compare <- data.frame(
  preds = c(cfbpred$vis_pred, cfbpred$hom_pred),
  obsv  = c(cfbpred$vis_score, cfbpred$hom_score),
  team  = factor(rep(c("visitor", "home"), each=35)))

scorepredsplot <- ggplot(compare, aes(x = preds, y=obsv, colour = team)) +
  geom_point(size=3) +
  theme_minimal() +
  scale_color_manual(values = c("#f1a340", "#998ec3")) +
  ylab("Final Score") + xlab("Predicted") +
  ggtitle("Week 12 score predictions")

#ggsave(filename = "week12/scorepreds_w12.png", plot = scorepredsplot, units = 'in', width = 5, height = 4)

# spreadplot <- ggplot(cfbpred, aes(x=spread, y=vis_score - hom_score)) +
#   geom_point(size=3) +
#   theme_minimal() +
#   coord_fixed(xlim = c(-55, 40), ylim = c(-55, 40)) +
#   xlab("predicted spread") + ylab("away team - home team") +
#   ggtitle("week 12 margin of victory")

# ggsave(filename = "week12/MOV_w13.png", plot = spreadplot, units = 'in', width = 4, height = 4)
write.table(cfbpred, file = "week12/cfbpred13.txt", sep = ',') 
write.table(cfbstat, file = "week12/rankings_w13.txt", sep = ',')
cfbstat[1:25,]
gc()

p13 <- subset(allgames, week <= 12)
#head(p13)

teams <- unique(c(
  as.character(teams),
  as.character(p13$awayteam), 
  as.character(p13$hometeam)))
teams <- factor(teams[order(teams)])

awayteams <- integer(nrow(p13))
for (i in seq_along(p13$awayteam)) {
  awayteams[i] <- which(teams %in% as.character(p13$awayteam[i]))
}
hometeams <- integer(nrow(p13))
for (i in seq_along(p13$awayteam)) {
  hometeams[i] <- which(teams %in% as.character(p13$hometeam[i]))
}

at = c("EMI", "OH", "TOL", "AKR", "NCS", "COS", "BY", "ECARO", "CFL", "MIS",
       "KS", "OKS", "MN", "WCU", "LIB", "PAS", "IL", "BALL", "SAL", "AIRF",
       "BOSCO", "TXS", "GASO", "UCLA", "SMU", "WKY", "PITT", "TNM", "ETNS",
       "TX", "MARSH", "NE", "TAM", "NTX", "MI", "SJS", "UTEP", "CA", "PURD",
       "CUSE", "MEMPH", "OLD", "COAST", "TROY", "FLAT", "OR", "TEMP", "KSS", "UMIA",
       "AR", "TN", "DUKE", "ACU", "HOUS", "TCU", "ORS", "UT", "WA", "NV",
       "BOIS", "SDS")
ht = c("NIL", "BOWL", "BUFF", "MIOH", "GAT", "WY", "UMASS", "CT", "TULN", "R", 
       "IAS", "WV", "NORW", "AL", "VA", "OHS", "IA", "KENT", "GAS", "NM",
       "NODA", "APPS", "ARS", "SC", "NAVY", "SMS", "VAT", "KY", "VAND",
       "BAYL", "CHARL", "MD", "GA", "RICE", "IN", "UNLV", "NMS", "ST", "WI",
       "LOUIS", "SFL", "MIDTN", "LAM", "LALA", "UTSA", "AZS", "CIN", "TXT", "FIU",
       "LAS", "MO", "WF", "MSS", "TULS", "OK", "WAS", "AZ", "CO", "FREST",
       "UTS", "HI")

ati <- integer(length(at))
for (i in seq_along(at)) {
  ati[i] <- which(teams %in% at[i])
}
hti <- integer(length(ht))
for (i in seq_along(ht)) {
  hti[i] <- which(teams %in% ht[i])
}

w13_data <- list(
  ngames = nrow(p13),
  nteams = length(teams),
  npreds = length(ati),
  awayscore = p13$awayscore,
  homescore = p13$homescore,
  awayteam = awayteams,
  hometeam = hometeams,
  ati = ati,
  hti = hti
)

gc()
week13_posterior <- stan(
  model_code = modelstring2,
  data = w13_data,
  iter = 110000,
  warmup = 10000,
  thin = 400,
  chains = 4
)

res13 <- rstan::extract(week13_posterior)

teamscore <- colMeans(res13$offense)+colMeans(res13$defense)
offhomeadv <- colMeans(res13$homeoff)
defhomeadv <- colMeans(res13$homedef)

cfbstat13 <- data.frame(
  team = teams, 
  teamscore = round(teamscore, 1), 
  homescore = round(
    teamscore + 
      colMeans(res13$offense) + 
      colMeans(res13$defense)),
  offense = round(colMeans(res13$offense), 1),
  defense = round(colMeans(res13$defense), 1),
  offhomeadv = round(offhomeadv, 1),
  defhomeadv = round(defhomeadv, 1))


cfbstat13 <- cfbstat13[order(cfbstat13$teamscore, decreasing = T),]
row.names(cfbstat13) <- 1:nrow(cfbstat13)
cfbstat13$week <- 12
cfbstat <- rbind(cfbstat, cfbstat13)

head(cfbstat13)
save.image()
cfbpred13 <- data.frame(
  visitor = at,
  home = ht,
  vis_pred = round(colMeans(res13$away_pred), 0),
  hom_pred = round(colMeans(res13$home_pred), 0),
  ovrund = round(colMeans(res13$away_pred) + 
                   colMeans(res13$home_pred), 0),
  spread = round(colMeans(res13$diff),0),
  low = round(apply(
    res13$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.025)), 0),
  hi  = round(apply(
    res13$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.975)), 0),
  home_win_prob = apply(
    res13$diff, MARGIN = 2, 
    FUN = function(x) paste(
      round(sum(x<0)/length(x) * 100, 1),
      "%"))
)

cfbpred13$vis_score <- c(45, 66, 30, 17, 26,  7, 56, 31, 34, 27, 
                         31, 20, 38,  3, 27, 17, 10, 38, 15, 44,
                          7, 13, 33, 35, 28, 28,  0,  7,  0, 10, 
                         13, 54, 13, 14, 39, 35, 35, 24, 24, 34,
                         49, 17, 42,  3, 40, 28, 13, 30, 24, 20,
                         24, 27,  7, 24, 24, 53, 35, 14, 35, 56,
                         11)
cfbpred13$hom_score <- c(17, 24, 49, 20, 28, 17, 24, 24, 31,  0, 
                         41, 13, 22, 66, 55, 28, 19, 41, 28, 22, 
                         40, 35, 38, 52, 35, 10, 28, 50, 38, 24, 
                         24,  7, 19, 20, 14, 38, 44, 20, 45, 56,
                         10, 38, 45, 53, 26, 31, 15, 27, 30, 56,
                         20, 39, 45, 14, 28, 54,  7, 20, 28, 21, 
                         14)
cfbpred13$week <- 12
cfbpred <- rbind(cfbpred, cfbpred13)

p <- ggplot(
  cfbpred, 
  aes(x=spread, 
      y=vis_score-hom_score, 
      color=factor(week))) + 
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("#d7191c", "#1a9641")) + 
  ylab("visitor - home") + 
  xlab("prediction") + 
  ggtitle("margin of victory")
ggsave(filename = "week13/predictplot.png", plot = p, units = 'in', width = 5, height = 4)

write.table(cfbstat, file = "week13/week14rankings.csv", sep = ',')

matchup <- function(team1, team2, posterior) {
  res <- posterior
  nsamples <- nrow(res$offense)
  mean_score <- mean(c(allgames$awayscore, allgames$homescore))
  team1idx <- which(teams %in% as.character(team1))
  team2idx <- which(teams %in% as.character(team2))
  team1off <- res$offense[,team1idx]
  team2off <- res$offense[,team2idx]
  team1def <- res$defense[,team1idx]
  team2def <- res$defense[,team2idx]
  team1offhome <- res$homeoff[,team1idx]
  team2offhome <- res$homeoff[, team2idx]
  team1defhome <- res$homedef[,team1idx]
  team2defhome <- res$homedef[,team2idx]
  team1.at.home <- mean_score + team1off - team2def + team1offhome
  team2.at.home <- mean_score + team2off - team1def + team2offhome
  team1.away <- mean_score + team1off - team2def - team2defhome
  team2.away <- mean_score + team2off - team1def - team1defhome
  team1.neutral <- mean_score + team1off - team2def
  team2.neutral <- mean_score + team2off - team1def
  allresults <- data.frame(
    score = c(team1.at.home-team2.away, team1.away-team2.at.home, team1.neutral-team2.neutral, team1.at.home-team2.at.home),
    venue = rep(c(
      paste(team1, "-at-home", sep=''), 
      paste(team2, "-at-home", sep=''), 
      "neutral-site", "playoff-mode"), each = nsamples)
    )
  p<-ggplot(allresults, aes(x=score, y=venue)) + 
    geom_density_ridges(fill="grey", alpha=.5) + 
    theme_minimal() +
    geom_vline(xintercept = 0, ) +
    xlab(paste(team1, "margin of victory"))
  print(p)
  data.frame(
    venue = c(
      paste(team1, "-at-home", sep=''),
      paste(team1, "-visitor", sep=''),
      "neutral-site", "playoff-mode"),
    awayteam = c(team2, team1, team2, team2),
    hometeam = c(team1, team2, team1, team1),
    awayscore = round(c(
      mean(team2.away), 
      mean(team1.away), 
      mean(team2.neutral),
      mean(team2.at.home)), 0),
    homescore = round(c(
      mean(team1.at.home), 
      mean(team2.at.home), 
      mean(team1.neutral),
      mean(team1.at.home)), 0),
    MOV = round(c(
      mean(team2.away-team1.at.home),
      mean(team1.away-team2.at.home),
      mean(team2.neutral-team1.neutral),
      mean(team2.at.home-team1.at.home)), 0),
    MOV_lo = round(as.numeric(c(
      quantile(team2.away-team1.at.home, p=c(0.025)),
      quantile(team1.away-team2.at.home, p=c(0.025)),
      quantile(team2.neutral-team1.neutral, p=c(0.025)),
      quantile(team2.at.home-team1.at.home, p=c(0.025)))), 0),
    MOV_hi = round(as.numeric(c(
      quantile(team2.away-team1.at.home, p=c(0.975)),
      quantile(team1.away-team2.at.home, p=c(0.975)),
      quantile(team2.neutral-team1.neutral, p=c(0.975)),
      quantile(team2.at.home-team1.at.home, p=c(0.975)))), 0),
    vistr_win_prob = paste(100 * as.numeric(c(
      sum(team2.away-team1.at.home>0)/nsamples,
      sum(team1.away-team2.at.home>0)/nsamples,
      sum(team2.neutral-team1.neutral>0)/nsamples,
      sum(team2.at.home-team1.at.home>0)/nsamples)), "%")
  )
}


png("week13/UTvOR.png")
matchup("OR", "UCLA", res13)
dev.off()

cfp <- matchup("AL", "LAS", res13)
cfp <- rbind(cfp, matchup("AL", "CLEM", res13))
cfp <- rbind(cfp, matchup("CLEM", "LAS", res13))
cfp <- rbind(cfp, matchup("OHS", "LAS", res13))
cfp <- rbind(cfp, matchup("CLEM", "OHS", res13))
cfp <- rbind(cfp, matchup("OHS", "AL", res13))
subset(cfp, venue=="neutral-site")


#####################
###### WEEK 14 ######
#####################

p14 <- subset(allgames, week <= 13)
#head(p14)

teams <- unique(c(
  as.character(teams),
  as.character(p14$awayteam), 
  as.character(p14$hometeam)))
teams <- factor(teams[order(teams)])

awayteams <- integer(nrow(p14))
for (i in seq_along(p14$awayteam)) {
  awayteams[i] <- which(teams %in% as.character(p14$awayteam[i]))
}
hometeams <- integer(nrow(p14))
for (i in seq_along(p14$awayteam)) {
  hometeams[i] <- which(teams %in% as.character(p14$hometeam[i]))
}

at = c("OH", "WMI", "MS", "BOWL", "TXT", "MIOH", "KENT", "TOL", "VAT", "IA",
       "MO", "CIN", "BOIS", "WAS", "WV", "ARS", "APPS", "SFL", "CLEM", "TXS",
       "OHS", "GA", "IN", "NORW", "FIU", "LOUIS", "TULS", "WF", "CHARL", "MIDTN",
       "NMS", "WY", "RICE", "UNLV", "WI", "BOSCO", "SMS", "UMIA", "BAYL", "R",
       "AL", "CT", "NODA", "TULN", "VAND", "ORS", "UTS", "ALB", "GAS", "IAS",
       "NAVY", "NC", "TAM", "LAM", "CO", "FLS", "OK", "BY", "AZ", "FREST", 
       "CA", "ARMY")
ht = c("AKR", "NIL", "MSS", "BUFF", "TX", "BALL", "EMI", "CMI", "VA", "NE",
       "AR", "MEMPH", "COS", "WA", "TCU", "SAL", "TROY", "CFL", "SCARO", "COAST",
       "MI", "GAT", "PURD", "IL", "MARSH", "KY", "ECARO", "CUSE", "OLD", "WKY",
       "LIB", "AIRF", "UTEP", "NV", "MN", "PITT", "FLAT", "DUKE", "KS", "PAS",
       "AUB", "TEMP", "ST", "SMU", "TN", "OR", "NM", "NTX", "GASO", "KSS",
       "HOUS", "NCS", "LAS", "LALA", "UT", "FL", "OKS", "SDS", "AZS", "SJS",
       "UCLA", "HI")

ati <- integer(length(at))
for (i in seq_along(at)) {
  ati[i] <- which(teams %in% at[i])
}
hti <- integer(length(ht))
for (i in seq_along(ht)) {
  hti[i] <- which(teams %in% ht[i])
}

w14_data <- list(
  ngames = nrow(p14),
  nteams = length(teams),
  npreds = length(ati),
  awayscore = p14$awayscore,
  homescore = p14$homescore,
  awayteam = awayteams,
  hometeam = hometeams,
  ati = ati,
  hti = hti
)
gc()
week14_posterior <- stan(
  model_code = modelstring2,
  data = w14_data,
  iter = 110000,
  warmup = 10000,
  thin = 400,
  chains = 4
)


res14 <- rstan::extract(week14_posterior)

teamscore <- colMeans(res14$offense)+colMeans(res14$defense)
offhomeadv <- colMeans(res14$homeoff)
defhomeadv <- colMeans(res14$homedef)

cfbstat14 <- data.frame(
  team = teams, 
  teamscore = round(teamscore, 1), 
  homescore = round(teamscore + offhomeadv + defhomeadv, 1),
  offense = round(colMeans(res14$offense), 1),
  defense = round(colMeans(res14$defense), 1),
  offhomeadv = round(offhomeadv, 1),
  defhomeadv = round(defhomeadv, 1))

cfbstat14 <- cfbstat14[order(cfbstat14$teamscore, decreasing = T),]
row.names(cfbstat14) <- 1:nrow(cfbstat14)
levels(teams)
cfbpred14 <- data.frame(
  visitor = at,
  home = ht,
  vis_pred = round(colMeans(res14$away_pred), 0),
  hom_pred = round(colMeans(res14$home_pred), 0),
  ovrund = round(colMeans(res14$away_pred) + 
                   colMeans(res14$home_pred), 0),
  spread = round(colMeans(res14$diff),0),
  low = round(apply(
    res14$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.025)), 0),
  hi  = round(apply(
    res14$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.975)), 0),
  home_win_prob = apply(
    res14$diff, MARGIN = 2, 
    FUN = function(x) paste(
      round(sum(x<0)/length(x) * 100, 1),
      "%"))
)

save.image()
cfbpred14
cfbstat14[1:25,]

#####################
###### WEEK 15 ######
#####################

p15 <- subset(allgames, week <= 14)
#head(p14)

teams <- unique(c(
  as.character(teams),
  as.character(p15$awayteam), 
  as.character(p15$hometeam)))
teams <- factor(teams[order(teams)])

awayteams <- integer(nrow(p15))
for (i in seq_along(p15$awayteam)) {
  awayteams[i] <- which(teams %in% as.character(p15$awayteam[i]))
}
hometeams <- integer(nrow(p15))
for (i in seq_along(p15$awayteam)) {
  hometeams[i] <- which(teams %in% as.character(p15$hometeam[i]))
}

at = c("OR", "BAYL", "LALA", "CIN", "GA", "HI", "VA", "OHS")
ht = c("UT", "OK", "APPS", "MEMPH", "LAS", "BOIS", "CLEM", "WI")

ati <- integer(length(at))
for (i in seq_along(at)) {
  ati[i] <- which(teams %in% at[i])
}
hti <- integer(length(ht))
for (i in seq_along(ht)) {
  hti[i] <- which(teams %in% ht[i])
}

w15_data <- list(
  ngames = nrow(p15),
  nteams = length(teams),
  npreds = length(ati),
  awayscore = p15$awayscore,
  homescore = p15$homescore,
  awayteam = awayteams,
  hometeam = hometeams,
  ati = ati,
  hti = hti
)
gc()
week15_posterior <- stan(
  model_code = modelstring2,
  data = w15_data,
  iter = 110000,
  warmup = 10000,
  thin = 400,
  chains = 4
)

res15 <- rstan::extract(week15_posterior)

teamscore <- colMeans(res15$offense)+colMeans(res15$defense)
offhomeadv <- colMeans(res15$homeoff)
defhomeadv <- colMeans(res15$homedef)

cfbstat15 <- data.frame(
  team = teams, 
  teamscore = round(teamscore, 1), 
  homescore = round(teamscore + offhomeadv + defhomeadv, 1),
  offense = round(colMeans(res15$offense), 1),
  defense = round(colMeans(res15$defense), 1),
  offhomeadv = round(offhomeadv, 1),
  defhomeadv = round(defhomeadv, 1))

cfbstat15 <- cfbstat15[order(cfbstat15$teamscore, decreasing = T),]
row.names(cfbstat15) <- 1:nrow(cfbstat15)

cfbpred15 <- data.frame(
  visitor = at,
  home = ht,
  vis_pred = round(colMeans(res15$away_pred), 0),
  hom_pred = round(colMeans(res15$home_pred), 0),
  ovrund = round(colMeans(res15$away_pred) + 
                   colMeans(res15$home_pred), 0),
  spread = round(colMeans(res15$diff),0),
  low = round(apply(
    res15$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.025)), 0),
  hi  = round(apply(
    res15$diff, MARGIN = 2,
    FUN = function(x) quantile(x, 0.975)), 0),
  home_win_prob = apply(
    res15$diff, MARGIN = 2, 
    FUN = function(x) paste(
      round(sum(x<0)/length(x) * 100, 1),
      "%"))
)

save.image()
cfbpred15
cfbstat15[1:25,]
png("OR_UT_TITLE_PRED.png")
matchup("OR", "UT", res15)
dev.off()
