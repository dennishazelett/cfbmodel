//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> ngames;
  int<lower=0> nteams;
  int<lower=0> awayscore[ngames];
  int<lower=0> homescore[ngames];
  int<lower=0> awayteam[ngames];
  int<lower=0> hometeam[ngames];
  int<lower=0> OT[ngames];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real offense[nteams];
  real defense[nteams];
  real homeadv[nteams];
  real otscore;
  real stdev;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (j in 1:nteams) {
    offense[j] ~ normal(10, 3);
    defense[j] ~ normal(10, 3);
    homeadv[j] ~ normal(0, 3);
  }
  otscore ~ normal(7, 1);
  stdev ~ gamma(3.0, 1.0);
  for (i in 1:ngames) {
    awayscore[i] ~ normal(offense[awayteam[i]] 
      - defense[hometeam[i]]
      - homeadv[hometeam[i]]
      + otscore * OT[i], stdev);
    homescore[i] ~ normal(offense[hometeam[i]]
      - defense[awayteam[i]]
      + homeadv[hometeam[i]]
      + otscore * OT[i], stdev);
  }
}

