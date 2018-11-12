data {
  int<lower=0> T; // Number of years
  real C[T];      // Catch
  real I[T];      // CPUE index
}
parameters {
  real<lower=0.01,upper=1.2> r;
  real<lower=10,upper=1000> K;
  real<lower=0.5,upper=100> iq;
  real<lower=0> isigma2;
  real<lower=0> itau2;
  vector<lower=0.05,upper=1.6>[T] P;
}

transformed parameters {
  real sigma;
  real tau;
  real q;

  sigma = 1.0 / sqrt(isigma2);
  tau = 1.0 / sqrt(itau2);
  q = 1 / iq;
}

model {
  vector[T] Pmed;
  vector[T] Imed;

  r ~ lognormal(-1.38, 1 / sqrt(3.845)) T[0.01, 1.2];
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664)) T[10, 1000];
  iq ~ gamma(0.001, 0.001) T[0.5,100];
  isigma2 ~ gamma(3.785518, 0.010223);
  itau2 ~ gamma(1.708603, 0.008613854);

  // Set initial state
  Pmed[1] = 0;
  P[1] ~ lognormal(Pmed[1], sigma) T[0.05, 1.6];

  // Time steps of the model
  for (t in 2:T) {
    Pmed[t] = log(fmax(P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) -
                       C[t - 1] / K,
                       0.001));
    P[t] ~ lognormal(Pmed[t], sigma) T[0.05, 1.6];
  }

  // Likelihood
  for (t in 1:T) {
    Imed[t] = log(q * K * P[t]);
    I[t] ~ lognormal(Imed[t], tau);
  }
}

generated quantities {
  vector[T] Imed;
  vector[T] Inew;
  vector[T] Biomass;
  real MSY;
  real EMSY;

  //posterior predictions (hint, the parameterization of dlnorm is not the same as in R)
  for (t in 1:T) {
    Imed[t] = log(q * K * P[t]);
    Inew[t] = lognormal_rng(Imed[t], tau);
    Biomass[t] = K * P[t];
  }

  // Other ouputs
  MSY = r * K / 4;
  EMSY = r / (2 * q);
}

