data {
  int<lower=0> T;                  // Number of years
  real C[T];                       // Observed catch
  real I[T];                       // CPUE index
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> q;                 // Catchability
  real<lower=0> sigma2;            // Process variance
  real<lower=0> tau2;              // Observation variance
  real u[T];                       // Process log-deviations
}

transformed parameters {
  real<lower=0> sigma;
  real<lower=0> tau;
  real<lower=0> Pmed[T];
  real<lower=0> P[T];

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  // Noncentered population deviations
  Pmed[1] = 1;
  P[1] = Pmed[1] * exp(u[1] * sigma);
  for (t in 2:T) {
    Pmed[t] = P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) - C[t - 1] / K;
    if (Pmed[t] < 0) reject("Negative depletion")
    P[t] = Pmed[t] * exp(u[t] * sigma);
  }
}

model {
  // Priors
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);
  target += -log(q);

  u ~ normal(0, 1);

  for (t in 1:T) {
    I[t] ~ lognormal(q * K * P[t], tau);
  }
}

generated quantities {
  vector[T] Imed;
  vector[T] Ipred;
  vector[T] Biomass;
  real MSY;
  real EMSY;

  //posterior predictions (hint, the parameterization of dlnorm is not the same as in R)
  for (t in 1:T) {
    Imed[t] = q * K * P[t];
    Ipred[t] = lognormal_rng(Imed[t], tau);
    Biomass[t] = K * P[t];
  }

  // Other ouputs
  MSY = r * K / 4;
  EMSY = r / (2 * q);
}

