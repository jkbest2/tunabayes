data {
  int<lower=0> T;                  // Number of years
  real C[T];                   // Observed catch
  real I[T];                       // CPUE index
  real catch_sd;                   // Standard deviation of catch obs; no data
                                   // to inform this so pass as data. Could be
                                   // worth trying a series of values.
}
parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> q;                 // Catchability
  real<lower=0> F[T];              // Instantaneous fishing mortality
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  real<lower=0> P[T];              // Predicted depletion
}

transformed parameters {
  real<lower=0> sigma;             // Transform to standard deviation
  real<lower=0> tau;               // Transform to standard deviation

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);
}

model {
  vector[T] Pmed;
  vector[T] Imed;
  vector[T] C_pred;

  // These priors match those specified in Meyer & Millar
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);
  /* F ~ normal(0, 3); */

  // Set initial state
  Pmed[1] = 1;
  P[1] ~ lognormal(log(Pmed[1]), sigma);
  C_pred[1] = K * P[1] * exp(-F[1]);

  // time steps of the model
  for (t in 2:T) {
    // Catch occurs after production
    Pmed[t] = (P[t - 1] + r * P[t - 1] * (1 - P[t - 1])) *
      (1 - exp(-F[t - 1]));
    P[t] ~ lognormal(log(Pmed[t]), sigma);
    C_pred[t] = K * P[t] * exp(-F[t]);
  }

  for (t in 1:T) {
    // CPUE likelihood
    Imed[t] = log(q * K * P[t]);
    I[t] ~ lognormal(Imed[t], tau);

    // Catch likelihood
    C[t] ~ normal(C_pred[t], catch_sd);
  }
}

generated quantities {
  vector[T] Imed;
  vector[T] Inew;
  vector[T] C_pred;
  vector[T + 1] Biomass;
  real P24;
  real MSY;
  real EMSY;

  //posterior predictions (hint, the parameterization of dlnorm is not the same as in R)
  for (t in 1:T) {
    Imed[t] = log(q * K * P[t]);
    Inew[t] = lognormal_rng(Imed[t], tau);
    C_pred[t] = K * P[t] * exp(-F[t]);
    Biomass[t] = K * P[t];
  }
  P24 = P[T] + r * P[T] * (1 - P[T]) - C_pred[T] / K;
  Biomass[T + 1] = K * P24;

  // Management values
  MSY = r * K / 4;
  EMSY = r / (2 * q);
}

