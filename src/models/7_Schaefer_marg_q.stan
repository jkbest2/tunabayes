data {
  int<lower=0> T; // number of years
  real C[T];      // Catch biomass
  real I[T];      // CPUE index
}
parameters {
  real<lower=0> r;        // Intrinsic growth rate
  real<lower=0> K;        // Carrying capacity
  real<lower=0> sigma2;   // Process variance
  real<lower=0> tau2;     // Observation variance
  real<lower=0> P[T];     // Estimated population
}

transformed parameters {
  real<lower=0> sigma;
  real<lower=0> tau;

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);
}

model {
  vector[T] Pmed;
  vector[T] Imed;
  vector[T] Z;
  real log_q_hat;

  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);

  // Set initial state
  Pmed[1] = 0;
  P[1] ~ lognormal(Pmed[1], sigma);

  // time steps of the model
  for (t in 2:T) {
    Pmed[t] = log(fmax(P[t - 1] + (r * P[t - 1]) * (1 - P[t - 1]) -
                       C[t - 1] / K,
                       0.00001));
    P[t] ~ lognormal(Pmed[t], sigma);
  }

  // Calculate "log q" for each year
  for (t in 1:T) {
    Z[t] = log(I[t]) - log(P[t]) - log(K);
  }
  log_q_hat = mean(Z);

  // Likelihood
  Z ~ normal(log_q_hat, tau);
}

/* generated quantities { */
/*   vector[T] Imed; */
/*   vector[T] Inew; */
/*   vector[T + 1] Biomass; */
/*   real P24; */
/*   real MSY; */
/*   real EMSY; */

/*   //posterior predictions (hint, the parameterization of dlnorm is not the same as in R) */
/*   for (t in 1:T) { */
/*     Imed[t] = log(q * K * P[t]); */
/*     Inew[t] = lognormal_rng(Imed[t], tau); */
/*     Biomass[t] = K * P[t]; */
/*   } */
/*   P24 = P[T] + r * P[T] * (1 - P[T]) - C[T] / K; */
/*   Biomass[T + 1] = K * P24; */

/*   // Other ouputs */
/*   MSY = r * K / 4; */
/*   EMSY = r / (2 * q); */
/* } */

