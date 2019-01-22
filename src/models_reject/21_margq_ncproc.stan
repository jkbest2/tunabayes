data {
  int<lower=0> T;                  // Number of years
  real C[T];                       // Catch biomass
  real I[T];                       // CPUE index
}
parameters {
  real<lower=0> r;                 // Intrinsic growth rate
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> sigma2;            // Process variance
  real<lower=0> tau2;              // Observation variance
  real u[T];                       // Process log-deviations
}

transformed parameters {
  real<lower=0> sigma;
  real<lower=0> tau;
  real<lower=0> Pmed[T];
  real<lower=0> P[T];
  real Z[T];
  real log_q_hat;

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  // Noncentered population deviations
  Pmed[1] = 1;
  P[1] = Pmed[1] * exp(u[1] * sigma);
  for (t in 2:T) {
    Pmed[t] = P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) - C[t - 1] / K;
    if (Pmed[t] < 0) reject("Negative depletion");
    P[t] = Pmed[t] * exp(u[t] * sigma);
  }

  // Calculate "log q" for each year
  for (t in 1:T) {
    Z[t] = log(I[t]) - log(P[t]) - log(K);
  }
  log_q_hat = mean(Z);
}

model {
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);

  u ~ normal(0, 1);

  // Likelihood
  /* NOTE: This will give a warning about using a transformed variable on the
   * left-hand side of a sapling statement. I'm pretty sure I don't need a
   * Jacobian adjustment here, but this is definitely the line causing the
   * warning.*/
  Z ~ normal(log_q_hat, tau);
}

generated quantities {
/*   vector[T] Imed; */
/*   vector[T] Inew; */
  vector[T + 1] Biomass;
  real P24;
  real MSY;
  /* real EMSY; */

  // Posterior predictions (hint, the parameterization of dlnorm is not the same
  // as in R)
  for (t in 1:T) {
/*     Imed[t] = log(q * K * P[t]); */
/*     Inew[t] = lognormal_rng(Imed[t], tau); */
    Biomass[t] = K * P[t];
  }
  P24 = P[T] + r * P[T] * (1 - P[T]) - C[T] / K;
  Biomass[T + 1] = K * P24;

  // Other ouputs
  MSY = r * K / 4;
  /* EMSY = r / (2 * q); */
}
