data {
  int<lower=0> T;                  // Number of years
  real C[T];                       // Observed catch
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
  real<lower=0> u[T];              // Process log-deviations
}

transformed parameters {
  real<lower=0> sigma;             // Transform to standard deviation
  real<lower=0> tau;               // Transform to standard deviation
  real<lower=0> P[T];              // Predicted depletion series
  real<lower=0> C_pred[T];         // Predicted catch series
  real<lower=0> I_med[T];          // Median of CPUE index given P_t etc.

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  P[1] = exp(u[1] * sigma);
  I_med[1] = log(q * K * P[1]);
  for (t in 2:T) {
    // Catch occurs after production
    P[t] = (P[t - 1] + r * P[t - 1] * (1 - P[t - 1])) *
      (1 - exp(-F[t - 1])) * exp(u[t] * sigma);
    if (P[t] < 0) reject("Negative depletion");
    C_pred[t] = K * P[t] * exp(-F[t]);
    I_med[t] = log(q * K * P[t]);
  }
}

model {
  // These priors match those specified in Meyer & Millar
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);

  // Noncentered process deviation prior
  u ~ normal(0, 1);

  // Likelihoods
  I ~ lognormal(I_med, tau);
  C ~ normal(C_pred, catch_sd);
}

generated quantities {
  vector[T] I_new;
  vector[T + 1] Biomass;
  real u_24;
  real P_24;
  real MSY;
  real EMSY;

  //posterior predictions (hint, the parameterization of dlnorm is not the same as in R)
  for (t in 1:T) {
    I_new[t] = lognormal_rng(I_med[t], tau);
    Biomass[t] = K * P[t];
  }
  u_24 = normal_rng(0, 1);
  P_24 = (P[T] + r * P[T] * (1 - P[T])) *
    (1 - exp(-F[T])) * exp(u_24 * sigma);
  Biomass[T + 1] = K * P_24;

  // Management values
  MSY = r * K / 4;
  EMSY = r / (2 * q);
}
