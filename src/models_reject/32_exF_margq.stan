data {
  int<lower=0> T;                  // Number of years
  vector[T] C;                     // Observed catch
  vector[T] I;                     // CPUE index
  real catch_sd;                   // Standard deviation of catch obs; no data
                                   // to inform this so pass as data. Could be
                                   // worth trying a series of values.
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  vector<lower=0>[T] F;            // Instantaneous fishing mortality
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  vector<lower=0>[T] P;            // Predicted depletion
}

transformed parameters {
  real<lower=0> sigma;             // Transform to standard deviation
  real<lower=0> tau;               // Transform to standard deviation
  vector[T] P_med;                 // Median depletion; no process error
  vector[T] C_pred;                // Predicted catch, after error
  vector[T] Z;                     // Per year q MLE; follow the notation of
                                   // Walters and Ludwig 1994
  real log_q_hat;                  // MLE of q

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  // Initial depletion and catch. Note that catch occurs *after* production
  // here.
  P_med[1] = 1;
  C_pred[1] = K * (P[1] + r * P[1] * (1 - P[1])) * exp(-F[1]);
  for (t in 2:T) {
    // Again, catch occurs after production. On the depletion scale we can
    // multiply by 1 - exp(-F) to get the fraction of biomass that does *not*
    // experience fishing mortality. Last year's depletion, production, and then
    // catch determine the median of the current year's depletion.
    P_med[t] = (P[t - 1] + r * P[t - 1] * (1 - P[t - 1])) *
      (1 - exp(-F[t - 1]));
    if (P_med < 0) reject("Negative depletion");
    // And current year catch depends on current depletion
    C_pred[t] = K * (P[t] + r * P[t] * (1 - P[t])) * exp(-F[t]);
  }

  // Calculate "log q" for each year
  for (t in 1:T) {
    Z[t] = log(I[t]) - log(P[t]) - log(K);
  }
  log_q_hat = mean(Z);
}

model {
  // Priors specified in Meyer and Millar 1999
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);

  // Likelihoods
  P ~ lognormal(log(P_med), sigma);
  C ~ normal(C_pred, catch_sd);
  /* NOTE: This will give a warning about using a transformed variable on the
   * left-hand side of a sapling statement. I'm pretty sure I don't need a
   * Jacobian adjustment here, but this is definitely the line causing the
   * warning.*/
  Z ~ normal(log_q_hat, tau);
}

generated quantities {
  vector[T + 1] Biomass;           // Biomass series with one step ahead prediction
  real P_medfinal;                 // One step ahead median depletion
  real P_final;                    // One step ahead depletion
  real MSY;                        // Maximum sustainable yield
  real FMSY;                       // Fishing mortality to achieve MSY

  // Calculate biomass at each time step from depletion and K, then simulate
  // one step ahead and include that final biomass
  for (t in 1:T) {
    Biomass[t] = K * P[t];
  }
  // One-step-ahead projection, including process error
  P_medfinal = (P[T] + r * P[T] * (1 - P[T])) * (1 - exp(-F[T]));
  P_final = lognormal_rng(log(P_medfinal), sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  MSY = r * K / 4;
  FMSY = r / 2;
}
