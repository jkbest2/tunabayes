functions {
  /* We know a priori that the population does not go extinct during the time
   * period being modeled. This can be enforced by ensuring that depletion is
   * sufficient for the predicted depletion in the next step to be positive.
   * These functions provide the lower and upper bounds for each P_t. */
  vector P_min(real r, real K, vector C, int T) {
    vector[T] P_lower;
    for (t in 1:T) {
      P_lower[t] = ((1 + r) - sqrt((1 + r)^2 - 4 * r * C[t] / K)) / (2 * r);
    }
    return P_lower;
  }
  vector P_max(real r, real K, vector C, int T) {
    vector[T] P_upper;
    for (t in 1:T) {
      P_upper[t] = ((1 + r) + sqrt((1 + r)^2 - 4 * r * C[t] / K)) / (2 * r);
    }
    return P_upper;
  }

  // Define a single step forward in time using Schaefer dynamics. This
  // uses the parameterization presented in Winker et al. 2018.
  real schaefer(real P0, real r, real K, real C) {
    return P0 + r * P0 * (1 - P0) - C / K;
  }

  // Calculate BMSY
  real sch_bmsy(real K) {
    return K / 2;
  }

  // Calculate FMSY
  real sch_fmsy(real r) {
    return r / 2;
  }

  // Use above to calculate MSY
  real sch_msy(real r, real K) {
    return r * K / 4;
  }
}

data {
  int<lower=0> T;                  // Number of years
  vector[T] C;                     // Observed catch
  vector[T] I;                     // CPUE index
}

parameters {
  real<lower=0> r; // Population growth
  real<lower=0> K; // Carrying capacity
  real<lower=0> q;                 // Catchability
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  vector<lower=0,upper=1>[T] P_raw; // Predicted depletion (state variable).
                                    // This is constrained between zero and one
                                    // to make the transformation to the actual
                                    // constraints (calculated below as P_lower
                                    // and P_upper) more straightforward.
                                    // Modeled after section 16.4 in the Stan
                                    // User's Guide.
}

transformed parameters {
  real<lower=0> sigma;             // Transform to standard deviation
  real<lower=0> tau;               // Transform to standard deviation
  vector[T] P;                     // Depletion (state variable)
  vector[T] P_lower;               // Lower bounds to ensure P stays positive
  vector[T] P_upper;               // Upper bounds to ensure P stays positive
  vector[T] P_med;                 // Median depletion; no process error

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  P_lower = P_min(r, K, C, T);
  P_upper = P_max(r, K, C, T);
  P = P_lower + (P_upper - P_lower) .* P_raw;

  // Initial depletion
  P_med[1] = 1;
  for (t in 2:T) {
    P_med[t] = schaefer(P[t - 1], r, K, C[t - 1]);
  }
}

model {
  vector[T] J_corr;                // Jacobian correction for P_raw -> P

  // Priors from Meyer & Millar 1999
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);

  for (t in 1:T) {
    // Add the log-Jacobian correction for the change of variables from P_raw to
    // P. This is required because the bounds depend on the parameters r and K.
    J_corr[t] = 0.5 * log((1 + r)^2 - 4 * r * C[t] / K) - log(r);
    target += J_corr[t];
    // Process likelihood
    // The `log` here is the practical reason to prevent `P_med[t]` from going
    // negative. The Jacobian correction is necessary here because the
    // constraints depend on other parameters. This also makes it necessary to
    // declare the truncations in the sampling statement.
    P[t] ~ lognormal(log(P_med[t]), sigma);
  }

  // Observation likelihood
  I ~ lognormal(log(q * K * P), tau);
}

generated quantities {
  vector[T + 1] Biomass;           // Biomass series with one step ahead prediction
  real P_medfinal;                 // One step ahead median depletion
  real P_final;                    // One step ahead depletion
  real BMSY;                       // Biomass at MSY
  real FMSY;                       // Fishing mortality to achieve MSY
  real MSY;                        // Maximum sustainable yield

  // Calculate biomass at each time step from depletion and K, then simulate
  // one step ahead and include that final biomass
  for (t in 1:T) {
    Biomass[t] = K * P[t];
  }
  // One-step-ahead projection, including process error
  P_medfinal = schaefer(P[T],r, K, C[T]);
  P_final = lognormal_rng(log(P_medfinal), sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  BMSY = sch_bmsy(K);
  FMSY = sch_fmsy(r);
  MSY  = sch_msy(r, K);
}

