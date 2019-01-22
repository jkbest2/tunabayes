data {
  int<lower=0> T;                  // Number of years
  vector[T] C;                     // Observed catch
  vector[T] I;                     // CPUE index
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> q;                 // Catchability
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  vector<lower=0>[T] P;            // Predicted depletion
}

transformed parameters {
  real<lower=0> sigma;             // Transform to standard deviation
  real<lower=0> tau;               // Transform to standard deviation
  vector[T] P_med;                 // Median depletion; no process error

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  // Initial depletion and catch
  P_med[1] = 1;
  for (t in 2:T) {
    P_med[t] = P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) - C[t - 1] / K;
    if (P_med[t] < 0) reject("Negative depletion, P_med[", t, "] = ", P_med[t],
                             " P[t - 1] = ", P[t - 1],
                             " r = ", r,
                             " K = ", K);
  }
}

model {
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);

  // Likelihood
  P ~ lognormal(log(P_med), sigma);
  I ~ lognormal(log(q * K * P), tau);
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
  P_medfinal = fmax(P[T] + r * P[T] * (1 - P[T]) - C[T] / K,
                    0.001);
  P_final = lognormal_rng(log(P_medfinal), sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  MSY = r * K / 4;
  FMSY = r / 2;
}

