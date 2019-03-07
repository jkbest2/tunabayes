data {
  int<lower=0> T;                      // Number of years
  real C[T];                           // Observed catch
  real I[T];                           // CPUE index
}

parameters {
  real<lower=0.01, upper=1.2> r;       // Population growth
  real<lower=10, upper=1000> K;        // Carrying capacity
  real<lower=0.5,upper=100> iq;        // Inverse catchability
  real<lower=0> isigma2;               // Process precision
  real<lower=0> itau2;                 // Observation precision
  vector<lower=0.001, upper=2.0>[T] P; // Predicted depletion
}

transformed parameters {
  real<lower=0> sigma;                 // Transform to standard deviation
  real<lower=0> tau;                   // Transform to standard deviation
  real<lower=0> q;                     // Inverse catchability to catchability
  vector[T] P_med;                     // Median depletion; no process error

  sigma = 1.0 / sqrt(isigma2);
  tau = 1.0 / sqrt(itau2);
  q = 1.0 / iq;

  P_med[1] = 1;
  // Time steps of the model
  for (t in 2:T) {
    // Note `fmax` function call here to keep depletion positive
    /* P_med[t] = fmax(P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) - */
    /*                C[t - 1] / K, */
    /*                0.001); */
    P_med[t] = P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) - C[t - 1] / K;
  }
}

model {
  // Priors from Meyer and Millar 1999 (BUGS program appendix). Note the
  // explicit truncations (corresponding to limits declared above).
  r ~ lognormal(-1.38, 1 / sqrt(3.845)); //T[0.01, 1.2];
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));// T[10, 1000];
  iq ~ gamma(0.001, 0.001);
  isigma2 ~ gamma(3.785518, 0.010223);
  itau2 ~ gamma(1.708603, 0.008613854);

  // Likelihoods
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
  P_medfinal =  fmax(P[T] + r * P[T] * (1 - P[T]) - C[T] / K,
                     0.001);
  P_final = lognormal_rng(log(P_medfinal), sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  MSY = r * K / 4;
  FMSY = r / 2;
}
