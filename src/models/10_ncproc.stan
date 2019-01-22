data {
  int<lower=0> T;                  // Number of years
  vector[T] C;                     // Observed catch
  vector[T] I;                     // CPUE index
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> q;                 // Catchability
  real<lower=0> sigma2;            // Process variance
  real<lower=0> tau2;              // Observation variance
  vector[T] u;                       // Process log-deviations
}

transformed parameters {
  real<lower=0> sigma;
  real<lower=0> tau;
  vector<lower=0>[T] Pmed;
  vector<lower=0>[T] P;

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  // Noncentered population deviations
  Pmed[1] = 1;
  P[1] = Pmed[1] * exp(u[1] * sigma);
  for (t in 2:T) {
    Pmed[t] = fmax(P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) -
                   C[t - 1] / K,
                   0.001);
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

