/* This is not the model that it claims to be! Use 3_Schaefer_PL.stan
   instead! */
data {
  int<lower=0> T; // number of years
  real C[T];      // Catch
  real I[T];      // CPUE index
}

parameters {
  real<lower=0> r;
  real<lower=0> K;
  real<lower=0> q;
  real<lower=0> sigma2;
  real<lower=0> tau2;
  real<lower=0> u[T];
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
  P[1] = Pmed[1] * u[1] ^ sigma;
  for (t in 2:T) {
    Pmed[t] = fmax(P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) -
                   C[t - 1] / K,
                   0.001);
    P[t] = Pmed[t] * u[t] ^ sigma;
  }
}

model {
  // Priors
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);

  u ~ lognormal(0, 1);

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

