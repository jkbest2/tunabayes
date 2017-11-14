data {
  int<lower=0> N; // number of years
  real C[N]; // estimated treatment effects
  real I[N]; // estimated treatment effects
}
parameters {
  real<lower=0> r;
  real<lower=0> K;
  real<lower=0> iq;
  real<lower=0> isigma2;
  real<lower=0> itau2;
  vector[N] P;

}

transformed parameters {
 real sigma; real tau; real q;
 sigma = 1.0/sqrt(isigma2);
 tau = 1.0/sqrt(itau2);
 q = 1/iq;
}

model {
  vector[N] Pmed;
  vector[N] Imed;

  r ~ lognormal(-1.38,3.845);
  isigma2 ~ gamma(3.785, 0.0102);
  itau2 ~ gamma(1.709, 0.0861);
  iq ~ gamma(0.001,0.001);
  K ~ lognormal(5.0429, 3.7603);

  // Set initial state
  Pmed[1] = 0;
  P[1] ~ lognormal(Pmed[1],sigma);

  // time steps of the model
  for (t in 2:N)
   {
    Pmed[t] = log(fmax(P[t - 1] + (r * P[t - 1]) * (1 - P[t - 1]) - C[t - 1] / K, 0.001) );
    P[t] ~ lognormal(Pmed[t], sigma);
   }

  // Likelihood
  for (t in 1:N)
   {
    Imed[t] = log((q * K) * P[t]);
    I[t] ~ lognormal(Imed[t],tau);
   }

}

generated quantities {
  vector[N] Imed;
  vector[N] Inew;
  vector[N] Biomass;
  real MSY;
  real EMSY;

  //posterior predictions (hint, the parameterization of dlnorm is not the same as in R)
  for (t in 1:N)
   {
    Imed[t] = log((q * K) * P[t]);
    Inew[t] = lognormal_rng(Imed[t], tau);
    Biomass[t] = K*P[t];
   }

   // Other ouputs
   MSY = r*K/4;
   EMSY = r/(2*q);
  }




