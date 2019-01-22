generated quantities {
  real<lower=0> r;
  real<lower=0> K;
  real<lower=0> sigma2;
  real<lower=0> tau2;
  real<lower=0> sigma;
  real<lower=0> tau;

  K = lognormal_rng(5.042905, 1 / sqrt(3.7603664));
  r = lognormal_rng(-1.38, 1 / sqrt(3.845));
  sigma2 = inv_gamma_rng(3.785518, 0.010223);
  tau2 = inv_gamma_rng(1.708603, 0.008613854);

  sigma = sqrt(sigma2);
  tau = sqrt(tau2);
}

