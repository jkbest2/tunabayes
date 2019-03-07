generated quantities {
  real<lower=0> r;
  real<lower=0> K;
  real<lower=0> sigma2_ig;
  real<lower=0> tau2_ig;
  real<lower=0> sigma_ig;
  real<lower=0> tau_ig;
  real<lower=0> isigma2_g;
  real<lower=0> itau2_g;
  real<lower=0> sigma2_g;
  real<lower=0> tau2_g;
  real<lower=0> sigma_g;
  real<lower=0> tau_g;


  K = lognormal_rng(5.042905, 1 / sqrt(3.7603664));
  r = lognormal_rng(-1.38, 1 / sqrt(3.845));

  // Variance priors using inverse gamma
  sigma2_ig = inv_gamma_rng(3.785518, 0.010223);
  tau2_ig = inv_gamma_rng(1.708603, 0.008613854);
  sigma_ig = sqrt(sigma2_ig);
  tau_ig = sqrt(tau2_ig);

  // Precision priors using Gamma (should be same as above)
  isigma2_g = gamma_rng(3.785518, 0.010223);
  itau2_g = gamma_rng(1.708603, 0.008613854);
  sigma2_g = 1 / isigma2_g;
  tau2_g = 1 / itau2_g;
  sigma_g = sqrt(sigma2_g);
  tau_g = sqrt(tau2_g);

}

