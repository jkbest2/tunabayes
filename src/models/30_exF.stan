data {
  int<lower=0> T;                  // Number of years
  vector[T] C;                     // Observed catch
  vector[T] I;                     // CPUE index
  real catch_cv_prior_rate;        // Rate parameter on the exponential prior of
                                   // the catch error standard deviation
                                   // parameter
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> q;                 // Catchability
  vector<lower=0>[T] F;            // Instantaneous fishing mortality
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  real<lower=0> catch_cv;          // CV of catch observations
  vector<lower=0>[T] P;            // Predicted depletion
}

transformed parameters {
  real<lower=0> sigma;             // Process standard deviation
  real<lower=0> tau;               // Observation standard deviation
  real<lower=0> xi;                // Standard deviation of catch obs.
  vector[T] P_med;                 // Median depletion; no process error
  vector[T] C_pred;                // Predicted catch, after error

  // Priors in Meyer and Millar 1999 are in terms of variance, but Stan uses
  // standard deviation as second parameter of the log Normal distribution
  sigma = sqrt(sigma2);
  tau = sqrt(tau2);
  // Coefficient of variation is a more concrete parameter to reason about, so
  // the prior is placed there. The log Normal distribution takes the standard
  // deviation parameter however, so it is necessary to convert CV.
  xi = sqrt(log(catch_cv^2 + 1));

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
    // And current year catch depends on current depletion
    C_pred[t] = K * (P[t] + r * P[t] * (1 - P[t])) * exp(-F[t]);
  }
}

model {
  // Priors specified in Meyer and Millar 1999
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);
  // Exponential prior on catch observation coefficient of variation
  catch_cv ~ exponential(catch_cv_prior_rate);

  // State likelihoods
  P ~ lognormal(log(P_med), sigma);
  // Observation likelihood
  I ~ lognormal(log(q * K * P), tau);
  // Catch observation likelihood
  C ~ lognormal(log(C_pred), xi);
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

