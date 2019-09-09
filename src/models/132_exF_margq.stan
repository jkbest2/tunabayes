functions {
  // Define a single step forward in time using Pella-Tomlinson dynamics. Uses
  // dynamics analogous to other non-explicit F models; both fishing *and*
  // production occur on the previous year's biomass/depletion.
  real pella_tomlinson(real P0, real r, real m, real F) {
    real Cp;            // Fraction mortality due to fishing
    real m1;            // Shape parameter minus 1

    Cp = P0 * exp(-F);

    m1 = m - 1;
    return P0 + P0 * r / m1 * (1 - P0^m1) - Cp;
  }

  // Calculate catch biomass based on current biomass, fishing mortality rate,
  // and carrying capacity.
  real pt_catch(real P0, real K, real F) {
    return K * P0 * exp(-F);
  }

  // Calculate BMSY
  real pt_bmsy(real K, real m) {
    real PMSY;
    PMSY = m ^ (-1 / (m - 1));
    return K * PMSY;
  }

  // Calculate FMSY
  real pt_fmsy(real r, real m) {
    return r / m;
  }

  // Use above to calculate MSY
  real pt_msy(real fmsy, real bmsy) {
    return fmsy * bmsy;
  }
}

data {
  int<lower=0> T;                  // Number of years
  vector[T] C;                     // Observed catch
  vector[T] I;                     // CPUE index
  real m;                          // Pella-Tomlinson shape parameter
  real catch_cv_prior_rate;        // Rate parameter on the exponential prior of
                                   // the catch error standard deviation
                                   // parameter
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  vector<lower=0>[T] F;            // Instantaneous fishing mortality
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  real<lower=0> catch_cv;          // CV of catch observations
  vector<lower=0>[T] P;            // Predicted depletion
}

transformed parameters {
  real<lower=0> sigma;             // Transform to standard deviation
  real<lower=0> tau;               // Transform to standard deviation
  real<lower=0> xi;                // Standard deviation of catch obs.
  vector<lower=0>[T] P_med;        // Median depletion; no process error
  vector<lower=0>[T] C_pred;       // Predicted catch, after error
  vector[T] Z;                     // Per year q MLE; follow the notation of
                                   // Walters and Ludwig 1994
  real log_q_hat;                  // Log catchability

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
  C_pred[1] = pt_catch(P[1], K, F[1]);
  // Initial depletion and catch. Note that catch occurs *after* production
  // here.
  for (t in 2:T) {
    P_med[t] = pella_tomlinson(P[t - 1], r, m, F[t - 1]);
    C_pred[t] = pt_catch(P[t], K, F[t]);
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
  // Exponential prior on catch observation coefficient of variation
  catch_cv ~ exponential(catch_cv_prior_rate);
  // Prior on F to give uniform prior on fraction of fishing mortality
  F ~ exponential(1);

  // State likelihood
  P ~ lognormal(log(P_med), sigma);
  // Observation likelihood
  /* NOTE: This will give a warning about using a transformed variable on the
   * left-hand side of a sampling statement. I'm pretty sure I don't need a
   * Jacobian adjustment here, but this is definitely the line causing the
   * warning.*/
  Z ~ normal(log_q_hat, tau);
  // Catch observation likelihood
  C ~ lognormal(log(C_pred), xi);
}

generated quantities {
  vector[T + 1] Biomass;           // Biomass series with one step ahead prediction
  real P_medfinal;                 // One step ahead median depletion
  real P_final;                    // One step ahead depletion
  real BMSY;                       // Biomass at MSY
  real FMSY;                       // Fishing mortality to achieve MSY
  real MSY;                        // Maximum sustainable yield
  real q_hat;                      // Catchability

  // Calculate biomass at each time step from depletion and K, then simulate
  // one step ahead and include that final biomass
  for (t in 1:T) {
    Biomass[t] = K * P[t];
  }

  // One-step-ahead projection, including process error
  P_medfinal = pella_tomlinson(P[T], r, m, F[T]);
  P_final = lognormal_rng(log(P_medfinal), sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  BMSY = pt_bmsy(K, m);
  FMSY = pt_fmsy(r, m);
  MSY  = pt_msy(BMSY, FMSY);

  // Catchability
  q_hat = exp(log_q_hat);
}
