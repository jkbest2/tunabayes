functions {
  // Define a single step forward in time using Pella-Tomlinson dynamics.
  // Modified here to account for fishing mortality occuring *after* production
  // each year.
  real pella_tomlinson(real P0, real r, real K, real m, real F) {
    real m1;
    real surplus_prod;

    m1 = m - 1;
    surplus_prod = r / m1 * P0 * (1 - P0 ^ m1);

    // Return the biomass the *isn't* harvested
    return (P0 + surplus_prod) * (1 - exp(-F));
  }

  // Based on the current biomass, step forward via Pella-Tomlinson dynamics,
  // then calculate the catch biomass using the fishing mortality rate.
  real pt_catch(real P0, real r, real K, real m, real F) {
    real m1;
    real surplus_prod;

    m1 = m - 1;
    surplus_prod = r / m1 * P0 * (1 - P0 ^ m1);

    // Return the biomass that *is* harvested
    return (P0 + surplus_prod) * exp(-F);
  }

  // Calculate PMSY. Need this separate from BMSY below because using Beta
  // distribution to set prior on PMSY.
  real pt_pmsy(real m) {
    return m ^ (-1 / (m - 1));
  }

  // Calculate the derivative of PMSY wrt m. Used for Jacobian correction. Has
  // singularities at 0 and 1, but well-behaved near those values. Unlikely to
  // hit them exactly so not branching here.
  real ddm_pt_pmsy(real m) {
    return pt_pmsy(m) * (-1 / (m - m^2) - log(m) / (1 - m)^2);
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
  vector[2] pmsy_prior;            // alpha and beta of Beta prior on Pmsy
                                   // (implicit prior on PT shape parameter)
  real catch_cv_prior_rate;        // Rate parameter on the exponential prior of
                                   // the catch error standard deviation
                                   // parameter
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> q;                 // Catchability
  real<lower=0> m;                 // Pella-Tomlinson shape
  vector<lower=0>[T] F;            // Instantaneous fishing mortality
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  real<lower=0> catch_cv;          // CV of catch observations
  vector<lower=0>[T] P;            // Predicted depletion
}

transformed parameters {
  real<lower=0,upper=1> P_msy;     // Depletion at MSY
  real<lower=0> sigma;             // Process standard deviation param
  real<lower=0> tau;               // Observation standard deviation param
  real<lower=0> xi;                // Standard deviation param of catch obs.
  vector<lower=0>[T] P_med;        // Median depletion; no process error
  vector<lower=0>[T] C_pred;       // Predicted catch, after error

  // Prior is on P_msy, need to calculate it here from m
  P_msy = pt_pmsy(m);

  // Priors from Meyer & Millar 1999 are on the variance (originally precision)
  // parameter, but Stan takes a standard deviation parameter.
  sigma = sqrt(sigma2);
  tau = sqrt(tau2);
  // Coefficient of variation is a more concrete parameter to reason about, so
  // the prior is placed there. The log Normal distribution takes the standard
  // deviation parameter however, so it is necessary to convert CV.
  xi = sqrt(log(catch_cv^2 + 1));

  // Initial depletion and catch. Note that catch occurs *after* production
  // here.
  P_med[1] = 1;
  // Note that catch predictions from the `pt_catch` function are on the
  // depletion scale, so it is necessary to multiply by `K` to compare with
  // observed catches.
  C_pred[1] = K * pt_catch(P[1], r, K, m, F[1]);

  for (t in 2:T) {
    // Again, catch occurs after production. On the depletion scale we can
    // multiply by 1 - exp(-F) to get the fraction of biomass that does *not*
    // experience fishing mortality. Last year's depletion, production, and then
    // catch determine the median of the current year's depletion.
    P_med[t] = pella_tomlinson(P[t - 1], r, K, m, F[t]);
    // And calculate the biomass harvested
    C_pred[t] = K * pt_catch(P[t - 1], r, K, m, F[t]);
  }
}

model {
  // Priors specified in Meyer and Millar 1999
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);
  // Prior on P_msy with Jacobian correction
  P_msy ~ beta(pmsy_prior[1], pmsy_prior[2]);
  target += ddm_pt_pmsy(m);
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
  real BMSY;                       // Biomass at MSY
  real FMSY;                       // Fishing mortality to achieve MSY
  real MSY;                        // Maximum sustainable yield

  // Calculate biomass at each time step from depletion and K, then simulate
  // one step ahead and include that final biomass
  for (t in 1:T) {
    Biomass[t] = K * P[t];
  }
  // One-step-ahead projection, including process error
  P_medfinal = pella_tomlinson(P[T], r, K, m, F[T]);
  P_final = lognormal_rng(log(P_medfinal), sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  BMSY = K * P_msy;
  FMSY = pt_fmsy(r, m);
  MSY  = pt_msy(BMSY, FMSY);
}

