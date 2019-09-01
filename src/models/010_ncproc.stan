functions {
  // Define a single step forward in time using Pella-Tomlinson dynamics. This
  // uses the parameterization presented in Winker et al. 2018.
  real pella_tomlinson(real P0, real r, real K, real m, real C) {
    real m1;
    real surplus_prod;
    real P1;

    m1 = m - 1;
    surplus_prod = r / m1 * P0 * (1 - P0 ^ m1);
    P1 = P0 + surplus_prod - C / K;

    return P1;
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
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=0> q;                 // Catchability
  real<lower=0> m;                 // Pella-Tomlinson shape
  real<lower=0> sigma2;            // Process variance
  real<lower=0> tau2;              // Observation variance
  vector[T] u;                     // Process log-deviations
}

transformed parameters {
  real<lower=0,upper=1> P_msy;     // Depletion at MSY
  real<lower=0> sigma;             // Process standard deviation parameter
  real<lower=0> tau;               // Observation standard deviation parameter
  vector<lower=0>[T] P_med;         // Median depletion; no process error
  vector<lower=0>[T] P;            // Predicted depletion with process error

  // Prior is on P_msy, need to calculate it here from m
  P_msy = pt_pmsy(m);

  // Priors from Meyer & Millar 1999 are on the variance (originally precision)
  // parameter, but Stan takes a standard deviation parameter.
  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  // Here the `u` parameters are applied as process error
  P_med[1] = 1;
  P[1] = P_med[1] * exp(u[1] * sigma);
  for (t in 2:T) {
    // Note `fmax` here to keep depletion positive!
    P_med[t] = fmax(pella_tomlinson(P[t - 1], r, K, m, C[t - 1]),
                    0.001);
    P[t] = P_med[t] * exp(u[t] * sigma);
  }
}

model {
  // Priors
  r ~ lognormal(-1.38, 1 / sqrt(3.845));
  K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
  target += -log(q);
  sigma2 ~ inv_gamma(3.785518, 0.010223);
  tau2 ~ inv_gamma(1.708603, 0.008613854);
  // Prior on P_msy with Jacobian correction
  P_msy ~ beta(pmsy_prior[1], pmsy_prior[2]);
  target += ddm_pt_pmsy(m);

  // Process error likelihood
  u ~ normal(0, 1);
  // Observation likelihood
  I ~ lognormal(log(q * K * P), tau);
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
  P_medfinal = fmax(pella_tomlinson(P[T], r, K, m, C[T]),
                    0.001);
  // Use the same noncentered form to project ahead, but would get the same
  // result using `lognormal_rng(log(P_medfinal), sigma);`
  P_final = P_medfinal * exp(normal_rng(0, 1) * sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  BMSY = K * P_msy;
  FMSY = pt_fmsy(r, m);
  MSY  = pt_msy(BMSY, FMSY);
}

