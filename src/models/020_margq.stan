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
}

parameters {
  real<lower=0> r;                 // Population growth
  real<lower=0> K;                 // Carrying capacity
  real<lower=1,upper=10> m;        // FIXME Pella-Tomlinson shape
  real<lower=0> sigma2;            // Process variability
  real<lower=0> tau2;              // Observation variability
  vector<lower=0>[T] P;            // Predicted depletion
}

transformed parameters {
  real<lower=0> sigma;             // Process standard deviation
  real<lower=0> tau;               // Observation standard deviation
  vector[T] P_med;                 // Median depletion; no process error
  vector[T] Z;                     // Per year catchability; follow the notation
                                   // of Walters and Ludwig 1994
  real log_q_hat;                  // Estimate of q

  // Priors from Meyer & Millar 1999 are on the variance (originally precision)
  // parameter, but Stan takes a standard deviation parameter.
  sigma = sqrt(sigma2);
  tau = sqrt(tau2);

  // Initial depletion and catch
  P_med[1] = 1;
  for (t in 2:T) {
    // Note `fmax` here to keep depletion positive!
    P_med[t] = fmax(pella_tomlinson(P[t - 1], r, K, m, C[t - 1]),
                    0.001);
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
  // FIXME Prior on Pella-Tomlinson shape parameter
  m ~ uniform(1, 10);

  // Process likelihood
  P ~ lognormal(log(P_med), sigma);
  // Observation likelihood
  /* NOTE: This will give a warning about using a transformed variable on the
   * left-hand side of a sapling statement. I'm pretty sure I don't need a
   * Jacobian adjustment here, but this is definitely the line causing the
   * warning.*/
  Z ~ normal(log_q_hat, tau);
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
  P_final = lognormal_rng(log(P_medfinal), sigma);
  Biomass[T + 1] = K * P_final;

  // Management values
  BMSY = pt_bmsy(K, m);
  FMSY = pt_fmsy(r, m);
  MSY  = pt_msy(BMSY, FMSY);
}

