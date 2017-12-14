data {
    int<lower=0> N; // number of years
    real C[N];      // Catch
    real I[N];      // CPUE Index
}

parameters {
    real<lower=0> K;
    real<lower=0> r;
    real<lower=0> u[N];
    real<lower=0> sigma2;
    real<lower=0> tau2;
    real<lower=0> q;
}

transformed parameters {
    real<lower=0> sigma;
    real<lower=0> tau;
    real<lower=0> Pmed[N];
    real<lower=0> P[N];

    sigma = sqrt(sigma2);
    tau = sqrt(tau2);

    // Noncentered population deviations
    Pmed[1] = 1;
    P[1] = Pmed[1] * exp(u[1] * sigma);
    for (t in 2:N) {
        Pmed[t] = fmax(P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) -
                           C[t - 1] / K,
                       0.001);
	    P[t] = Pmed[t] * u[t] ^ sigma;
    }
}

model {
    // Priors
    K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
    r ~ lognormal(-1.38, 1 / sqrt(3.845));
    sigma2 ~ inv_gamma(3.785518, 0.010223);
    tau2 ~ inv_gamma(1.708603, 0.008613854);
    target += -log(q);

    u ~ lognormal(0, 1);

    for (t in 1:N) {
      I[t] ~ lognormal(q * K * P[t], tau);
    }
}

generated quantities {
    vector[N] Imed;
    vector[N] Ipred;
    vector[N] Biomass;
    real MSY;
    real EMSY;

    //posterior predictions (hint, the parameterization of dlnorm is not the same as in R)
    for (t in 1:N) {
        Imed[t] = q * K * P[t];
        Ipred[t] = lognormal_rng(Imed[t], tau);
        Biomass[t] = K * P[t];
    }

    // Other ouputs
    MSY = r * K / 4;
    EMSY = r / (2 * q);
}

