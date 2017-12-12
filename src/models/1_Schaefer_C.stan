data {
    int<lower=0> N; // number of years
    real C[N]; // estimated treatment effects
    real I[N]; // estimated treatment effects
}
parameters {
    real<lower=0> r;
    real<lower=0> K;
    real<lower=0> q;
    real<lower=0> sigma2;
    real<lower=0> tau2;
    real<lower=0> P[N];
}

transformed parameters {
    real<lower=0> sigma;
    real<lower=0> tau;

    sigma = sqrt(sigma2);
    tau = sqrt(tau2);
}

model {
    vector[N] Pmed;
    vector[N] Imed;

    K ~ lognormal(5.042905, 1 / sqrt(3.7603664));
    r ~ lognormal(-1.38, 1 / sqrt(3.845));
    sigma2 ~ inv_gamma(3.785518, 0.010223);
    tau2 ~ inv_gamma(1.708603, 0.008613854);
    target += -log(q);

    // Set initial state
    Pmed[1] = 0;
    P[1] ~ lognormal(Pmed[1], sigma);

    // time steps of the model
    for (t in 2:N) {
        Pmed[t] = log(fmax(P[t - 1] + (r * P[t - 1]) * (1 - P[t - 1]) -
                               C[t - 1] / K,
                           0.00001));
        P[t] ~ lognormal(Pmed[t], sigma);
    }

    // Likelihood
    for (t in 1:N) {
        Imed[t] = log(q * K * P[t]);
        I[t] ~ lognormal(Imed[t], tau);
    }
}

generated quantities {
    vector[N] Imed;
    vector[N] Inew;
    vector[N] Biomass;
    real MSY;
    real EMSY;

    //posterior predictions (hint, the parameterization of dlnorm is not the same as in R)
    for (t in 1:N) {
        Imed[t] = log(q * K * P[t]);
        Inew[t] = lognormal_rng(Imed[t], tau);
        Biomass[t] = K * P[t];
    }

    // Other ouputs
    MSY = r * K / 4;
    EMSY = r / (2 * q);
}

