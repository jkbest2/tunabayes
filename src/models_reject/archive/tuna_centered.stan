data {
    int<lower=0> N; // number of years
    real C[N]; // estimated treatment effects
    real I[N]; // estimated treatment effects
}

parameters {
    real<lower=0> K;
    real r;
    real<lower=0> sigma2;
    real<lower=0> tau2;
    real<lower=0> q;
    real<lower=0> P[N];
}

transformed parameters {
    real<lower=0> sigma;
    real<lower=0> tau;

    sigma = sqrt(sigma2);
    tau = sqrt(tau2);
}

model {
    real Pmed[N];
    real Imed[N];

    r ~ lognormal(-1.38, 0.51);
    sigma2 ~ inv_gamma(3.785, 0.0102);
    tau2 ~ inv_gamma(1.709, 0.00861);
    target += 1 / q;
    K ~ lognormal(5.0429, 0.5162);

    // Set initial state
    Pmed[1] = 1;
    P[1] ~ lognormal(Pmed[1], sigma);

    // time steps of the model
    for (t in 2:N) {
        Pmed[t] = fmax(P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) - C[t - 1] / K, 0.001);
        // Pmed[t] = P[t - 1] + r * P[t - 1] * (1 - P[t - 1]) - C[t - 1] / K;
        P[t] ~ lognormal(Pmed[t], sigma);
     }

    // Likelihood
    for (t in 1:N) {
        Imed[t] = q * K * P[t];
        I[t] ~ lognormal(Imed[t], tau);
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
        Biomass[t] = K*P[t];
    }

    // Other ouputs
    MSY = r*K/4;
    EMSY = r/(2*q);
}
