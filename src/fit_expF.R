source("src/01_setup.R")

tuna_data = list(T = n_years,
                 I = indices,
                 C_obs = catch,
                 catch_sd = 0.02)

fit <- stan("src/models/8_Schaefer_exp_F.stan",
            data = tuna_data,
            iter = 10000, warmup = 5000,
            control = list(max_treedepth = 20L))

mod <- stan_model("src/models/8_Schaefer_exp_F.stan")

vb_fit <- vb(mod, data = tuna_data,
             init = init_expF,
             ## init = "0",
             algorithm = "meanfield",
             iter = 1e7)

init_expF <- function(chain) {
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       q = runif(1, 0.01, 0.99),
       F = runif(n_years, 1, 3),
       sigma2 = 1 / rgamma(1, 3.785518, scale = 0.010223),
       tau2 = 1 / rgamma(1, 1.708603, scale = 0.008613854),
       P = 1 - 0.02 * 0:(n_years - 1L) + rnorm(n_years, 0, 0.1))
}

fit <- stan("src/models/8_Schaefer_exp_F.stan",
            data = list(T = n_years,
                        I = indices,
                        C_obs = catch,
                        catch_sd = 0.02),
            init = 0,
            chains = 4, iter = 10000,
            control = list(max_treedepth = 15L))
save(fit, file = "expFfit.Rdata")

pars <- list(r = 0.2,
             K = 2000,
             q = 1,
             F = rep(2, n_years),
             sigma2 = 0.64,
             tau2 = 3.0,
             P = rep(2, n_years))
log_prob(fit, unconstrain_pars(fit, pars))
grad_log_prob(fit, unconstrain_pars(fit, pars))

opt <- optim(unconstrain_pars(fit, pars),
             fn = function(upar) log_prob(fit, upar),
             gr = function(upar) grad_log_prob(fit, upar),
             method = "BFGS")
