##-Initial value functions------------------------------------------------------
## Most parameters are drawn from their priors, P uses the data (assuming that
## I[1] is approximately P = 1) with some noise thrown in. Initial values for

## Initial values for centered models
init_cent <- function(chain_id) {
  sigma2 <- 1 / rgamma(1, 3.785518, rate = 0.010223)
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       m = runif(1, 1, 5),
       q = runif(1, 0.15, 0.5),
       sigma2 = sigma2,
       tau2 = 1 / rgamma(1, 1.708603, rate = 0.008613854),
       P = rlnorm(tuna_data$T,
                  log(tuna_data$I / tuna_data$I[1]),
                  sqrt(sigma2)))
}

## Initial values for noncentered models
init_ncproc <- function(chain_id) {
  sigma2 <- 1 / rgamma(1, 3.785518, rate = 0.010223)
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       m = runif(1, 1, 5),
       q = runif(1, 0.15, 0.5),
       sigma2 = sigma2,
       tau2 = 1 / rgamma(1, 1.708603, rate = 0.008613854),
       u = rnorm(tuna_data$T, 0, sqrt(sigma2)))
}

## Initial values for model with q marginalized out
init_margq <- function(chain_id) {
  sigma2 <- 1 / rgamma(1, 3.785518, rate = 0.010223)
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       m = runif(1, 1, 5),
       sigma2 = sigma2,
       tau2 = 1 / rgamma(1, 1.708603, rate = 0.008613854),
       P = rlnorm(tuna_data$T,
                  log(tuna_data$I / tuna_data$I[1]),
                  sqrt(sigma2)))
}

## Initial value function for exF models
init_exF <- function(chain_id) {
  sigma2 <- 1 / rgamma(1, 3.785518, rate = 0.010223)
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       m = runif(1, 1, 5),
       q = runif(1, 0.15, 0.5),
       F = rexp(n_years, 1),
       sigma2 = sigma2,
       tau2 = 1 / rgamma(1, 1.708603, rate = 0.008613854),
       catch_cv = rexp(1, 40),
       P = rlnorm(tuna_data$T,
                  log(tuna_data$I / tuna_data$I[1]),
                  sqrt(sigma2)))
}

## Initial values for constrained P models
init_constrP <- function(chain_id) {
  sigma2 <- 1 / rgamma(1, 3.785518, rate = 0.010223)
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       q = runif(1, 0.1, 0.5),
       sigma2 = sigma2,
       tau2 = 1 / rgamma(1, 1.708603, rate = 0.008613854),
       P = rlnorm(tuna_data$T,
                  log(tuna_data$I / tuna_data$I[1]),
                  sqrt(sigma2)))
}

## Convert the coefficient of variation of a log Normal distribution to a
## standard deviation parameter, as required by the R rlnorm function.
convert_cv_sigma <- function(cv) sqrt(log(cv^2 + 1))
## Convert the standard deviation function of the log normal to a coefficient of
## variation.
convert_sigma_cv <- function(sigma) sqrt(exp(sigma^2) - 1)
## Calculate the exponential rate that gives a probability of p that the cv is
## less than cv_lim.
cv_prior_rate <- function(cv_lim, p) -log(1 - p) / cv_lim

## Find the best value of the Pella-Tomlinson shape parameter for a given
## depletion-at-MSY value
Pmsy_to_m <- function(Pmsy) {
  uniroot(function(m) m ^ (-1 / (m - 1)) - Pmsy, c(0, 10))$root
}
