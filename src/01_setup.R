library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)
library(bayesplot)

color_scheme_set('darkgray')
div_style <- parcoord_style_np(div_color = 'green',
                               div_size = 0.25,
                               div_alpha = 0.4)
stanfit_parcoord <- function(stanfit, transform = scale,
                             size = 0.25, alpha = 0.1,
                             np_style = div_style, ...) {
    draws <- as.array(stanfit)
    np <- nuts_params(stanfit)
    mcmc_parcoord(draws, transform = transform,
                  size = size, alpha = alpha,
                  np = np, np_style = np_style, ...)
}

# Data: Catches, CPUE Index, and number of years
C <- c(15.9, 25.7, 28.5, 23.7, 25.0, 33.3, 28.2, 19.7, 17.5, 19.3,
       21.6, 23.1, 22.5, 22.5, 23.6, 29.1, 14.4, 13.2, 28.4, 34.6,
       37.5, 25.9, 25.3)
I <- c(61.89, 78.98, 55.59, 44.61, 56.89, 38.27, 33.84, 36.13, 41.95,
       36.63, 36.33, 38.82, 34.32, 37.64, 34.01, 32.16, 26.88, 36.61,
       30.07, 30.75, 23.36, 22.36, 21.91)
N <- 23

tuna_data <- list(C=C,I=I,N=N)

#initial values
inits1 <- list(r = 0.8, K = 200, iq = 0.5, isigma2 = 100, itau2 = 100,
               P = c(0.99, 0.98, 0.96, 0.94, 0.92, 0.90, 0.88, 0.86,
                     0.84, 0.82, 0.80, 0.78, 0.76, 0.74, 0.72, 0.70,
                     0.68, 0.66, 0.64, 0.62, 0.60, 0.58, 0.56))
inits2 <- list(r = 0.5, K = 300, iq = 0.8, isigma2 = 200, itau2 = 200,
               P = c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99,
                     0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99,
                     0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99))
inits3 <- list(r = 0.15, K = 90, iq = 0.7, isigma2 = 200, itau2 = 200,
               P = rep(0.75, N))
inits4 <- list(r = 0.25, K = 200, iq = 0.5, isigma2 = 200, itau2 = 200,
               P = rep(0.9, N))
inits <- list(inits1, inits1, inits1, inits1)#, inits3, inits4)

inits11 <- list(r = 0.8, K = 200, q = 2.0, sigma2 = 1e-2, tau2 = 1e-2,
                P = c(0.99, 0.98, 0.96, 0.94, 0.92, 0.90, 0.88, 0.86,
                      0.84, 0.82, 0.80, 0.78, 0.76, 0.74, 0.72, 0.70,
                      0.68, 0.66, 0.64, 0.62, 0.60, 0.58, 0.56))
inits12 <- list(r = 0.5, K = 300, q = 1.25, sigma2 = 5e-3, tau2 = 5e-3,
                P = c(0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99,
                      0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99,
                      0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99))
inits_r <- list(inits11, inits12)
