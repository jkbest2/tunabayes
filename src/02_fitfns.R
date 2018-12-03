## `bayesplot` package settings and wrapper for parcoord plots
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

## Initial value function for exF models
init_expF <- function(chain) {
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       q = runif(1, 0.01, 0.99),
       F = runif(n_years, 1, 3),
       sigma2 = 1 / rgamma(1, 3.785518, scale = 0.010223),
       tau2 = 1 / rgamma(1, 1.708603, scale = 0.008613854),
       P = 1 - 0.02 * 0:(n_years - 1L) + rnorm(n_years, 0, 0.1))
}
