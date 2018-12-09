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

plottheme <- theme(axis.title = element_text(size = 20),
                   axis.text = element_text(size = 16),
                   strip.text = element_text(size = 20),
                   title = element_text(size = 24),
                   legend.title = element_text(size = 16),
                   legend.text = element_text(size = 14))

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

## Retrieve a posterior sample and put in a list that can be used as initial
## values for new fits. Need to get elements from one-dimensional parameters or
## rows from two-dimensional parameters.
get_post_sample <- function(iter, post_list) {
  parnames <- names(post_list)
  pardims <- lapply(post_list, dim)
  samp <- lapply(parnames, function(nm) {
    if (length(pardims[[nm]]) == 1) {
      post_list[[nm]][iter]
    } else {
      post_list[[nm]][iter, 1:pardims[[nm]][2]]
    }
  })
  names(samp) <- parnames
  samp
}

get_rand_init <- function(chain_id, post_list) {
  ## Use the total number of samples to choose from; r is a one-dimensional
  ## parameter in all the models so it's a convenient way to get this.
  it <- sample(1:length(post_list$r), 1L)
  get_post_sample(it, post_list)
}

## Retrieve `n` random samples from the previously fit posterior
make_init_list <- function(n, post_list) {
  it <- sample(1:length(post_list$r), n)
  lapply(it, function(i) get_post_sample(i, post_list))
}
