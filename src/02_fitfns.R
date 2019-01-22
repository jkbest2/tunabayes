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

theme_jkb <- function(base_size = 11,
                      title_family = "Rubik",
                      base_family = "Montserrat") {
  sizes <- c(8, 9, 10, 11, 12, 14, 16, 18, 20, 24, 28, 32)
  base_idx <- which(base_size == sizes)
  theme(
    text = element_text(family = base_family,
                        size = base_size),
    plot.margin = unit(c(0.5, 0.75, 0.5, 0.25), "cm"),
    ## Title
    title = element_text(family = title_family,
                         size = sizes[base_idx + 3]),
    ## Panel
    panel.background = element_blank(),
    panel.grid = element_blank(),
    ## Axes
    axis.line = element_line(),
    axis.title = element_text(family = title_family,
                              size = sizes[base_idx + 1]),
    axis.text = element_text(),
    ## Facet labels
    strip.text = element_text(size = sizes[base_idx + 1]),
    strip.background = element_blank(),
    ## Legend labels
    legend.title = element_text(family = title_family,
                                size = sizes[base_idx + 1]),
    legend.text = element_text(),
    legend.key = element_blank())
}

##-Initial value functions------------------------------------------------------
## Most parameters are drawn from their priors, P uses the data (assuming that
## I[1] is approximately P = 1) with some noise thrown in. Initial values for

## Initial values for truncated and centered models
init_cent <- function(chain_id) {
  sigma2 <- 1 / rgamma(1, 3.785518, rate = 0.010223)
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
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
       sigma2 = sigma2,
       tau2 = 1 / rgamma(1, 1.708603, rate = 0.008613854),
       P = rlnorm(tuna_data$T,
                  log(tuna_data$I / tuna_data$I[1]),
                  sqrt(sigma2)))
}

## Initial value function for exF models
init_expF <- function(chain_id) {
  sigma2 <- 1 / rgamma(1, 3.785518, rate = 0.010223)
  list(r = rlnorm(1, -1.38, 1 / sqrt(3.845)),
       K = rlnorm(1, 5.042905, 1 / sqrt(3.7603664)),
       q = runif(1, 0.15, 0.5),
       F = runif(n_years, 1, 3),
       sigma2 = sigma2,
       tau2 = 1 / rgamma(1, 1.708603, rate = 0.008613854),
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

