## Load packages and data
source("src/01_setup.R")
## Load plot specifics and initial value function
source("src/02_fitfns.R")

load("results/2018-12-02 15:51:23_fits.Rdata")
post10 <- extract(fit_df$fit[[10]])

set.seed(9448949)

chain_spec <- list(n_iter = 1e4,
                   n_warm = 5e3,
                   n_chain = 4)

## Explicit F flattens posterior so much that it needs a larger treedepth
exF_control = list(max_treedepth = 15L)

## Standard deviations to try for catch, from 2e-3 to 200
catch_sd <- 10^(seq(-1, 1, length.out = 5L))

catch_sd_data <- function(catch_sd) {
  td2 <- tuna_data
  td2$catch_sd <- catch_sd
  td2
}

get_post_sample <- function(iter, post_list = post10) {
  parnames <- c("r", "K", "q", "F", "sigma2", "tau2", "P")
  list(r = post_list$r[iter],
       K = post_list$K[iter],
       q = post_list$q[iter],
       F = post_list$F[iter, ],
       sigma2 = post_list$sigma2[iter],
       tau2 = post_list$tau2[iter],
       P = post_list$P[iter, ])
}

get_rand_post <- function(chain) {
  it <- sample(1:20000, 1L)
  get_post_sample(it, post10)
}

## Model location
modfile <- "src/models/30_exF.stan"

csd_df <- data_frame(catch_sd = catch_sd) %>%
  mutate(data = map(catch_sd, catch_sd_data),
         fit = map(data, ~ stan(file = modfile,
                                data = .x,
                                init = get_rand_post,
                                chains = chain_spec$n_chain,
                                iter = chain_spec$n_iter,
                                warmup = chain_spec$n_warm,
                                control = list(max_treedepth = 15L))))
walk(csd_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))
save(csd_df, chain_spec,
     file = paste0("results/", Sys.time(), "_csd.Rdata"))

