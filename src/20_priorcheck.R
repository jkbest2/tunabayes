### This section is just checking that the log Normal, gamma, and inverse gamma
### distributions in Stan are using the correct parameterization.
library(rstan)

priorfit <- stan("src/models/priorcheck.stan",
                 algorithm = "Fixed_param",   # Only generating random draws
                 chains = 6L, iter = 10000L)
monitor(priorfit, probs = c(0.1, 0.9), digits_summary = 2)

as_tibble(as.data.frame(priorfit)) %>%
  select(sigma_ig, sigma_g) %>%
  gather(par, val) %>%
  ggplot(aes(x = val, fill = par, color = par)) +
  geom_density(alpha = 0.4)

lognorm_fit <- stan("src/models/lognormalcheck.stan",
                    chains = 10L, iter = 1e4, warmup = 0L,
                    algorithm = "Fixed_param")
post <- as.data.frame(lognorm_fit, pars = c("X", "Y"))

post %>%
  gather("par", "val") %>%
  ggplot(aes(x = val, fill = par, color = par)) +
  geom_density(alpha = 0.4)

### This is a basic prior predictive check, used to set a prior on the
### coefficient of variation of the catch observations for the explicit F
### parameterizations.
## Import the data and source the CV-related functions
source("src/01_setup.R")
source("src/02_fitfns.R")
library(ggridges)

## Find the value of sigma that gives an 80% probability that catch is within
## about 10% of the observed value.
rt1 <- uniroot(function(sig) qlnorm(0.1, 0, sig) - 0.9, c(0.01, 1))
cv1 <- convert_sigma_cv(rt1$root)
rt2 <- uniroot(function(sig) qlnorm(0.9, 0, sig) - 1.1, c(0.01, 1))
cv2 <- convert_sigma_cv(rt2$root)

prior_rate <- cv_prior_rate(0.05, 0.9)

cv_draws <- rexp(1000, prior_rate)
sigma_draws <- convert_cv_sigma(cv_draws)
obs_draws <- rlnorm(1000, 0, sigma_draws)

## Plot the distribution of log Normal observations with cv marginalized out.
## Shows a much sharper central peak owing to the exponential prior on CV.
hist(obs_draws, breaks = 100, probability = TRUE)
curve(dlnorm(x, 0, 0.08), add = TRUE, col = "red")

## Calculate the quantiles of the observations on the scale of the actual
## catches to see if they're reasonable.
obs_qtiles <- quantile(obs_draws, c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
prior_pred_catch_qtiles <- outer(tuna_data$C, obs_qtiles)


ppc_df <- as_tibble(tuna_data$C %o% obs_draws) %>%
  mutate(year = 1:23) %>%
  gather(samp_id, pp_obs, -year)

## Density plots of the prior predictive observations (conditioned on the
## observed catch). Shows a fair amount of variation around the observed
## catches, generally on the order of a few megatonnes.
ggplot(ppc_df, aes(x = pp_obs, y = factor(year))) +
  geom_density_ridges(scale = 0.95) +
  geom_point(aes(y = year, x = obs),
             tibble(year = 1:23, obs = tuna_data$C)) +
  coord_flip() +
  scale_x_continuous(minor_breaks = 10:50) +
  ylab("Year") + xlab("Catch (Mt)") +
  theme_bw(30)

ggsave("notes/Manuscript/PriorPred_Catch.pdf")

## Prior predictive checks for Pella-Tomlinson shape parameter by looking at
## depletion at MSY
pmsy <- function(p) (p + 1) ^ (-1 / p)

## Simple rejection sampler to prevent m < 1
rlb <- function (rf, ..., lb = 1) {
  samp <- rf(...)
  while (samp < lb) samp <- rf(...)
  return(samp)
}

n_samp <- 10000L

sig <- 1.1
m_draws <- rlnorm(n_samp, log(0.19) - sig ^ 2 / 2, sig)
## gamma_shape <- 1e5
## m_draws <- rgamma(n_samp, gamma_shape, gamma_shape / 0.19)
pmsy_draws <- pmsy(m_draws)
hist(pmsy_draws, breaks = 50)
