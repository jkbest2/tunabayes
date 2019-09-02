source("src/03_priorfunctions.R")

## Most stock assessments set m so that Pmsy is at 0.4. We use the m value
## associated with this Pmsy as a starting point to explore potential priors on
## m, with the goal of choosing a prior with mean Pmsy of 0.4.

## Try different Beta priors on Pmsy. Easier to interpret. Double check that the
## density function on m integrates to 1 and has a reasonable expectation.
alpha <- 4
beta <- 0.6 / 0.4 * alpha

## For plotting densities
pmsy <- seq(0.01, 0.99, length.out = 513)
m <- sapply(pmsy, pmsy_to_m)
dpmsy <- dbeta(pmsy, alpha, beta)
dm <- dbeta_pmsy(m, alpha, beta)

## Generate random draws to compare with densities
rpmsy <- rbeta(100000, alpha, beta)
rm <- rbeta_pmsy(100000, alpha, beta)

par(mfrow = c(1, 2))

hist(rpmsy, breaks = 100, probability = TRUE,
     ylim = c(0, 1.05 * max(dpmsy)),
     yaxs = "i")
lines(pmsy, dpmsy)
abline(h = 0)

hist(rm[rm <= 10], breaks = 100, probability = TRUE,
     xlim = c(0, 10), ylim = c(0, 1.05 * max(dm)),
     yaxs = "i")
lines(m, dm)
abline(h = 0)

## Check the quantiles of Pmsy to see if they're reasonable. A distribution with
## more right-skew would be preferred, but the Beta is only a two-parameter
## distribution and there's not much to do about it. A joint hyper prior on
## alpha a beta might do it, but that's a lot to explore at this point.
quantile(rpmsy, c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))

## Make sure that the new distribution integrates to 1 and has finite expectation.
integrate(function(m) dbeta_pmsy(m, alpha, beta), lower = 0, upper = Inf)
integrate(function(m) m * dbeta_pmsy(m, alpha, beta), lower = 0, upper = Inf)


plot(m, dm, type = "l", xlim = c(0, 10))
## rb <- rbeta_pmsy()
hist(pmsy_to_m(rpmsy), breaks = 200, prob = TRUE, add = TRUE)
abline(v = 1, lty = 2)

## Plot the implied prior on Pmsy given a log Normal prior on the
## Pella-Tomlinson shape parameter. Looking for a relatively uninformative prior
## an expected Pmsy of 0.4. These priors give a right skew to the distribution
## of Pmsy.
sig = 1
ml <- find_meanlog(sig)
plot_dpmsy(dlnorm_m, ml, sig)
## Generate random draws to look at the quantiles of the implied prior on Pmsy
rpmsy <- rlnorm_m(10000, ml, sig)
quantile(rpmsy, c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))

## Alternatively, consider a gamma prior, again with expected Pmsy fixed
## to 0.4. These priors have a strong left skew, especially at higher
## variances. Putting extra probability mass on *lower* Pmsy's seems
## anticonservative at best.
var <- 1.5
gpars <- find_gammaparam(var)
plot_dpmsy(dgamma_ptshape, gpars$shape, gpars$rate)
## Generate random draws to look at the quantiles of the implied prior on Pmsy
rpmsy <- rgamma_ptshape(100000, gpars$shape, gpars$rate)
quantile(rpmsy, c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))

## And instead try a Beta prior on Pmsy for a better interpreability
alpha <- 4
beta <- 6
pmsy <- seq(1e-3, 9.999e-1, len = 1025)
m <- sapply(pmsy, pmsy_to_m)
dpmsy <- dbeta(pmsy, alpha, beta)
dm <- sapply(m, dbeta_pmsy, alpha = 4, beta = 6)
plot(m, dm, type = "l", xlim = c(0, 10))
