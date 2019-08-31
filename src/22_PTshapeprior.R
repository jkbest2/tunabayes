source("src/03_priorfunctions.R")

## Most stock assessments set m so that Pmsy is at 0.4. We use the m
## value associated with this Pmsy as a starting point to explore
## potential priors on m, with the goal of choosing a prior with mean
## Pmsy of 0.4.

## Plot the implied prior on Pmsy given a log Normal prior on the
## Pella-Tomlinson shape parameter. Looking for a relatively uninformative prior
## an expected Pmsy of 0.4. These priors give a right skew to the distribution
## of Pmsy.
sig = 1
ml <- find_meanlog(sig)
plot_dpmsy(dlnorm_ptshape, ml, sig)
## Generate random draws to look at the quantiles of the implied prior on Pmsy
rpmsy <- rlnorm_ptshape(10000, ml, sig)
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

