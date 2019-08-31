## Need the gsl library for Lambert's W function
library(gsl)

## Prior predictive checks for Pella-Tomlinson shape parameter by looking at
## depletion at MSY
m_to_pmsy <- function(m) m ^ (-1 / (m - 1))

## This calculates the value of m for a given Pmsy
pmsy_to_m <- function(pmsy) {
  ## The correct branch of the Lambert's W function needs to be chosen
  ## based on whether m < 1 or m > 1
  if (pmsy < 1 / exp(1)) {
    lambert <- lambert_W0
  } else {
    lambert <- lambert_Wm1
  }
  lambert(pmsy * log(pmsy)) / log(pmsy)
}

## This is the derivative of the above function, which can be used a
## Jacobian correction when calculating the PDF of Pmsy
ddm_pmsy_to_m <- function(pmsy, log = FALSE) {
  ## The correct branch of the Lambert's W function needs to be chosen
  ## based on whether m < 1 or m > 1
  if (pmsy < 1 / exp(1)) {
    lambert <- lambert_W0
  } else {
    lambert <- lambert_Wm1
  }
  lp <- log(pmsy)
  plp <- pmsy * lp
  Wplp <- lambert(plp)
  num <- (lp - Wplp) * Wplp
  den <- pmsy * lp^2 * (Wplp + 1)
  ld <- num / den
  ld
}

## Calculate the expected Pmsy by numerical integration. The `df` argument is a
## density function that takes arguments `par1` and `par2` (e.g. meanlog and
## sdlog or shape and rate).
exp_pmsy <- function(df, par1, par2) {
  integrate(function(pmsy)
    pmsy * df(pmsy, par1, par2),
    lower = 0, upper = 1)$value
}

## Plot the PDF of Pmsy where the PT shape parameter has density function `df`
## taking two parameters, `par1` and `par2`.
plot_dpmsy <- function(df, par1, par2) {
  pmsy <- seq(0.01, 0.99, 0.01)
  dpmsy <- df(pmsy, par1, par2)
  plot(pmsy, dpmsy, type = "l")
}

##-log Normal priors------------------------------------------------------------
## This is the prior PDF of Pmsy with a logNormal prior on m
dlnorm_ptshape <- function(pmsy, meanlog, sdlog) {
    ## Need to use *apply here or the if statements in the pmsy_to_m
    ## function will only look at the first element of the `pmsy` vector
    ## (and throw a warning).
    m <- vapply(pmsy, pmsy_to_m, FUN.VALUE = 1.0)
    dlm <- dlnorm(m, meanlog, sdlog)
    jac <- vapply(pmsy, ddm_pmsy_to_m, FUN.VALUE = 1.0)
    dlm * abs(jac)
}

## Generate `n` Pmsy values for given log Normal prior hyperparameters
rlnorm_ptshape <- function(n, meanlog, sdlog) {
    rln <- rlnorm(n, meanlog, sdlog)
    m_to_pmsy(rln)
}

## Find the meanlog of a logNormal distribution with expected Pmsy 0.4 and given
## sdlog. WARNING limited range of working values for `sdlog`; `uniroot` will
## error if it's not in the range 0.21 < sig < 1.85. Could probably be fixed,
## but this is a good range for what I'm doing here.
find_meanlog <- function(sdlog = 1, target_pmsy = 0.4, interval = c(0, 10)) {
  ml <- uniroot(function(ml) {
    exp_pmsy(dlnorm_ptshape, ml, sdlog) - target_pmsy
  }, lower = interval[1], upper = interval[2])$root
  ml
}

##-Gamma prior------------------------------------------------------------------
## This is the PDF of Pmsy with a Gamma prior on m
dgamma_ptshape <- function(pmsy, shape, rate) {
    ## Need to use *apply here or the if statements in the pmsy_to_m
    ## function will only look at the first element of the `pmsy` vector
    ## (and throw a warning).
    m <- vapply(pmsy, pmsy_to_m, FUN.VALUE = 1.0)
    dlm <- dgamma(m, shape, rate)
    jac <- vapply(pmsy, ddm_pmsy_to_m, FUN.VALUE = 1.0)
    dlm * abs(jac)
}

## Generate `n` Pmsy values for given log Normal prior hyperparameters
rgamma_ptshape <- function(n, shape, rate) {
    rln <- rgamma(n, shape, rate)
    m_to_pmsy(rln)
}

## Get the Gamma rate and shape parameters from a given mean and variance
gamma_rateshape <- function(gmean, gvar) {
  list(shape = gmean^2 / gvar, rate = gmean / gvar)
}

## Find the shape and rate of a Gamma prior where the expected Pmsy is
## `target_pmsy` and that has variance `gvar`
find_gammaparam <- function(gvar, target_pmsy = 0.4, interval = c(0, 10)) {
  gmean <- uniroot(function(gmean) {
    gpars <- gamma_rateshape(gmean, gvar)
    exp_pmsy(dgamma_ptshape, gpars$shape, gpars$rate) - target_pmsy
  }, lower = interval[1], upper = interval[2])$root
  gamma_rateshape(gmean, gvar)
}

