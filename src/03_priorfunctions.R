## Need the gsl library for Lambert's W function
library(gsl)
## Library with function for skew-normal distribution
library(sn)

## Prior predictive checks for Pella-Tomlinson shape parameter by looking at
## depletion at MSY. Includes branches to eliminate singularity at m = 1 and m =
## 0 (both by taking the the limit).
m_to_pmsy <- function(m) {
  if (!(m == 1 | m <= 0)) {
    return(m ^ (-1 / (m - 1)))
  } else if (m == 0){
    return(0)
  } else if (m == 1) {
    return(1 / exp(1))
  } else {
    stop("m must be non-negative")
  }
}

## The first derivative of the previous function; useful for Jacobian
## corrections.
ddm_m_to_pmsy <- function(m) {
  if (!(m == 1 | m <= 0)) {
    t1 <- m^(-m / (m - 1))
    num <- -m + m * log(m) + 1
    den <- (m - 1)^2
    return(t1 * num / den)
  } else if (m == 0) {
    return(1)
  } else if (m == 1) {
    return(1 / (2 * exp(1)))
  } else {
    stop("m must be non-negative")
  }
}

log_ddm_m_to_pmsy <- function(m) {
  if (!(m == 1 | m <= 0)) {
    logt1 <- (-m / (m - 1)) * log(m)
    lognum <- log(-m + m * log(m) + 1)
    ## Need to square before taking log to keep everyting positive
    logden <- log((m - 1)^2)
    return(logt1 + lognum - logden)
  } else if (m == 0) {
    return(0)
  } else if (m == 1) {
    return(-2)
  } else {
    stop("m must be non-negative")
  }
}

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
ddpmsy_pmsy_to_m <- function(pmsy, log = FALSE) {
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
exp_pmsy <- function(df, ...) {
  integrate(function(pmsy)
    pmsy * df(pmsy, ...),
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
dlnorm_m <- function(pmsy, meanlog, sdlog) {
  ## Need to use *apply here or the if statements in the pmsy_to_m
  ## function will only look at the first element of the `pmsy` vector
  ## (and throw a warning).
  m <- vapply(pmsy, pmsy_to_m, FUN.VALUE = 1.0)
  dlm <- dlnorm(m, meanlog, sdlog)
  jac <- vapply(pmsy, ddpmsy_pmsy_to_m, FUN.VALUE = 1.0)
  ## jac <- vapply(m, ddm_m_to_pmsy, FUN.VALUE = 1.0)
  dlm * abs(jac)
}

## Generate `n` Pmsy values for given log Normal prior hyperparameters
rlnorm_m <- function(n, meanlog, sdlog) {
  rln <- rlnorm(n, meanlog, sdlog)
  m_to_pmsy(rln)
}

## Find the meanlog of a logNormal distribution with expected Pmsy 0.4 and given
## sdlog. WARNING limited range of working values for `sdlog`; `uniroot` will
## error if it's not in the range 0.21 < sig < 1.85. Could probably be fixed,
## but this is a good range for what I'm doing here.
find_meanlog <- function(sdlog = 1, target_pmsy = 0.4, interval = c(0, 10)) {
  ml <- uniroot(function(ml) {
    exp_pmsy(dlnorm_m, ml, sdlog) - target_pmsy
  }, lower = interval[1], upper = interval[2])$root
  ml
}

##-Gamma prior------------------------------------------------------------------
## This is the PDF of Pmsy with a Gamma prior on m
dgamma_m <- function(pmsy, shape, rate) {
  ## Need to use *apply here or the if statements in the pmsy_to_m
  ## function will only look at the first element of the `pmsy` vector
  ## (and throw a warning).
  m <- vapply(pmsy, pmsy_to_m, FUN.VALUE = 1.0)
  dlm <- dgamma(m, shape, rate)
  jac <- vapply(pmsy, ddm_pmsy_to_m, FUN.VALUE = 1.0)
  dlm * abs(jac)
}

## Generate `n` Pmsy values for given log Normal prior hyperparameters
rgamma_m <- function(n, shape, rate) {
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
    exp_pmsy(dgamma_m, gpars$shape, gpars$rate) - target_pmsy
  }, lower = interval[1], upper = interval[2])$root
  gamma_rateshape(gmean, gvar)
}

## For better interpretability, put a Beta prior on Pmsy and include the
## Jacobian correction for the density of `m`.
dbeta_pmsy <- function(m, alpha, beta) {
  pmsy <- vapply(m, m_to_pmsy, FUN.VALUE = 1.0)
  db <- dbeta(pmsy, alpha, beta)
  jac <- vapply(m, ddm_m_to_pmsy, FUN.VALUE = 1.0)
  db * abs(jac)
}

rbeta_pmsy <- function(n, alpha, beta) {
  pmsy <- rbeta(n, alpha, beta)
  vapply(pmsy, pmsy_to_m, FUN.VALUE = 1.0)
}

## Use a log-skew-normal prior on m
dlsn <- function(m, xi, omega, alpha) {
  1 / m * dsn(log(m), xi, omega, alpha)
}

dlsn_pmsy <- function(pmsy, xi, omega, alpha) {
  m <- vapply(pmsy, pmsy_to_m, 1.0)
  dm <- dlsn(m, xi, omega, alpha)
  jac <- vapply(pmsy, ddpmsy_pmsy_to_m, 1.0)
  dm * jac
}

rlsn_pmsy <- function(n, xi, omega, alpha) {
  x <- rsn(n, xi, omega, alpha)
  m <- exp(x)
  vapply(m, m_to_pmsy, 1.0)
}
