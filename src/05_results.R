library(rstan)
library(bayesplot)
library(tidyverse)

load('fit_results.Rdata')

sampparT <- get_sampler_params(fitT)
sampparC1 <- get_sampler_params(fitC1)
sampparC2 <- get_sampler_params(fitC2)
sampparN <- get_sampler_params(fitN)

divT <- sum(sapply(sampparT, function(x) sum(x[, 'divergent__'])))
divC1 <- sum(sapply(sampparC1, function(x) sum(x[, 'divergent__'])))
divC2 <- sum(sapply(sampparC2, function(x) sum(x[, 'divergent__'])))
divN <- sum(sapply(sampparN, function(x) sum(x[, 'divergent__'])))

pars <- c('K', 'r', 'q', 'sigma', 'tau')

summT <- summary(fitT)$summary
summC1 <- summary(fitC1)$summary
summC2 <- summary(fitC2)$summary
summN <- summary(fitN)$summary

df <- data_frame(Model = c('T', 'C1', 'C2', 'N'),
            Time = c(timeT, timeC1, timeC2, timeN),
            Div = c(divT, divC1, divC2, divN),
            summ = list(summT, summC1, summC2, summN))

df %>%
    transmute(Model = Model,
         Min_Par = map_chr(summ,
                           function(m) rownames(m)[which.min(m[, 'n_eff'])]),
         Min_Eff = round(map_dbl(summ, function(m) min(m[, 'n_eff']))),
         Time = Time,
         Eff_per_Sec = Min_Eff / as.numeric(Time),
         Divergences = Div) -> min_eff

df %>%
    transmute(Model = Model,
         K = map_dbl(summ, function(m) m['K', 'mean']),
         K_se = map_dbl(summ, function(m) m['K', 'se_mean'])) -> Kmean
Kmean <- bind_rows(data_frame(Model = 'MM_ss', K = 279.8, K_se = NA),
                   Kmean)

df %>%
    transmute(Model = Model,
         r = map_dbl(summ, function(m) m['r', 'mean']),
         r_se = map_dbl(summ, function(m) m['r', 'se_mean'])) -> rmean
rmean <- bind_rows(data_frame(Model = 'MM_ss', r = 0.293, r_se = NA),
                   rmean)

df %>%
    transmute(Model = Model,
              q = map_dbl(summ, function(m) m['q', 'mean']),
              q_se = map_dbl(summ, function(m) m['q', 'se_mean'])) -> qmean
qmean <- bind_rows(data_frame(Model = 'MM_ss', q = 23.89, q_se = NA),
                   qmean)
