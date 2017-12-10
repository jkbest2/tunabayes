source('01_setup.R')
source('02_fitfns.R')

resT <- fitmod('Schaefer.stan',
               data = tuna_data,
               iter = 1e4, chains = 4,
               init = inits,
               cores = 1L)
fitT <- resT$fit
timeT <- resT$time
rm(resT)

resC1 <- fitmod('Schaefer_corrpriors.stan',
               data = tuna_data,
               iter = 1e4, chains = 4,
               cores = 1L)
fitC1 <- resC1$fit
timeC1 <- resC1$time
rm(resC1)

resC2 <- fitmod('Schaefer_corrpriors.stan',
               data = tuna_data,
               iter = 1e4, chains = 4,
               cores = 1L,
               control = list(adapt_delta = 0.99))
fitC2 <- resC2$fit
timeC2 <- resC2$time
rm(resC2)

resN <- fitmod('tuna_noncentered.stan',
               data = tuna_data,
               iter = 1e4, chains = 4,
               cores = 1L,
               control = list(adapt_delta = 0.99,
                              max_treedepth = 20))
fitN <- resN$fit
timeN <- resN$time
rm(resN)

save.image('fit_results.Rdata')
