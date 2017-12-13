source('src/01_setup.R')
source('src/02_fitfns.R')

n_iter <- 1e4
n_chain <- 4

fit0 <- stan('src/models/0_Schaefer_T.stan',
             data = tuna_data,
             iter = n_iter, chains = n_chain)
saveRDS(fit0, 'results/fit0.Rds')

fit1 <- stan('src/models/1_Schaefer_C.stan',
             data = tuna_data,
             iter = n_iter, chains = n_chain)
saveRDS(fit1, 'results/fit1.Rds')

# fit2 <- stan('src/models/2_Schaefer_OL.stan',
#                data = tuna_data,
#                iter = n_iter, chains = n_chain)
# saveRDS(fit2, 'results/fit2.Rds')

fit3 <- stan('src/models/3_Schaefer_PL.stan',
               data = tuna_data,
               iter = n_iter, chains = n_chain)
saveRDS(fit3, 'results/fit3.Rds')

# fit4 <- stan('src/models/4_Schaefer_OPL.stan',
#                data = tuna_data,
#                iter = n_iter, chains = n_chain)
# saveRDS(fit4, 'results/fit4.Rds')

# fit5 <- stan('src/models/5_Schaefer_ON.stan',
#                data = tuna_data,
#                iter = n_iter, chains = 5)
# saveRDS(fit5, 'results/fit5.Rds')

fit6 <- stan('src/models/6_Schaefer_PN.stan',
               data = tuna_data,
               iter = n_iter, chains = n_chain)
saveRDS(fit6, 'results/fit6.Rds')

# fit7 <- stan('src/models/7_Schaefer_OPN.stan',
#                data = tuna_data,
#                iter = n_iter, chains = n_chain)
# saveRDS(fit7, 'results/fit7.Rds')

