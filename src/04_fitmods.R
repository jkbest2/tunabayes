source('src/01_setup.R')
source('src/02_fitfns.R')

n_iter <- 1e4
n_chain <- 4

res0 <- fitmod('src/models/0_Schaefer_T.stan',
               data = tuna_data,
               iter = n_iter, chains = n_chain,
               cores = 1L)
fit0 <- resT$fit
time0 <- resT$time
rm(res0)

res1 <- fitmod('src/models/1_Schaefer_C.stan',
               data = tuna_data,
               iter = n_iter, chains = n_chain,
               cores = 1L)
fit1 <- res1$fit
time1 <- res1$time
rm(res1)

# res2 <- fitmod('src/models/2_Schaefer_OL.stan',
#                data = tuna_data,
#                iter = n_iter, chains = n_chain,
#                cores = 1L)
# fit2 <- res2$fit
# time2 <- res2$time
# rm(res2)

res3 <- fitmod('src/models/3_Schaefer_PL.stan',
               data = tuna_data,
               iter = n_iter, chains = n_chain,
               cores = 1L)
fit3 <- res3$fit
time3 <- res3$time
rm(res3)

# res4 <- fitmod('src/models/4_Schaefer_OPL.stan',
#                data = tuna_data,
#                iter = n_iter, chains = n_chain,
#                cores = 1L)
# fit4 <- res4$fit
# time4 <- res4$time
# rm(res4)

# res5 <- fitmod('src/models/5_Schaefer_ON.stan',
#                data = tuna_data,
#                iter = n_iter, chains = 5,
#                cores = 1L)
# fit5 <- res5$fit
# time5 <- res5$time
# rm(res5)

res6 <- fitmod('src/models/6_Schaefer_PN.stan',
               data = tuna_data,
               iter = n_iter, chains = n_chain,
               cores = 1L)
fit6 <- res4$fit
time6 <- res4$time
rm(res6)

# res7 <- fitmod('src/models/7_Schaefer_OPN.stan',
#                data = tuna_data,
#                iter = n_iter, chains = n_chain,
#                cores = 1L)
# fit7 <- res7$fit
# time7 <- res7$time
# rm(res7)

save.image('results/fit_results.Rdata')
