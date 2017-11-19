library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("D:\\courses\\FISH 558_17\\Stan\\")


C<-c(15.9,25.7,28.5,23.7,25.0,33.3,28.2,19.7,17.5,19.3,21.6,23.1,22.5,22.5,23.6,29.1,14.4,13.2,28.4,34.6,37.5,25.9,25.3) 
I<-c(61.89,78.98,55.59,44.61,56.89,38.27,33.84,36.13,41.95,36.63,36.33,38.82,34.32,37.64,34.01,32.16,26.88,36.61,30.07,30.75,23.36,22.36,21.91)
N<-23

tuna_data <- list(C=C,I=I,N=N)

set.seed(1901)

#initial values
inits1 <- list(r=0.8, K=200, iq=0.5, isigma2=100, itau2=100, P=c(0.99,0.98,0.96,0.94,0.92,0.90,0.88,0.86,0.84,0.82,0.80,0.78,0.76,0.74,0.72,0.70,0.68,0.66,0.64,0.62,0.60,0.58,0.56))
inits2 <- list(r=0.5, K=300, iq=0.8, isigma2=200, itau2=200, P=c(0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99))
inits <- list(inits1,inits2)

fit <- stan(file = 'Schaefer.stan', data = tuna_data, 
              iter = 4000, chains = 2,init=inits, warmup=2000,verbose=F)
print(fit)
la <- extract(fit, permuted = TRUE) # return a list of arrays 

par(mfrow=c(2,2))
quants <- matrix(0,nrow=3,ncol=N)
for (II in 1:N)
  quants[,II] <- quantile(la$Biomass[,II],probs=,c(0.05,0.5,0.95))
ymax <- max(quants)*1.05
plot(1:N,quants[2,],xlab="Year",ylab="Biomass",type="b",lty=1,pch=16,ylim=c(0,ymax))
lines(1:N,quants[1,],lwd=1,lty=2)
lines(1:N,quants[3,],lwd=1,lty=2)

quants <- matrix(0,nrow=3,ncol=N)
for (II in 1:N)
  quants[,II] <- quantile(la$Inew[,II],probs=,c(0.05,0.5,0.95))
ymax <- max(quants)*1.05
plot(1:N,I,xlab="Year",ylab="Index",type="b",lty=1,pch=16,ylim=c(0,ymax))
lines(1:N,quants[2,],lwd=2,lty=1)
lines(1:N,quants[1,],lwd=1,lty=2)
lines(1:N,quants[3,],lwd=1,lty=2)



init_f <- function() {
    list(K = rlnorm(1, 5.0429, 0.5162),
         r = rlnorm(1, -1.38, 0.51),
         tau2 = 1 / rgamma(1, 1.709, 0.00861),
         sigma2 = 1 / rgamma(1, 3.785, 0.0102),
         q = rexp(1),
         P = rlnorm(23, 0, 0.1))
}

color_scheme_set("brightblue")
mcmc_parcoord(draws, alpha = 0.05)
mcmc_parcoord(draws, np = np)
# customize appearance of divergences
color_scheme_set("darkgray")
div_style <- parcoord_style_np(div_color = "green", div_size = 0.05, div_alpha = 0.4)
mcmc_parcoord(draws, size = 0.25, alpha = 0.1,
np = np, np_style = div_style)
# to use a transformation (e.g., to standarde all the variables)
# specify the 'transformations' argument (though partial argument name
# matching means we can just use 'trans' or 'transform')

library(bayesplot)

fit <- stan('tuna_centered.stan', data = tuna_data, iter = 1e4, init = init_f)
draws <- as.array(fit)
np <- nuts_params(fit)

mcmc_parcoord(
draws,
pars = c('r', 'K', 'sigma', 'tau'),
transform = function(x) {(x - mean(x)) / sd(x)},
size = 0.25,
alpha = 0.1,
np = np,
np_style = div_style
)
