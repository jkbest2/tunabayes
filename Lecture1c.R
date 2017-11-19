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
              iter = 50000, chains = 2,init=inits, warmup=5000,verbose=F)
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




