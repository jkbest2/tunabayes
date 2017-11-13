library(runjags)


model <- "model {
 for(i in 1 : N){ 
 Y[i] ~ dnorm(true.y[i], precision) 
 true.y[i] <- (coef * X[i]) + int 
 }
 coef ~ dunif(-1000,1000)
 int ~ dunif(-1000,1000)
 sigma <- 1.0/sqrt(precision)
 precision ~ dexp(1)
 }"

set.seed(1)
X <- 1:100
Y <- rnorm(length(X),2*X+10,1)
N <- length(X)

data <- list(X=X, Y=Y, N=length(X))
inits1 <- list(coef=1, int=1, precision=1,
               .RNG.name="base::Super-Duper", .RNG.seed=1)
inits2 <- list(coef=0.1, int=10, precision=1,
               .RNG.name="base::Wichmann-Hill", .RNG.seed=21)
inits1 <- list(coef=1, int=1, precision=1)
inits2 <- list(coef=0.1, int=10, precision=1)
inits3 <- list(coef=0.1, int=10, precision=1,
               .RNG.name="base::Wichmann-Hill", .RNG.seed=9)
inits4 <- list(coef=0.1, int=10, precision=1,
               .RNG.name="base::Wichmann-Hill", .RNG.seed=4)
inits5 <- list(coef=0.1, int=10, precision=1,
               .RNG.name="base::Wichmann-Hill", .RNG.seed=3)
inits <- list(inits1,inits2,inits3,inits4,inits5)

Y[3] <- NA
Y[5] <- NA

results <- run.jags(model=model, monitor=c("coef", "int", "precision","sigma"), 
         data=data, n.chains=5, method="rjags", inits=inits,plots=T,monitor.deviance=T,
         silent.jag=T)
         
print(results)
plot(results, layout = runjags.getOption("plot.layout"),
     new.windows = runjags.getOption("new.windows"), file = "testjags.pdf")


