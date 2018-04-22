suppressMessages(library(tidyverse))
suppressMessages(library(coda))

set.seed(8675309)

#URL of data given in assignment
www <- "http://www.biostat.umn.edu/~lynn/iid/wolf.river.dat"

#Read in the data
pollutants <- read.table(www, header=TRUE, stringsAsFactors = F)

pollutants %<>%
    mutate(id = rep(seq(1, 10, by = 1), 3)) %>%
    select(-Aldrin) %>%
    spread(Depth, HCB) %>%
    select(-id, -Middepth)


#Prior Information----
#Theta
mu.0 <- 6
tau.0 <- 1.5

#P
alpha.0 <- 4.5
beta.0 <- 0.19
alpha.star <- alpha.0 + (length(pollutants$Bottom) + length(pollutants$Surface)) / 2

thetaS <- c(mean(pollutants$Surface))
thetaB <- c(mean(pollutants$Bottom))
sigma <- c(sd(c(pollutants$Surface, pollutants$Bottom)))
rho <- c(1 / sigma[1]^2)

muS <- c(mu.0)
muB <- c(mu.0)
tauS <- c(tau.0)
tauB <- c(tau.0)
Beta <- c(beta.0)

numSim <- 10000
nS <- length(pollutants$Surface)
nB <- length(pollutants$Bottom)

for(j in 2:numSim){
    muS[j] <- ((mu.0/tau.0^2) + (rho[j-1] * sum(pollutants$Surface))) / (tau.0^-2 + nS * rho[j-1])
    tauS[j] <- (tau.0^-2 + nS * rho[j-1]) ^ (-0.5)
    thetaS[j] <- rnorm(1, mean = muS[j], sd = tauS[j])
    
    muB[j] <- ((mu.0/tau.0^2) + (rho[j-1] * sum(pollutants$Bottom))) / (tau.0^-2 + nB * rho[j-1])
    tauB[j] <- (tau.0^-2 + nB * rho[j-1]) ^ (-0.5)
    thetaB[j] <- rnorm(1, mean = muB[j], sd = tauB[j])
    
    Beta[j] <- (1/beta.0 + 1/2 * sum((pollutants$Surface - thetaS[j])^2 + (pollutants$Bottom - thetaB[j])^2)) ^ -1
    rho[j] <- rgamma(1, shape=alpha.star, scale=Beta[j])
    sigma[j]<-1/sqrt(rho[j])
}


thetaS.quant <- sprintf("[%s, %s]", round(quantile(thetaS, .025), 3), round(quantile(thetaS, 0.975), 3))
thetaB.quant <- sprintf("[%s, %s]", round(quantile(thetaB, .025), 3), round(quantile(thetaB, 0.975), 3))
sigma.quant <- sprintf("[%s, %s]", round(quantile(sigma, .025), 3), round(quantile(sigma, 0.975), 3))
thetaBS.quant <- sprintf("[%s, %s]", round(quantile(thetaB - thetaS, .025), 3), round(quantile(thetaB - thetaS, 0.975), 3))



quantile(thetaB, c(.025, 0.975))
quantile(sigma, c(.025, 0.975))
quantile(thetaB - thetaS, c(.025, 0.975))

plot(thetaB - thetaS)
acf(thetaB - thetaS)

#Calculate effective sample size
effectiveSize(thetaB - thetaS)
effectiveSize(sigma)



