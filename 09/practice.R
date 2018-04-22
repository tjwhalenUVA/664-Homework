library(tidyverse)
library(magrittr)

#Data URL
www <- "http://www.stat.columbia.edu/~gelman/book/data/schiz.asc"
#Read and Wrangle
nschiz <- read.table(www, skip = 5) %>% 
    mutate(condition = rep(c("non-schizophrenics", "schizophrenics"), 
                           c(11, 6))) %>%
    filter(condition == 'non-schizophrenics') %>% 
    select(-condition) %>%
    mutate(subject = sprintf("p%s", 1:11)) %>%
    gather(reaction, ms, -subject) %>%
    mutate(lms = log(ms))

nschiz %<>% mutate(subject = factor(subject, levels = unique(nschiz$subject)))

init.nschiz <- 
    nschiz %>%
    group_by(subject) %>%
    summarise(yBar = mean(lms),
              ySSQ = sum(lms^2),
              nSub = n(),
              ySD = sd(lms))

# Data
nSub <- length(unique(nschiz$subject))
nSubL <- init.nschiz$nSub
ySSQ <- init.nschiz$ySSQ
yBar <- init.nschiz$yBar
ySD <- init.nschiz$ySD

# Prior hyperparameters
mu.mean.0 = 5.52
mu.SD.0 = 0.22
tau.sh.0 = sig.sh.0 = 1/2
tau.sc.0 = sig.sc.0 = 50

## Initialize Gibbs sample variables
numSim = 5000
thetaG = matrix(nrow = numSim, ncol = nSub)
thetaG[1,] = yBar    # initial value for theta
muG = mean(yBar)    # initial value for mu
tauG = sd(yBar)     # initial value for tau
sigG = mean(ySD)    # initial value for sigma

k=2
# Draw the Gibbs samples
for (k in 2:numSim) {
    mu.mean.1 = (mu.mean.0/mu.SD.0^2+sum(thetaG[k-1,])/tauG[k-1]^2)/(1/mu.SD.0^2+nSub/tauG[k-1]^2)
    mu.SD.1 = (1/mu.SD.0^2+nSub/tauG[k-1]^2)^-(1/2)
    muG[k] = rnorm(1,mu.mean.1, mu.SD.1)    # sample new mu
    
    tau.sh.1 = tau.sh.0 + 0.5*nSub  # this is the same every iteration
    tau.sc.1 = (1/tau.sc.0 + 0.5*sum((thetaG[k-1,]-muG[k])^2))^-1
    tauG[k] = rgamma(1,shape=tau.sh.1,scale=tau.sc.1)^(-1/2)
    
    sig.sh.1 = sig.sh.0 + 0.5*sum(nSubL)  # this is the same every iteration
    sig.sc.1 = (1/sig.sc.0 + 0.5*sum(ySSQ - nSubL*yBar^2 + nSubL*(yBar - thetaG[k-1,])^2))^-1
    sigG[k] = rgamma(1,shape=sig.sh.1,scale=sig.sc.1)^(-1/2)
    
    mu.1 = (muG[k]/tauG[k]^2+yBar*nSubL/sigG[k]^2)/(1/tauG[k]^2+nSubL/sigG[k]^2) # vector of posterior means
    tau.1 = (1/tauG[k]^2+nSubL/sigG[k]^2)^(-1/2)  # vector of posterior SDs
    thetaG[k,] = rnorm(nSub,mu.1,tau.1)
    
}

theta.hat = colMeans(thetaG)
th.lower=th.upper=NULL
for (i in 1:nSub) {
    th.lower[i] = quantile(thetaG[,i],0.025)
    th.upper[i] = quantile(thetaG[,i],0.975)
}

# Shrinkage plot for Theta
# This is the same as Figure 8.8 in Hoff except it includes 95% intervals
plot(yBar,theta.hat,main = "Reaction Time Means for 11 Subjects",
     ylim = c(min(th.lower),max(th.upper)),
     ylab = "Mean and 95% Interval for Theta")
lines(yBar,yBar)
for (i in 1:nSub) {
    lines(c(yBar[i],yBar[i]),c(th.lower[i],th.upper[i]))
}

# Normal qq-plot for Theta
qqnorm(theta.hat)
qqline(theta.hat)

# Diagnostics - note all theta values have very high effective sample size
library(coda)

data.frame('numSim' = numSim, 
           'Mu' = round(effectiveSize(muG)[[1]],2),
           'Tau' = round(effectiveSize(tauG)[[1]],2), 
           'Sigma' = round(effectiveSize(sigG)[[1]],2)) %>%
    knitr::kable(.)


# Traceplots look stationary
plot(1:numSim,muG, main="Traceplot for mean of subject means")
plot(1:numSim,tauG, main="Traceplot for SD of subject means")
plot(1:numSim,sigG, main="Traceplot for SD of reaction times")



#Assignemtn 8 CI
# nschiz <- ppl %>%
#     filter(., condition == 'non-schizophrenics') %>% 
#     select(-condition) %>%
#     mutate(subject = sprintf("p%s", 1:11)) %>%
#     gather(reaction, ms, -subject) %>%
#     mutate(lms = log(ms))

ns.param <- 
    nschiz %>%
    group_by(subject) %>%
    summarise(variance = var(lms)) %>%
    mutate(precision = 1 / variance) %>%
    ungroup()

ns.beta <- var(ns.param$precision)/mean(ns.param$precision)
ns.alpha <- mean(ns.param$precision) / ns.beta
ns.mu <- mean(nschiz$lms)

ns.k <- nschiz %>%
    group_by(subject) %>%
    summarise(smean = mean(lms)) %>%
    ungroup() %>%
    summarise(svar = var(smean)) %>%
    mutate(sprec = 1/ svar,
           sk = sprec / mean(ns.param$precision)) %>%
    select(sk)
ns.k <- ns.k[[1]]

findPost <- function(s){
    subjSamples <- filter(nschiz, subject == s)$lms
    nSubj <- nrow(filter(nschiz, subject == s))
    xbarSubj <- mean(subjSamples)
    #Alpha
    aStar <- ns.alpha + nSubj / 2
    #Beta
    b.p1 <- 0.5 * sum((subjSamples - xbarSubj)^2)
    b.p2 <- ns.k * nSubj * (xbarSubj - ns.mu)^2 / (2 * (ns.k + nSubj))
    bStar <- (ns.beta^-1 + b.p1 + b.p2)^-1
    #Mu
    muStar <- (ns.k * ns.mu + sum(subjSamples)) / (ns.k + nSubj)
    #k
    kStar = ns.k + nSubj
    
    res <- list("mu" = muStar, "k" = kStar, "alpha" = aStar, "beta" = bStar)
    return(res)
}

nsPost <- NULL
for(s in unique(nschiz$subject)){
    if(is.null(nsPost)){
        nsPost <- data.frame(subject = s, findPost(s))
    }
    else{
        nsPost %<>% bind_rows(., data.frame(subject = s, findPost(s)))
    }
}

nsNorm <-
    nsPost %>%
    mutate(sigma = sqrt(1/(alpha * beta * k))) %>%
    select(mu, sigma) %>%
    as.matrix()
nsPostDens <- data.frame(theta = seq(5.45,6,length.out = 1000))
for(r in 1:nrow(nsNorm)){
    s <- sprintf("p%s", r)
    nsPostDens[s] <- dnorm(nsPostDens$theta, nsNorm[r,'mu'], nsNorm[r,'sigma'])
}

df.plot <-
    nsPost %>%
    mutate(Low = qnorm(0.025, mu, sqrt(1/(alpha * beta * k))),
           High = qnorm(0.975, mu, sqrt(1/(alpha * beta * k))),
           Assignment = 8) %>%
    select(-mu, -k, -alpha, -beta) %>%
    bind_rows(.,
              data.frame('subject' = sprintf("p%s", 1:11),
                         'Assignment' = rep(9, length(th.lower)),
                         'Low' = th.lower,
                         'High' = th.upper)) %>%
    mutate(Assignment = as.character(Assignment),
           subject = factor(subject, levels = unique(nschiz$subject)))


df.plot %>%
    ggplot() +
    geom_segment(aes(y=Assignment, yend=Assignment,
                     x=Low, xend=High,
                     color=Assignment),
                 size=4) +
    facet_wrap(~subject, scales = 'free_x') +
    theme_classic() +
    labs(title='95% Credible Intervals',
         x=expression(theta),
         y='Subject') +
    theme(panel.grid.major.y = element_line(color='gray80'),
          panel.grid.major.x = element_line(),
          legend.position = c(.95,0),
          legend.justification = c(1,0))


data.frame(thetaG) %>%
    gather(subject, samples) %>%
    ggplot() +
    geom_violin(aes(x=subject, y=samples,
                    fill=subject))



