p2_mc <- function(depth){
    x <- pollutants[,depth]
    xbar <- mean(x)
    n <- length(x)
    # Assume non-informative prior distribution
    # Normal-Gamma with mu0=0, k0=0, alpha0=-1/2, beta0=infinity
    # Posterior hyperparameters mu1, k1, alpha1, beta1
    mu1 <- xbar
    k1 <- n
    alpha1 <- -1/2 + n/2
    beta1 <- 1/(0.5*sum((x-xbar)^2))
    spread1 <- sqrt(1/(k1*alpha1*beta1))
    #Theoretical marginal density for theta
    thetaVals <- seq(xbar - sd(x), xbar + sd(x), by = 0.01)
    stdVals <- (thetaVals - mu1)/spread1
    thetaMargDens <- dt(stdVals,df=2*alpha1)/spread1
    #Set simulation sample size
    numSim <- 10000
    # Simulate directly from the posterior distribution 
    rhoDirect <- rgamma(numSim,shape=alpha1,scale=beta1)
    sigmaDirect <- 1/sqrt(rhoDirect)
    thetaDirect <- rnorm(numSim,mean=xbar,sd=sigmaDirect/sqrt(n))
    #Plot theoretical and Monte Carlo density functions
    compare.plot <-
        ggplot() +
        geom_line(data = data.frame('x' = thetaVals,
                                    'density' = thetaMargDens),
                  aes(x=x,
                      y=density, 
                      color = 'Theoretical')) +
        geom_line(data = data.frame('x' = thetaDirect), 
                  aes(x=x, 
                      color = 'Monte Carlo'),
                  stat = 'density') +
        labs(title = sprintf('Monte Carlo Estimate for %s Depth', depth),
             y='Density',
             x='Theta') +
        theme_classic() +
        xlim(c(xbar - sd(x), xbar + sd(x)))
    #Return a list with a plot and the monte carlo simulated values
    res <- list('plot' = compare.plot,
                'thetaVals' = thetaDirect,
                'rhoVals' = rhoDirect)
    return(res)
}


mc.surface.theta.ci <- quantile(p2_mc('Surface')$thetaVals, probs = c(.05, .95))
mc.surface.rho.ci <- quantile(p2_mc('Surface')$rhoVals, probs = c(.05, .95))

mc.bottom.theta.ci <- quantile(p2_mc('Bottom')$thetaVals, probs = c(.05, .95))
mc.bottom.rho.ci <- quantile(p2_mc('Bottom')$rhoVals, probs = c(.05, .95))


