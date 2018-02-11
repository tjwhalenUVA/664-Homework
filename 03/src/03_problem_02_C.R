#C====
# Assume that L, the rate parameter of the Poisson distribution 
# (and the inverse of the mean of the exponential distribution), 
# has a discrete uniform prior distribution on 20 equally spaced values between 
# (0.2, 0.4, …, 3.8, 4.0) cars per 15-second interval. Find the posterior mean, 
# standard deviation, median and 95  percentile of L given the observations. 
# Describe what your results mean in terms of traffic on this motorway.

lambda <- seq(0.2, 4.0, length.out = 20)  # Grid of values for parameter lambda

# Prior distribution on transmission error rate
priorDist <- rep(1/20, 20)

# The observations
errors <- CarsBy15Sec

# The likelihood function is a product of Poisson pmfs
lik <- array(1,length(lambda))         # Initialize likelihood as a constant
for (i in 1:length(errors)) {
    lik <- lik*dpois(errors[i],lambda)   # Multiply by Likelihood 
}

# The posterior distribution
postDist <- priorDist*lik           # Prior times likelihood
postDist <- postDist/sum(postDist)  # Normalize to sum to 1

#Features of the posterior distribution
postMean = sum(lambda*postDist)
postVar = sum((lambda-postMean)^2*postDist)
postSD = sqrt(postVar)
#Median
dft <- 
    data.frame('lamb' = lambda, 
               'pdf' = postDist) %>%
    mutate(cdf = cumsum(pdf))

ls <- 1
p2.c.plot.1 <- 
    dft %>%
    ggplot() +
    geom_hline(yintercept = 0.5, 
               color='green', 
               linetype='dashed', 
               size=ls) +
    geom_label(aes(label='50%', 
                   x=1, 
                   y=.5), 
               fill='green') +
    geom_hline(yintercept = 0.95, 
               color='khaki', 
               linetype='dashed', 
               size=ls) +
    geom_label(aes(label='95%', 
                   x=1, 
                   y=.95), 
               fill='khaki') +
    geom_step(aes(x=lamb, 
                  y=cdf, 
                  color='cdf'), 
              size=ls, 
              show.legend = F) +
    theme_dark() +
    labs(title='Cumulative Distribution Function', 
         y=NULL, 
         x=NULL)

p2.c.plot.2 <- 
    dft %>%
    ggplot() +
    geom_vline(xintercept = postMean, 
               color='khaki', 
               linetype='dashed', 
               size=ls) +
    geom_label(aes(label=round(postMean, 2), 
                   x=postMean, 
                   y=.32), 
               fill='khaki') +
    geom_vline(xintercept = postMean + postSD, 
               color='lightgreen', 
               linetype='dashed', 
               size=ls) +
    geom_label(aes(label=round(postMean+postSD, 2), 
                   x=postMean+postSD, 
                   y=.28), 
               fill='lightgreen') +
    geom_vline(xintercept = postMean - postSD, 
               color='lightgreen', 
               linetype='dashed', 
               size=ls) +
    geom_label(aes(label=round(postMean-postSD, 2), 
                   x=postMean-postSD, 
                   y=.28), 
               fill='lightgreen') +
    geom_line(aes(x=lamb, 
                  y=pdf, 
                  color='pdf'), 
              show.legend = F) +
    geom_point(aes(x=lamb, 
                   y=pdf, 
                   color='pdf'), 
               show.legend = F) +
    theme_dark() +
    labs(title='Probability Density Function', 
         y=NULL, 
         x=NULL)

# Bar chart of prior and posterior distributions
# barplot(priorDist,main=expression("Prior distribution for Error Rate "~Lambda),
#         xlab=expression(lambda),ylab="Probability", col="lightblue",
#         border="darkblue",names.arg=lambda,ylim=c(0, 0.25))
# barplot(postDist,main=expression("Posterior distribution for Error Rate "~Lambda),
#         xlab=expression(lambda),ylab="Probability", col="lightblue",
#         border="darkblue",names.arg=lambda,ylim=c(0, 0.25))
# 
# normLik <- lik/sum(lik)      # Normalized likelihood
# PriLikPost=rbind(priorDist,normLik,postDist)
# barplot(PriLikPost,main=expression("Triplot for Transmission Error Rate"~Lambda),
#         xlab=expression(lambda), ylab="Probability", col=c("lightblue","lightgreen","pink"),
#         border=c("darkblue","darkgreen","red"),names.arg=round(lambda,2),
#         beside=TRUE,legend=c("Prior","Norm Lik","Posterior"),ylim=c(0,0.3))


#Process observations sequentially and do a triplot at each step
df.seq <- NULL
df.seq$i <- NA
df.seq$lambda <- NA
df.seq$prior <- NA
df.seq$post <- NA
df.seq$norm <- NA

df.seq <- as.data.frame(df.seq)

for (i in 1:length(errors)) {
    num <- errors[i]                            # transmission errors in ith hour
    postDist <- priorDist * dpois(num,lambda)   # Prior times Likelihood
    postDist <- postDist/sum(postDist)          # Normalize to get posterior after ith hour
    normLik <- dpois(num,lambda)/sum(dpois(num,lambda)) # Normalized likelihood
    PriLikPost=rbind(priorDist,normLik,postDist)
    
    df.tmp <- NULL
    df.tmp$i <- i
    df.tmp$lambda <- lambda
    df.tmp$prior <- priorDist
    df.tmp$post <- postDist
    df.tmp$norm <- normLik
    
    df.seq<- 
        df.seq %>%
        bind_rows(., 
                  as.data.frame(df.tmp))
    
    # barplot(PriLikPost,main=paste("Triplot for Arrival Rate at 15 Second Block ",i),
    #         xlab=expression(lambda), ylab="Probability", col=c("lightblue","lightgreen","pink"),
    #         border=c("darkblue","darkgreen","red"),names.arg=round(lambda,2),
    #         beside=TRUE,legend=c("Prior","Norm Lik","Posterior"),ylim=c(0,0.3))
    
    priorDist <- postDist     # posterior at this step becomes prior for next step
}


seq.plot <-
    df.seq %>%
    filter(!is.na(i)) %>%
    gather(Distribution, Value, -i, -lambda) %>%
    ggplot(aes(x=lambda, 
               y=Value, 
               fill=Distribution)) +
    geom_bar(stat = 'identity', 
             position = 'dodge') +
    facet_wrap(~i, ncol = 3) +
    theme_dark()
