# Transmission error example from Unit 2

# Poisson probability mass function for current system and design goal
currentpmf = dpois(0:7, 1.6)
barplot(currentpmf,main=expression("Tranmission Error pmf Given Current Rate"~lambda~"=1.6"),
        xlab="Number of Errors",ylab="Probability", col="mediumpurple1", 
        border="purple4",names.arg=0:7,ylim=c(0, 0.45))

designpmf = dpois(0:7, 0.8)
barplot(designpmf,main=expression("Tranmission Error pmf Given Design Goal"~lambda~"=0.8"),
        xlab="Number of Errors",ylab="Probability", col="mediumpurple1", 
        border="purple4",names.arg=0:7,ylim=c(0, 0.45))

# Discretized parameter values (15 equally spaced values)
lambda <- seq(length=15, from=0.2, to=3)  # Grid of values for parameter lambda

# Prior distribution on transmission error rate
priorDist <- c(0.110, 0.148, 0.149, 0.133, 0.111, 0.090, 0.070, 
               0.054, 0.040, 0.030, 0.022, 0.016, 0.012, 0.009, 0.006)

# The observations
errors <- c(1,0,1,2,1,0)

# The likelihood function is a product of Poisson pmfs
lik <- array(1,length(lambda))         # Initialize likelihood as a constant
for (i in 1:6) {
  lik <- lik*dpois(errors[i],lambda)   # Multiply by Likelihood 
}

# The posterior distribution
postDist <- priorDist*lik           # Prior times likelihood
postDist <- postDist/sum(postDist)  # Normalize to sum to 1

#Features of the posterior distribution
postMean = sum(lambda*postDist)
postVar = sum((lambda-postMean)^2*postDist)
postSD = sqrt(postVar)



# Bar chart of prior and posterior distributions
barplot(priorDist,main=expression("Prior distribution for Error Rate "~Lambda),
        xlab=expression(lambda),ylab="Probability", col="lightblue", 
        border="darkblue",names.arg=lambda,ylim=c(0, 0.25))
barplot(postDist,main=expression("Posterior distribution for Error Rate "~Lambda),
        xlab=expression(lambda),ylab="Probability", col="lightblue", 
        border="darkblue",names.arg=lambda,ylim=c(0, 0.25))

# Triplot shows prior, normalized likelihood and posterior
normLik <- lik/sum(lik)      # Normalized likelihood
PriLikPost=rbind(priorDist,normLik,postDist)
barplot(PriLikPost,main=expression("Triplot for Transmission Error Rate"~Lambda), 
        xlab=expression(lambda), ylab="Probability", col=c("lightblue","lightgreen","pink"), 
        border=c("darkblue","darkgreen","red"),names.arg=round(lambda,2), 
        beside=TRUE,legend=c("Prior","Norm Lik","Posterior"),ylim=c(0,0.3))



#Process observations sequentially and do a triplot at each step

priorDist <- c(0.110, 0.148, 0.149, 0.133, 0.111, 0.090, 0.070, 
               0.054, 0.040, 0.030, 0.022, 0.016, 0.012, 0.009, 0.006)
for (i in 1:6) {
	num <- errors[i]                            # transmission errors in ith hour
	postDist <- priorDist * dpois(num,lambda)   # Prior times Likelihood
	postDist <- postDist/sum(postDist)          # Normalize to get posterior after ith hour
	normLik <- dpois(num,lambda)/sum(dpois(num,lambda)) # Normalized likelihood
	PriLikPost=rbind(priorDist,normLik,postDist)
	
	barplot(PriLikPost,main=paste("Triplot for Error Rate at Hour ",i), 
	        xlab=expression(lambda), ylab="Probability", col=c("lightblue","lightgreen","pink"), 
	        border=c("darkblue","darkgreen","red"),names.arg=round(lambda,2), 
	        beside=TRUE,legend=c("Prior","Norm Lik","Posterior"),ylim=c(0,0.3))

	priorDist <- postDist     # posterior at this step becomes prior for next step
}

#Process observations sequentially and predict next observation at each step
layout(matrix(c(1:6),2,3,byrow=TRUE))  # Put all six plots on one page
priorDist <- c(0.110, 0.148, 0.149, 0.133, 0.111, 0.090, 0.070, 0.054, 0.040, 0.030, 0.022, 0.016, 0.012, 0.009, 0.006)
predDist <- rep(0,10)
for (i in 1:6) {
  lambdaMean=sum(lambda*priorDist)
  for (k in 1:length(predDist)) {
    predDist[k]=sum(dpois(k-1,lambda)*priorDist)
  }
  predPois=rbind(predDist,dpois(0:9,lambdaMean)) # Bayesian and Poisson predictive

  barplot(predPois,main=paste("Predicting Errors in Hour",i),xlab="Number of Errors",ylab="Probability", 
          col=c("turquoise1","mediumpurple"), border=c("turquoise4","purple4"),names.arg=0:9, beside=TRUE,
          legend=c("Marginal Likelihood","Poisson"),ylim=c(0,0.5))
  num <- errors[i]                            # transmission errors in ith hour
  postDist <- priorDist * dpois(num,lambda)   # Prior times Likelihood
  postDist <- postDist/sum(postDist)          # Normalize to get posterior after ith hour
  
  priorDist <- postDist     # posterior at this step becomes prior for next step
}
layout(matrix(c(1),1,1))    # Go back to single plot per page


