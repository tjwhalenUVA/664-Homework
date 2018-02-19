# This code produces the triplot for the transmission error example from Units 2 and 3
# Prior distribution is Gamma with shape 2 and scale 0.48
# There are 5 errors in 6 observations

# The data
errors <- c(1,0,1,2,1,0)

# Discretized version -- Unit 2 (also p. 21 of Unit 3)
lambdaDisc <- seq(length=15, from=0.2, to=3)  # Grid of values for parameter lambda
priorDist <- c(0.110, 0.148, 0.149, 0.133, 0.111, 0.090, 0.070, 0.054, 0.040, 0.030, 0.022, 0.016, 0.012, 0.009, 0.006) # First column of table on Page 46 of Unit 2
lik=array(1,length(lambdaDisc))          # Initialize likelihood to all 1's
for(e in errors) {                       # For each time data point...
	lik<-lik*dpois(e,lambdaDisc)}        #  ...multiply by likelihood for that data point 
postDist <- priorDist*lik                # Prior times likelihood 
postDist <- postDist/sum(postDist)       # Normalize to get posterior
normLikD <- lik/sum(lik)                 #Normalized likelihood

PriLikPost=rbind(priorDist,normLikD,postDist)   # Bind into single object for plotting
barplot(PriLikPost,main="Triplot for Lambda (Discretized)", xlab="lambda", ylab="Probability", col=c("lightblue","lightgreen","pink"), border=c("darkblue","darkgreen","red"),names.arg=lambdaDisc, beside=TRUE,legend=c("Prior","Norm Lik","Posterior"),ylim=c(0,0.3))


# Continuous version -- page 17 of Unit 3 (also p. 21 of Unit 3)
lambda=seq(length=100,from=0.03,to=3)
alpha0=2; beta0=0.48      # prior shape and scale
priorDens=dgamma(lambda,shape=alpha0,scale=beta0) # Prior density
alpha1=alpha0+sum(errors)             # posterior shape
beta1=1/(1/beta0+length(errors))      # posterior scale
postDens=dgamma(lambda,shape=alpha1,scale=beta1)  # Posterior density
normLikC=dgamma(lambda,               # Normalized Likelihood is Gamma(6,1/6)
                shape=sum(errors)+1,scale=1/length(errors))    

plot(lambda,priorDens,type="l",col="blue",main="Triplot for Transmission Error Rate (Continuous)",
     xlab="Error Rate (errors per hour)",ylab="Probability Density",
     xlim=c(0,3),ylim=c(0,1.3))
lines(lambda,normLikC,col="green")
lines(lambda,postDens,col="red")
legend(2.0,1.0,c("Prior","Norm Lik","Posterior"),col=c("blue","green","red"),lty=c(1,1,1))

