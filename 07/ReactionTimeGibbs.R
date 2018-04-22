# Reaction time example from Unit 5 and 6

# Reaction time data (log reaction times) for first non-schizophrenic subject
reaction.times=c(5.743, 5.606, 5.858, 5.656, 5.591, 5.793, 5.697, 5.875, 5.677, 5.73, 
                 5.69, 5.919, 5.981, 5.996, 5.635, 5.799, 5.537, 5.642, 5.858, 5.793, 
                 5.805, 5.73, 5.677, 5.553, 5.829, 5.489, 5.724, 5.793, 5.684, 5.606)

require(coda)   # load the coda package

x = reaction.times
xbar = mean(x)
n = length(x)

#####
# First consider conjugate model of Unit 5
#   Assume non-informative prior distribution
#   Normal-Gamma with mu0=0, k0=0, alpha0=-1/2, beta0=infinity
#   Posterior hyperparameters mu1, k1, alpha1, beta1 
mu1 <- xbar
k1 <- n
alpha1 <- -1/2 + n/2
beta1 <- 1/(0.5*sum((x-xbar)^2))
spread1 <- sqrt(1/(k1*alpha1*beta1))

thetaVals <- 5.64+(0:100)/500
stdVals <- (thetaVals - mu1)/spread1
thetaMargDens <- dt(stdVals,df=2*alpha1)/spread1
normDens <- dnorm(thetaVals,mu1,spread1)
dens <- cbind(thetaMargDens,normDens)
matplot(thetaVals,dens,type=c("l","l"),col=c("red","blue"),xlab="Theta",ylab="Probability Density")
legend(5.76,15,c("Unknown SD (t)","Known SD (Normal)"),col=c("red","blue"),lty=c(1,2))


#Set simulation sample size
numSim <- 10000

# Simulate directly from the posterior distribution 
rhoDirect <- rgamma(numSim,shape=alpha1,scale=beta1)
sigmaDirect <- 1/sqrt(rhoDirect)
thetaDirect <- rnorm(numSim,mean=mu1,sd=sigmaDirect/sqrt(k1))

####
# Now use Gibbs sampling for semi-conjugate distribution

thetaGibbs<-xbar     #Initial guess for mean
sigmaGibbs<-sd(x)    #Initial guess for stdev
rhoGibbs<-1/sigmaGibbs[1]^2  # Initial guess for precision
for (k in 2:numSim) {
	thetaGibbs[k]<-  # note posterior mean is mu1 because k is zero
	  rnorm(1,mean=mu1,sd=sigmaGibbs[k-1]/sqrt(k1))
	alphaG<-alpha1    # This is unncecessary because alpha1 does not change
	betaG<-1/(0.5*sum((x-thetaGibbs[k])^2))
	rhoGibbs[k]<-rgamma(1,shape=alphaG,scale=betaG)
	sigmaGibbs[k]<-1/sqrt(rhoGibbs[k])
	}


#Plot theoretical and Monte Carlo density functions
plot(density(thetaDirect),col="darkgreen",lty=2,main="",xlab="Theta")
densityGibbs<-density(thetaGibbs)
lines(densityGibbs$x,densityGibbs$y,col="blue",lty=3)
lines(thetaVals,thetaMargDens,col="red")
legend(5.76,15,c("Direct MC KD","Gibbs KD","Theoretical t"),col=c("darkgreen","blue","red"),lty=c(2,3,1))

#Calculate effective sample size
effectiveSize(thetaDirect)
effectiveSize(thetaGibbs)
effectiveSize(sigmaDirect)
effectiveSize(sigmaGibbs)