# Rat tumor example from SYST 664 / CSI 674 Unit 7

# Read in the data and define the variables (set working directory to
#  location of file "rats.txt")
rats <- read.table ("rats.txt", header=T)  
nS = nrow(rats)      # Number of studies
nt <- rats$tumors    # Number of tumors in each study
nr <- rats$numRats   # Number of rats in each study

require(coda)        # MCMC diagnostics

q.hat <- nt/nr          # Frequency of tumors in each study

#Gibbs sample

numSim <- 10000

numU=60
uGrid <- .05+(0:numU/numU)*.2     # virtual prior tumor frequency
uPrior <- array(1/length(uGrid),length(uGrid)) # This isn't strictly necessary (why?)
vGrid <- 1:60                 # virtual prior count
vPrior <- dgamma(vGrid,shape=1,scale=20)
vPrior <- vPrior/sum(vPrior) # This isn't strictly necessary (why?)

uMC <- sample(uGrid,1,prob=uPrior)  # Initialize virtual frequency
vMC <- sample(vGrid,1,prob=vPrior)  # Initialize virtual count
q <- array(0, dim=c(numSim,nS))   # Allocate space for simulated tumor probs 
q[1,] <- rbeta(nS,uMC*vMC,(1-uMC)*vMC)  # Initialize tumor probs

for (k in 2:numSim) {
	alpha1 <- uMC[k-1]*vMC[k-1] + nt           # posterior alpha
	beta1 <- (1-uMC[k-1])*vMC[k-1] + nr - nt   # posterior beta
	q[k,] <- rbeta(nS,alpha1,beta1)            # simulate new tumor probs
	uLik <- 1   # Initialize u likelihood
	for (j in 1:nS) {
		uLik <- uLik*dbeta(q[k,j],uGrid*vMC[k-1],(1-uGrid)*vMC[k-1])
		}
	uPost <- uPrior*uLik/sum(uPrior*uLik)
	uMC[k] <- sample(uGrid,1,prob=uPost)
	vLik <- 1    # Initialize v likelihood
	for (j in 1:nS) {
		vLik <- vLik*dbeta(q[k,j],uMC[k]*vGrid,(1-uMC[k])*vGrid)
		}
	vPost <- vPrior*vLik/sum(vPrior*vLik)
	vMC[k] <- sum(rmultinom(1,1,vPost)*vGrid)  # choose value of V
	}
	

# Histogram of Posterior Distribution for Top-Level Hyperparameters
require(lattice)    # Needed for histogram() function
histogram(uMC,breaks=do.breaks(c(0.05,0.25),length(uGrid)),xlim=c(0.08,0.21),xlab="Expected Population Tumor Probability",ylab="Posterior Probability")
histogram(vMC,breaks=do.breaks(c(1,60),length(vGrid)/2),xlab="Expected Virtual Prior Sample Size",ylab="Posterior Probability")

# MCMC Diagnostics 
effectiveSize(uMC)
effectiveSize(vMC)
effectiveSize(q)

acf(uMC)
acf(vMC)

# Approximate standard errors for posterior means of uMC and vMC
uSD <- sd(uMC)/sqrt(effectiveSize(uMC))
vSD <- sd(vMC)/sqrt(effectiveSize(vMC))

# Traceplots
plot(1:length(uMC),uMC,main="",xlab="Iteration",ylab="Expected Population Tumor Probability")
plot(1:length(vMC),vMC,main="",xlab="Iteration",ylab="Virtual Prior Sample Size")
	
	
# Kernel density plots
plot(density(uMC),main="",xlab="Expected Population Tumor Probability",ylab="Probability Density")
plot(density(vMC),main="",xlab="Expected Virtual Prior Sample Size",ylab="Probability Density")

# Plot posterior intervals 
qMean <- colSums(q)/numSim
plot(nt/nr,qMean,xlab="Tumor Frequency in Sample",ylab="MCMC Estimate and 90% Credible Interval for Tumor Probability",main="",xlim=c(0,0.3),ylim=c(0,0.35),pch=20)
lines(c(0,.32),c(0,.32))
q05 <- NULL
q95 <- NULL
for(i in 1:nS) {
	q05[i]<-quantile(q[,i],0.05)
	q95[i]<-quantile(q[,i],0.95)
	lines(c(nt[i]/nr[i],nt[i]/nr[i]),c(q05[i],q95[i]))
	}
	
# Plot amount of shrinkage against  of sample size
diff <- nt/nr - qMean
plot(nr,diff,main="",xlab="Number of Rats in Study",ylab="Amount of Shrinkage")
lines(c(0,max(nr)),c(0,0))


#Compare posterior credible intervals -- Bayesian hierarchical and independent analyses
f=nt/nr   # Frequency
u=(nt+1)/(nr+2)
plot(f,u,pch=20,col="red",ylim=c(0,.55),xlab="Sample Frequency",ylab="Tumor Probability")
lines(f[order(f)],u[order(f)],col="red",lty=1)
lines(f[order(f)],q95[order(f)],lty=3,col="red")
lines(f[order(f)],q05[order(f)],lty=3,col="red")

points(f[order(f)],qMean[order(f)],pch=20,col="blue")
lines(f[order(f)],qMean[order(f)],col="blue",lty=1)
lines(f[order(f)],q95[order(f)],lty=2,col="blue")
lines(f[order(f)],q05[order(f)],lty=2,col="blue")
legend(x=0,y=0.51,legend=c("Independent Uniform","Hierarchical Bayes"),lty=c(3,2),col=c("red","blue"))

#Compare posterior credible intervals -- Bayesian hierarchical and pooled analyses
poolM=array((sum(nt)+1)/(sum(nr)+2),nS)   # Pooled posterior mean
plot(f,poolM,pch=20,col="red",ylim=c(0,.55),xlab="Sample Frequency",ylab="Tumor Probability")
lines(f,poolM,col="red",lty=1)
qp95<-array(qbeta(0.95,sum(nt)+1,sum(nr-nt)+1),nS)
lines(f[order(f)],qp95,lty=3,col="red")
qp05<-array(qbeta(0.05,sum(nt)+1,sum(nr-nt)+1),nS)
lines(f[order(f)],qp05,lty=3,col="red")

points(f[order(f)],qMean[order(f)],pch=20,col="blue")
lines(f[order(f)],qMean[order(f)],col="blue",lty=1)
lines(f[order(f)],q95[order(f)],lty=2,col="blue")
lines(f[order(f)],q05[order(f)],lty=2,col="blue")
legend(x=0,y=0.51,legend=c("Independent Uniform","Hierarchical Bayes"),lty=c(3,2),col=c("red","blue"))

# Posterior predictive distribution
# Using our posterior distribution for U and V we will generate
# a sample of random studies and compare tumor probabilities

alpha <- uMC*vMC
beta <- vMC-alpha
ppSample <- array(0,c(numSim,nS))
for (i in 1:nS) {
	qpp <- rbeta(numSim,alpha,beta)    # generate tumor probabilities
	ppSample[,i] <- rbinom(numSim,nr[i],qpp) # generate number of tumors
	}

# Plot posterior predictive intervals for study tumor counts
ppMean <- colSums(ppSample)/numSim
plot(1:nS,ppMean,xlab="Observed Tumor Count",ylab="Posterior Predictive Tumor Probability",main="",ylim=c(0,20),pch=20)
points(1:nS,nt,pch="X")
qpp05 <- NULL
qpp95 <- NULL
for(i in 1:nS) {
	qpp05[i]<-quantile(ppSample[,i],0.05)
	qpp95[i]<-quantile(ppSample[,i],0.95)
	lines(c(i,i),c(qpp05[i],qpp95[i]))
	}


# Barplot of sample counts and posterior predictive expected counts
ppCounts <- hist(c(ppSample),breaks=(0:42-0.5))$counts/numSim
obsCounts <- hist(c(nt),breaks=(0:42-0.5))$counts
barplot(rbind(ppCounts[1:17],obsCounts[1:17]),beside=TRUE,col=c("lightblue","pink"),names=0:16,border=c("darkblue","red"),legend=c("Posterior Predictive","Observed"))

barplot(ppCounts,obsCounts,beside=TRUE,col=c("lightblue","pink"),border=c("darkblue","red"),legend=c("Posterior Predictive","Observed"))
# Frequency counts for number of tumors
ppCounts <- array(0,c(numSim,17))
obsCounts <- array(0,17)
for (j in 1:17) {
	obsCounts[j]=length(subset(nt,nt==j-1))
	for (i in 1:numSim) {
		ppCounts[i,j] <- length(subset(ppSample[i,],ppSample[i,]==j-1))
		}
	}
	
