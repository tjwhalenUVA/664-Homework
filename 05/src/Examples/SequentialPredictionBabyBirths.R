# This code produces the Bayesian belief dynamics example from Unit 3
# - We use the Gamma-Poisson marginal likelihood (negative binomial
#   distribution) to predict each 4-hour block of baby births. 
# - After observing the number of births we update the Gamma distribution 
# - and then predict the next block.

# This assumes the UsingR package has been installed but not loaded
# The UsingR package has the babyboom data
# To install this package type install.packages("UsingR") before running this
# You only need to install once; you won't need to install again

# Set up the data
require(UsingR)                            # Load the UsingR add-on package (has babyboom data)
birthtime=babyboom[,4]                     # Time of birth in minutes after midnight (44 observations)
WhichHour <- floor(birthtime/60)           # Hour in which each birth occurred
count4=array(0,6)                          # Count vector for six 4-hour blocks
for (i in 1:6) {
	count4[i]=sum(WhichHour < 4*i)           # How many births up to and including block i
}
count4=count4-c(0,count4[1:5])             # How many births in 4-hour block i


y=0:16                                     # Possible values for birth count in 4 hours
alpha=2                                    # Prior shape
beta=0.75                                  # Prior scale

# Process 4-hour blocks in sequence.  Compare Poisson-Gamma predictive distribution (negative 
# binomial) with Poisson distribution with same mean.  Plot these distributions on the same
# chart. Wait for input before doing next plot.
for (i in 1:6) {
	fpred=dnbinom(y,size=alpha,prob=1/(1+4*beta))   # Negative binomial predictive distribution
	fpois=dpois(y,alpha*beta*4)                     # Poisson distribution for comparison
	PredPois=rbind(fpred,fpois)                     # Bind into a matrix
	barplot(PredPois,main=paste("Predictive Distribution for Hours",i*4-3," to ",i*4), 
	xlab="Babies Born", ylab="Probability", 	col=c("lightblue","pink"), 
	border=c("darkblue","red"),names.arg=y, beside=TRUE,
	legend=c("Predictive","Poisson"),ylim=c(0,0.2))
	alpha=alpha+count4[i]                           # Update shape with number of births
	beta=1/(1/beta+4)                               # Update scale with length of time process was observed
}
