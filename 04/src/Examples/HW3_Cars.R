
# SYST 664 / CSI 674 Spring 2018
# Assignment 3 Problem 2
# Analysis of automobile arrivals

# Set up the data
carIntervals=c(12, 2, 6, 2, 19, 5, 34, 4, 1, 4, 8, 7, 1, 21, 
               6, 11, 8, 28, 6, 4, 5, 1, 18, 9, 5, 1, 21, 1, 
               1, 5, 3, 14, 5, 3, 4, 5, 1, 3, 16, 2)
summary(carIntervals)            # Summary statistics for car interrival times 

# Evaluate fit of exponential distribution using a Q-Q plot
exp.quantiles = qexp(ppoints(length(carIntervals)))
qqplot(exp.quantiles,carIntervals,
       main="Exponential Q-Q Plot of Car Interarrival Times",
       xlab="Exponential Quantiles",ylab="Empirical Quantiles")
lines(exp.quantiles,exp.quantiles*mean(carIntervals))  # Overlay a line

# Goodness of fit based on exponential distribution
qq=c(0:8)/8                            # exponential quantiles
br=qexp(qq,rate=1/mean(carIntervals))  # bin boundaries
oo=hist(carIntervals,br,plot=F)$counts # counts in the bins

# With 8 bins we use chisquare test with degrees of freedom 6
# 8 equally likely bins means expected count of 5 in each bin
sum((oo-5)^2/5)       # chisquare test statistic
qchisq(0.95,6)        # critical value for test statistic


# Compare empirical and theoretical distributions for arrivals by 15-second interval
carTimes = cumsum(carIntervals)           # Cumulative sums
Which15Sec <- ceiling(carTimes/15)        # Minute during which each car passed
CarsBy15Sec <- tabulate(Which15Sec)       # Count number of cars in each 15 second interval
ObservedCounts <- table(CarsBy15Sec)      # Counts of intervals with 0, 1, 2 etc. cars

ObservedCounts[6]=0                       # No intervals with more than 5 cars

# Sample mean is total number of events divided by total number of time periods
# Here we assume 40 cars in 21 15-second intervals (ignoring the fact that the
# last interval is slightly shorter than 15 seconds) 
ArrivalRate=sum(CarsBy15Sec)/length(CarsBy15Sec)  # Estimate Poisson rate by 40 cars in 21 intervals
ExpectedCounts=dpois(0:4,ArrivalRate)*length(CarsBy15Sec)    # Expected counts for Poisson distribution
ExpectedCounts[6]=(1-ppois(4,ArrivalRate))*length(CarsBy15Sec)

# Plot the data
ObsExp <- rbind(ObservedCounts,ExpectedCounts) # bind into matrix for plotting
barplot(ObsExp,main="Distribution of Arrivals per 15-second Block", 
        xlab="Number of Cars (in 15-second Block)", 
        ylab="Empirical Count (of 15-second Blocks)", col=c("lightblue","pink"), 
        border=c("darkblue","red"),names=c(0:4,"5+"), beside=TRUE,
        legend=c("Observed","Expected"))


# Chi-squared test
oc = c(ObservedCounts[1:4],sum(ObservedCounts[5:6]))
ec = c(ExpectedCounts[1:4],sum(ExpectedCounts[5:6]))

# With 5 bins we use chisquare test with degrees of freedom 3
sum((oc-ec)^2/ec)             # Test statistic
qchisq(0.95,3)                # 95th percentile of chisq distribution


# Find the posterior distribution
lambda=seq(from=0.2,to=4,by=0.2)
priorDist = array(1,length(lambda))/length(lambda)
lik = array(1,length(lambda))
for (i in 1:length(CarsBy15Sec)) {
  lik = lik*dpois(CarsBy15Sec[i],lambda)
}
postDist=priorDist*lik/sum(priorDist*lik)
barplot(postDist,main=expression("Posterior Distribution for"~Lambda),
        col=c("mediumpurple"), border=c("purple4"),
        xlab=expression(lambda), ylab="Probability",
        names=lambda)

# Posterior mean and standard deviation
postMean = sum(lambda*postDist)
postVar = sum(((lambda-postMean)^2)*postDist)

# Quantiles of posterior distribution
# Quantile q is smallest value of lambda for which the posterior cdf is >= q
# To find quantile q: first we count how many values of posterior cdf are less than q,
# then we add 1 to find the index of the first value that is greater than or equal to q, 
# then we find the value of lambda corresponding to this index
median = lambda[sum(cumsum(postDist)<0.5)+1]   # Median
q025=lambda[sum(cumsum(postDist)<0.025)+1]     # 0.025 quantile
q975=lambda[sum(cumsum(postDist)<0.975)+1]     # 0.975 quantile

mode = lambda[which(postDist==max(postDist))]
