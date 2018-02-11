# Un-comment the following line if you have not already installed the UsingR package
# install.packages("UsingR")   # Install UsingR add-on package (contains babyboom data)
# Documentation for UnsinR can be found at 

# Set up the data
require(UsingR)              # Load the UsingR add-on package
summary(babyboom)            # Summary statistics for babyboom data
birthweight=babyboom$wt      # baby weights
sd(birthweight)              # Standard deviation for birth weights
mad(birthweight)             # Median absolute deviation for birth weights


# Compare empirical and theoretical distributions for hourly births
birthtime=babyboom[,4]                    # Time of birth in minutes after midnight (44 observations)
WhichHour <- ceiling(birthtime/60)        # Hour in which each birth occurred
BirthsByHour <- tabulate(WhichHour)       # Count number of births in each hour (1 to 24)
ObservedCounts <- table(BirthsByHour)     # Number of hours with 0, 1, 2, 3, 4 births

BirthRate=sum(BirthsByHour)/24            # Estimate Poisson rate by 44 births in 24 hours
ExpectedCounts=dpois(0:4,BirthRate)*24    # Expected counts for Poisson distribution

# Plot the data
ObsExp <- rbind(ObservedCounts,ExpectedCounts) # bind into matrix for plotting
barplot(ObsExp,main="Distribution of Births per One-Hour Block", 
        xlab="Number of Babies Born (in One-Hour Block)", 
        ylab="Empirical Count (of One-Hour Blocks)", col=c("lightblue","pink"), 
        border=c("darkblue","red"),names=0:4, beside=TRUE,
        legend=c("Observed","Expected"))


# Exponential q-q plot of birth intervals
birthtime=babyboom[,4]           # Time of birth in minutes after midnight (44 observations)
birthinterval=diff(birthtime)    # compute birth intervals using diff function (43 observations)
exponential.quantiles = qexp(ppoints(length(birthtime)))  # quantiles of standard exponential distribution (rate=1)
qqplot(exponential.quantiles, birthinterval,main="Exponential Q-Q Plot of Birth Intervals",
       xlab = "Theoretical Exponential Quantiles", ylab = "Empirical Quantiles")
lines(exponential.quantiles,exponential.quantiles*mean(birthinterval)) # Overlay a line

