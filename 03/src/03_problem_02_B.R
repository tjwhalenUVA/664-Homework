#B====
# When interarrival times are randomly sampled from an exponential distribution, 
# the counts of events per unit time are a random sample from a Poisson distribution.
# Using a time unit of 15 seconds, find the number of cars passing in each 15-second block of time 
# after the initial car. (The initial car is used to bound the recording interval,
#                         so the total car count in your data set should be 40.)  
# Do you think a Poisson distribution provides a good model for the count data? 
# Justify your answer.
car.df <-
    data.frame('iat' = interArrival) %>%
    mutate(actualT = cumsum(iat), 
           tmp=actualT/15, 
           group = ceiling(tmp))

# Compare empirical and theoretical distributions for hourly births
Which15Sec <- car.df$group        # Hour in which each birth occurred
CarsBy15Sec <- tabulate(Which15Sec)       # Count number of births in each hour (1 to 24)
ObservedCounts <- table(CarsBy15Sec)     # Number of hours with 0, 1, 2, 3, 4 births

CarRate=sum(CarsBy15Sec)/21            # Estimate Poisson rate by 44 births in 24 hours
ExpectedCounts=dpois(0:4,CarRate)*21    # Expected counts for Poisson distribution

# Plot the data
# p2.b.plot <-
#     data.frame('Cars' = names(ObservedCounts), 
#                'Observed' = as.vector(ObservedCounts), 
#                'Expected' = ExpectedCounts) %>%
#     gather(Sample, Values, -Cars) %>%
#     ggplot(aes(x=Cars, 
#                y=Values, 
#                fill=Sample)) +
#     geom_bar(stat = 'identity', 
#              position = 'dodge') +
#     labs(title='Distribution of Cars per 15 Second Block', 
#          x='# of Cars Passing Point (in 15 Second Blocks)', 
#          y='Empirical Count (of 15 Second Blocks)') +
#     theme_dark()


fit.pois <- fitdist(CarsBy15Sec, 'pois', discrete = TRUE)
fit.pois$aic

fit.norm <- fitdist(CarsBy15Sec, 'norm', discrete = TRUE)
# plot(fit.nbinom)
fit.norm$aic


#descdist(CarsBy15Sec, discrete = TRUE, boot = 500)
