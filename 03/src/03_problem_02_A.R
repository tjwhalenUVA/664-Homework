#The inter arrival time is the time between each arrival into the system and the next.
interArrival <- c(12, 2, 6, 2, 19, 5, 34, 4, 1, 4, 
                  8, 7, 1, 21, 6, 11, 8, 28, 6, 4, 
                  5, 1, 18, 9, 5, 1, 21, 1, 1, 5, 
                  3, 14, 5, 3, 4, 5, 1, 3, 16, 2)

#A====
#A common model for interarrival times is a random sample from an exponential distribution. 
#Do you think an exponential distribution provides a good model for the interarrival times? 
#Justify your answer.

exp.quant <- qexp(ppoints(length(interArrival)))
# qqplot(exp.quant, interArrival,
# main='Exponential Q-Q Plot of Car Arrival Times',
# xlab='Theoretical Exponential Quantiles',ylab='Empirical Quantiles')
# lines(exp.quant, exp.quant *mean(interArrival))

#Get a description of possible distributions
#exp.desc <- descdist(interArrival, discrete=FALSE, boot=500)



#Based on graph try exponential and gamma distributions
fit.exp <- fitdist(interArrival, "exp")
# exp.plot <- plot(fit.exp)
fit.exp$aic

fit.gam <- fitdist(interArrival, "gamma")
# gam.plot <- plot(fit.gam)
fit.gam$aic

#Find sample parameters, sample new data from dist, run test

#Exponential
fitExp <- fitdistr(interArrival, 'exponential')
ks.test(interArrival, "pexp", fitExp$estimate)

ex <- rexp(length(interArrival), rate = fitExp$estimate)
t.test(interArrival, ex)

#Gamma
fitGam <- fitdistr(interArrival, 'gamma')
ks.test(interArrival, "pgamma", fitGam$estimate)

gam <- rgamma(length(interArrival),
              shape = fitGam$estimate[1], rate = fitGam$estimate[2])
t.test(interArrival, gam)




