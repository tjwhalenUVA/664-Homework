# Inverse Gamma triplot for Baby Births example in Unit 3

#### Installing invgamma package ####
# Run the commented line of code below to install the invgamma package
# After being run once it does not need to be run again until you re-install R
# install.packages("invgamma")

# Load MCMCpack and the packages it depends on
library('invgamma')

# Prior distribution - Mean time between birhts has inverse-Gamma(alpha,beta) 
# distribuition and birth rate has Gamma(alpha, beta distribution)
alpha0 = 2        # Prior shape
beta0 = 0.75      # Prior inverse-scale (scale of the Gamma distributionf for birth rate)

n = 44            # number of births
sum.ibt = 23.92   # sum of inter-birth times

alpha1 = 2 + 44   # posterior shape
beta1 = (1/beta0 + sum.ibt)^-1

thetavals=seq(length=100,from=0.1,to=1)
prior.dens=dinvgamma(thetavals,alpha0,scale=beta0)
norm.lik=dinvgamma(thetavals,n-1,scale=1/sum.ibt)
post.dens=dinvgamma(thetavals,alpha1,scale=beta1)
plot(thetavals,prior.dens,type="l",col="blue",main="Triplot for Mean Time Between Births",
     xlab="Mean Time Between Births",ylab="Probability Density",
     xlim=c(0,1),ylim=c(0,6))
lines(thetavals,norm.lik,col="green")
lines(thetavals,post.dens,col="red")
legend(0.01,5.9,c("Prior","Norm Lik","Posterior"),col=c("blue","green","red"),lty=c(1,1,1))
