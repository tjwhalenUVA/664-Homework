# This code produces the triplots from the Unit 3 notes

alpha0 = 1.4
beta0 = 7.2

# The data
numTumor = 0    # Set this to the number of tumors
numRats = 20    # Set this to the number of rats

alpha1 = alpha0 + numTumor
beta1 = beta0 + numRats - numTumor


# Continuous version -- page 16 of Unit 3
theta=seq(length=400,from=0.001,to=0.99)
priorDens=dbeta(theta,shape1=alpha0,shape2=beta0)  # Prior
postDens=dbeta(theta,shape1=alpha1,shape2=beta1)  # Posterior
normLikC=dbeta(theta,shape1=numTumor+1,shape2=numRats+1-numTumor)    # Normalized Likelihood 

plot(theta,priorDens,type="l",col="blue",
     main=paste("Triplot for Tumor Probability (",numTumor,"Tumors in",numRats,"Rats)"),
     xlab="Tumor Probability",ylab="Probability Density",
     xlim=c(0,1),ylim=c(0,15))
lines(theta,normLikC,col="green")
lines(theta,postDens,col="red")
legend(0.7,12.0,c("Prior","Norm Lik","Posterior"),col=c("blue","green","red"),
       lty=c(1,1,1))

cat("Posterior mean",round(alpha1/(alpha1+beta1),4))
cat("Posterior standard deviation",
    round(sqrt(alpha1*beta1/((alpha1+beta1)^2*(alpha1+beta1+1))),4))
cat("Posterior mode",round((alpha1-1)/(alpha1+beta1-2),4))
cat("Posterior median",round(qbeta(0.5,alpha1,beta1),4))
cat("Posterior 90th percentile",round(qbeta(0.9,alpha1,beta1),4))

    