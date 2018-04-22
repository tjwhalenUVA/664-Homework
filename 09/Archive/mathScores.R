# Get school data from Hoff book web site
Y.school.mathscore<-dget("http://www.stat.washington.edu/~hoff/Book/Data/data/Y.school.mathscore")

# Data comes as a 2-D array with school ID in first column and math score in second column
# Transform into a list where each list element is the data from one school

# Data
y = list()
nSch = max(Y.school.mathscore[,1])
yBar = nStu = ySD = ySSQ = NULL
for (i in 1:nSch) {
  thisSchool = Y.school.mathscore[,1]==i      # rows for ith school 
  y[[i]] = Y.school.mathscore[thisSchool, 2]  # scores for ith school
  yBar[i] = mean(y[[i]])                      # mean for ith school
  ySSQ[i] = sum(y[[i]]^2)
  nStu[i] = length(y[[i]])                    # number of students in ith school
  ySD[i] = sd(y[[i]])
}

# Prior hyperparameters
mu.mean.0 = 50
mu.SD.0 = 25
tau.sh.0 = sig.sh.0 = 1/2
tau.sc.0 = sig.sc.0 = 1/50


## Initialize Gibbs sample variables
numSim = 5000
thetaG = matrix(nrow = numSim, ncol = nSch)
thetaG[1,] = yBar    # initial value for theta
muG = mean(yBar)    # initial value for mu
tauG = sd(yBar)     # initial value for tau
sigG = mean(ySD)    # initial value for sigma

# Draw the Gibbs samples
for (k in 2:numSim) {
  mu.mean.1 = 
    (mu.mean.0/mu.SD.0^2+sum(thetaG[k-1,])/tauG[k-1]^2)/(1/mu.SD.0^2+nSch/tauG[k-1]^2)
  mu.SD.1 = (1/mu.SD.0^2+nSch/tauG[k-1]^2)^-(1/2)
  muG[k]=rnorm(1,mu.mean.1, mu.SD.1)    # sample new mu
  
  tau.sh.1 = tau.sh.0 + 0.5*nSch  # this is the same every iteration
  tau.sc.1 = (1/tau.sc.0 + 0.5*sum((thetaG[k-1,]-muG[k])^2))^-1
  tauG[k] = rgamma(1,shape=tau.sh.1,scale=tau.sc.1)^(-1/2)
  
  sig.sh.1 = sig.sh.0 + 0.5*sum(nStu)  # this is the same every iteration
  sig.sc.1 = (1/sig.sc.0 + 0.5*sum(ySSQ - nStu*yBar^2 + nStu*(yBar - thetaG[k-1,])^2))^-1
  sigG[k] = rgamma(1,shape=sig.sh.1,scale=sig.sc.1)^(-1/2)
  
  mu.1 = (muG[k]/tauG[k]^2+yBar*nStu/sigG[k]^2)/(1/tauG[k]^2+nStu/sigG[k]^2) # vector of posterior means
  tau.1 = (1/tauG[k]^2+nStu/sigG[k]^2)^(-1/2)  # vector of posterior SDs
  thetaG[k,] = rnorm(nSch,mu.1,tau.1)
  
}

theta.hat = colMeans(thetaG)
th.lower=th.upper=NULL
for (i in 1:nSch) {
  th.lower[i] = quantile(thetaG[,i],0.025)
  th.upper[i] = quantile(thetaG[,i],0.975)
}

# Shrinkage plot for Theta
# This is the same as Figure 8.8 in Hoff except it includes 95% intervals
plot(yBar,theta.hat,main = "Math Score Means for 100 Schools",
     ylim = c(min(th.lower),max(th.upper)),
     ylab = "Mean and 95% Interval for Theta")
lines(yBar,yBar)
for (i in 1:nSch) {
  lines(c(yBar[i],yBar[i]),c(th.lower[i],th.upper[i]))
}

# Normal qq-plot for Theta
qqnorm(theta.hat)
qqline(theta.hat)

# Diagnostics - note all theta values have very high effective sample size
effectiveSize(muG)
effectiveSize(tauG)
effectiveSize(sigG)
apply(thetaG,2,effectiveSize)

# Traceplots look stationary
plot(1:numSim,muG, main="Traceplot for mean of school means")
plot(1:numSim,tauG, main="Traceplot for SD of school means")
plot(1:numSim,sigG, main="Traceplot for SD of test scores")

