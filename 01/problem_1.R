#Problem 1A====
sensitivity.1a <- .8
specificity.1a <- .85

p.prior.1a <- 1/1000

# What is the posterior probability that an individual whose polygraph 
# report indicates a concern has committed a security violation?
numerator.1a <- p.prior.1a * sensitivity.1a
denominator.1a <- p.prior.1a * sensitivity.1a + (1 - p.prior.1a) * (1-specificity.1a)

postProb.1a <- numerator.1a / denominator.1a

# Comment on the implications of these results for the use of routine screening 
# polygraphs for individuals working in positions requiring security clearances.





#Problem 1B====
sensitivity.1b <- sensitivity.1a
specificity.1b <- specificity.1a
# prior probability of 25% that the individual stole the item
p.prior.1b <- .25

# What is the posterior probability that this individual committed the theft?
numerator.1b <- p.prior.1b * sensitivity.1b
denominator.1b <- p.prior.1b * sensitivity.1b + (1 - p.prior.1b) * (1-specificity.1b)

postProb.1b <- numerator.1b / denominator.1b

# Explain the difference between this result and Part a.
