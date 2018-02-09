#Problem 3====
df.p3 <- NULL

theta <- seq(from = 0.025, to = 0.975, length.out = 20)

priorDist <- array(1/20, 20)

numTook <- 15
numConcern <- 12

postDist <- priorDist * theta^numConcern*(1-theta)^(numTook-numConcern)

postDist <- postDist/sum(postDist)

df.p3$theta <- theta
df.p3$priorDist <- priorDist
df.p3$postDist <- postDist
df.p3 <- as.data.frame(df.p3)

p3.plot <-
    df.p3 %>%
    ggplot(aes(x=theta, y=postDist)) +
    geom_bar(stat = 'identity', fill='lightblue') +
    theme_bw() +
    labs(x='Q', 
         y='Posterior Probability')
