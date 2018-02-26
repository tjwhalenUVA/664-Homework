#A----
#Prior
p2.prior.shape <- 1
p2.prior.scale <- 10000
#Data
p2.car.df <-
    data.frame('timeInterval' = seq(1, 10, by = 1), 
               'carCount' = c(2, 2, 1, 1, 0, 4, 3, 0, 2, 1))

p2.occurences <-
    p2.car.df %>%
    group_by(carCount) %>%
    summarise(Occurences = n())
#alpha
p2.a.shape <- p2.prior.shape + sum(p2.occurences$carCount * p2.occurences$Occurences)
#beta
p2.a.scale<- p2.prior.scale / (1 + sum(p2.occurences$Occurences) * p2.prior.scale)
#Posterior
p2.lambda <- seq(0, 4, by=.001)
p2.dist.df <-
    data.frame('lambda' = p2.lambda, 
               'Posterior' = dgamma(p2.lambda, 
                                    shape = p2.a.shape, 
                                    scale = p2.a.scale))

p2.a.lower <- qgamma(.05, shape = p2.a.shape, scale = p2.a.scale)
p2.a.upper <- qgamma(.95, shape = p2.a.shape, scale = p2.a.scale)

#Plot
p2.a.line.size <- 1
p2.a.posterior <-
    p2.dist.df %>%
    ggplot() +
    geom_vline(aes(color = '90% CI', 
                   xintercept = p2.a.lower), 
               linetype = dash.line, 
               size = p2.a.line.size) +
    geom_vline(aes(color = '90% CI', 
                   xintercept = p2.a.upper), 
               linetype = dash.line, 
               size = p2.a.line.size) +
    geom_line(aes(x = lambda, 
                  y = Posterior, 
                  color = 'Posterior'), 
              stat = 'identity', 
              size = p2.a.line.size) +
    theme_dark() +
    labs(title = 'Posterior Conditional on First 10 Observations')

#B----
p2.size.b <- p2.a.shape
p2.prob.b <- 1/(1 + 11 * p2.a.scale)

p2.b.df <- 
    data.frame('cars' = seq(0, 40, by=1)) %>%
    mutate(Prediction = dnbinom(cars, 
                                size = p2.size.b, 
                                prob = p2.prob.b))

p2.b.pred.plot <-
    p2.b.df %>%
    ggplot() +
    geom_bar(aes(x = cars, 
                 y = Prediction), 
             stat = 'identity', 
             fill = 'lightblue') +
    theme_dark() +
    labs(title = 'Predictive Distribution\nPeriods 11 - 23', 
         x = '# Of Cars')

#C----
# p2.c.postProb <- pnbinom(30, size = p2.size.b, prob = p2.prob.b) - pnbinom(10, size = p2.size.b, prob = p2.prob.b)

p2.c.postProb <- 
    p2.b.df %>%
    filter(cars >= 10, 
           cars <= 30) %>%
    summarise(pred = sum(Prediction)) %>%
    .$pred

#D----
p2.d.pointEst <- 11 * p2.a.shape * p2.a.scale
p2.cars <- seq(0, 40, by=1)
p2.d.dist.df <-
    data.frame('cars' = p2.cars, 
               'Posterior' = dpois(p2.cars, p2.d.pointEst))

p2.d.pred.plot <-
    p2.d.dist.df %>%
    ggplot(aes(x=cars, 
               y=Posterior)) +
    geom_bar(stat = 'identity', 
             fill = 'orange') +
    theme_dark() +
    labs(title = 'Predictive Distribution\nPeriods 24 - 35', 
         x = '# Of Cars')


# p2.d.postProb <- ppois(30, p2.d.pointEst) - ppois(10, p2.d.pointEst)
p2.d.postProb <- 
    p2.d.dist.df %>%
    filter(cars >= 10, 
           cars <= 30) %>%
    summarise(post = sum(Posterior)) %>%
    .$post

#E----
p2.e.pred.plot <-
    data.frame('cars' = seq(0, 40, by=1)) %>%
    mutate('Part C' = dnbinom(cars, size = p2.size.b, prob = p2.prob.b), 
           'Part D' = dpois(cars, p2.d.pointEst)) %>%
    gather(Problem, Density, -cars) %>%
    ggplot() +
    geom_bar(aes(x = cars, 
                 y = Density, 
                 fill = Problem), 
             stat = 'identity', 
             position = 'dodge') +
    scale_fill_manual(values = c('lightblue', 'orange')) +
    theme_dark() +
    labs(title = 'Predictive Distributions from Part C & D', 
         x = '# Of Cars')

p2.e.probs <- 
    data.frame('Periods' = c('Periods 11-23', 'Periods 24-35'), 
               'Probability' = c(p2.c.postProb, p2.d.postProb))


