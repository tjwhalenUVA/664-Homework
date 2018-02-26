#Part A----
p1.a.alpha.0 <- 1
p1.a.beta.0 <- 3
#Stats
p1.a.df <-
    data.frame('Parameters' = paste('alpha:',
                                    p1.a.alpha.0,
                                    ', beta:', 
                                    p1.a.beta.0, 
                                    sep = ' ')) %>%
    mutate(Mean = p1.a.alpha.0 / (p1.a.alpha.0 + p1.a.beta.0), 
           StDev = sqrt((p1.a.alpha.0 * p1.a.beta.0) / ((p1.a.alpha.0 + p1.a.beta.0)^2 * (p1.a.alpha.0 + p1.a.beta.0 + 1))), 
           Median = qbeta(0.5, shape1 = p1.a.alpha.0, shape2 = p1.a.beta.0), 
           Q.025 = qbeta(0.025, shape1 = p1.a.alpha.0, shape2 = p1.a.beta.0), 
           Q.975 = qbeta(0.975, shape1 = p1.a.alpha.0, shape2 = p1.a.beta.0))
#Plot
dash.line <- 'dashed'
p1.a.line.size <- 1
p1.a.plot <-
    data.frame('pi' = seq(0, 1, length.out = 5000)) %>%
    mutate(Prior = dbeta(pi, 
                         shape1 = p1.a.alpha.0, 
                         shape2 = p1.a.beta.0)) %>%
    ggplot() +
    geom_vline(aes(xintercept = p1.a.df$Mean, 
                   color = 'Mean'), 
               size = p1.a.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.a.df$Mean + p1.a.df$StDev, 
                   color = 'StDev'), 
               size = p1.a.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.a.df$Mean - p1.a.df$StDev, 
                   color = 'StDev'), 
               size = p1.a.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.a.df$Q.025, 
                   color = '95% CI'), 
               size = p1.a.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.a.df$Q.975, 
                   color = '95% CI'), 
               size = p1.a.line.size, 
               linetype = dash.line) +
    geom_line(aes(x=pi, 
                  y=Prior), 
              stat = 'identity', 
              color = 'lightblue', 
              size = p1.a.line.size) +
    labs(title = 'Prior Distribution for Choosing B then C', 
         y='Density', 
         x = 'Probability of Chosing B and C', 
         color = 'Statistics') +
    theme_dark()






#Part B----
p1.b.Chose.BandC <- 19
p1.b.Chose.Total <- 47
p1.b.alpha.0 <- p1.a.alpha.0 + p1.b.Chose.BandC
p1.b.beta.0 <- p1.a.beta.0 + p1.b.Chose.Total - p1.b.Chose.BandC
#Stats
p1.b.df <-
    data.frame('Parameters' = paste('alpha:',
                                    p1.b.alpha.0,
                                    ', beta:', 
                                    p1.b.beta.0, 
                                    sep = ' ')) %>%
    mutate(Mean = p1.b.alpha.0 / (p1.b.alpha.0 + p1.b.beta.0), 
           StDev = sqrt((p1.b.alpha.0 * p1.b.beta.0) / ((p1.b.alpha.0 + p1.b.beta.0)^2 * (p1.b.alpha.0 + p1.b.beta.0 + 1))), 
           Median = qbeta(0.5, shape1 = p1.b.alpha.0, shape2 = p1.b.beta.0), 
           Q.025 = qbeta(0.025, shape1 = p1.b.alpha.0, shape2 = p1.b.beta.0), 
           Q.975 = qbeta(0.975, shape1 = p1.b.alpha.0, shape2 = p1.b.beta.0))
#Plot
p1.b.line.size <- 1
p1.b.plot.stats <-
    data.frame('pi' = seq(0, 1, length.out = 5000)) %>%
    mutate(Posterior = dbeta(pi, 
                         shape1 = p1.b.alpha.0, 
                         shape2 = p1.b.beta.0)) %>%
    ggplot() +
    geom_vline(aes(xintercept = p1.b.df$Mean, 
                   color = 'Mean'), 
               size = p1.b.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.b.df$Mean + p1.b.df$StDev, 
                   color = 'StDev'), 
               size = p1.b.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.b.df$Mean - p1.b.df$StDev, 
                   color = 'StDev'), 
               size = p1.b.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.b.df$Q.025, 
                   color = '95% CI'), 
               size = p1.b.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.b.df$Q.975, 
                   color = '95% CI'), 
               size = p1.b.line.size, 
               linetype = dash.line) +
    geom_line(aes(x=pi, 
                  y=Posterior), 
              stat = 'identity', 
              color = 'lightblue', 
              size = p1.b.line.size) +
    labs(title = '2009: Posterior Distribution for Choosing B then C', 
         y='Density', 
         x = 'Probability of Chosing B and C', 
         color = 'Statistics') +
    theme_dark()

#Tripplot
p1.b.plot.tri <-
    data.frame('pi' = seq(0, 1, length.out = 5000)) %>%
    mutate(Posterior = dbeta(pi, 
                             shape1 = p1.b.alpha.0, 
                             shape2 = p1.b.beta.0), 
           Prior = dbeta(pi, 
                         shape1 = p1.a.alpha.0, 
                         shape2 = p1.a.beta.0), 
           NormLik = dbeta(pi, 
                           shape1 = p1.b.Chose.BandC + 1, 
                           shape2 = p1.b.Chose.Total - p1.b.Chose.BandC + 1)) %>%
    gather(Distribution, Density, -pi) %>%
    ggplot() +
    geom_line(aes(x=pi, 
                  y=Density, 
                  color = Distribution), 
              stat = 'identity', 
              size = p1.b.line.size) +
    labs(title = '2009: Triplot Distribution for Choosing B then C', 
         x = 'Probability of Chosing B and C') +
    theme_dark()


#Part C----
p1.c.Chose.BandC <- 20
p1.c.Chose.Total <- 47
p1.c.alpha.0 <- p1.b.alpha.0 + p1.c.Chose.BandC
p1.c.beta.0 <- p1.b.beta.0 + p1.c.Chose.Total - p1.c.Chose.BandC
#Stats
p1.c.df <-
    data.frame('Parameters' = paste('alpha:',
                                    p1.c.alpha.0,
                                    ', beta:', 
                                    p1.c.beta.0, 
                                    sep = ' ')) %>%
    mutate(Mean = p1.c.alpha.0 / (p1.c.alpha.0 + p1.c.beta.0), 
           StDev = sqrt((p1.c.alpha.0 * p1.c.beta.0) / ((p1.c.alpha.0 + p1.c.beta.0)^2 * (p1.c.alpha.0 + p1.c.beta.0 + 1))), 
           Median = qbeta(0.5, shape1 = p1.c.alpha.0, shape2 = p1.c.beta.0), 
           Q.025 = qbeta(0.025, shape1 = p1.c.alpha.0, shape2 = p1.c.beta.0), 
           Q.975 = qbeta(0.975, shape1 = p1.c.alpha.0, shape2 = p1.c.beta.0))
#Plot
p1.c.line.size <- 1
p1.c.plot.stats <-
    data.frame('pi' = seq(0, 1, length.out = 5000)) %>%
    mutate(Posterior = dbeta(pi, 
                             shape1 = p1.c.alpha.0, 
                             shape2 = p1.c.beta.0)) %>%
    ggplot() +
    geom_vline(aes(xintercept = p1.c.df$Mean, 
                   color = 'Mean'), 
               size = p1.c.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.c.df$Mean + p1.c.df$StDev, 
                   color = 'StDev'), 
               size = p1.c.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.c.df$Mean - p1.c.df$StDev, 
                   color = 'StDev'), 
               size = p1.c.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.c.df$Q.025, 
                   color = '95% CI'), 
               size = p1.c.line.size, 
               linetype = dash.line) +
    geom_vline(aes(xintercept = p1.c.df$Q.975, 
                   color = '95% CI'), 
               size = p1.c.line.size, 
               linetype = dash.line) +
    geom_line(aes(x=pi, 
                  y=Posterior), 
              stat = 'identity', 
              color = 'lightblue', 
              size = p1.c.line.size) +
    labs(title = '2011: Posterior Distribution for Choosing B then C', 
         y='Density', 
         x = 'Probability of Chosing B and C', 
         color = 'Statistics') +
    theme_dark()

#Tripplot
p1.c.plot.tri <-
    data.frame('pi' = seq(0, 1, length.out = 5000)) %>%
    mutate(Posterior = dbeta(pi, 
                             shape1 = p1.c.alpha.0, 
                             shape2 = p1.c.beta.0), 
           Prior = dbeta(pi, 
                         shape1 = p1.b.alpha.0, 
                         shape2 = p1.b.beta.0), 
           NormLik = dbeta(pi, 
                           shape1 = p1.c.Chose.BandC + 1, 
                           shape2 = p1.c.Chose.Total - p1.c.Chose.BandC + 1)) %>%
    gather(Distribution, Density, -pi) %>%
    ggplot() +
    geom_line(aes(x=pi, 
                  y=Density, 
                  color = Distribution), 
              stat = 'identity', 
              size = p1.c.line.size) +
    labs(title = '2011: Triplot Distribution for Choosing B then C', 
         x = 'Probability of Chosing B and C') +
    theme_dark()


#D----
p1.final.df <-
    p1.a.df %>%
    bind_rows(., 
              p1.b.df) %>%
    bind_rows(., 
              p1.c.df)

p1.d.line.size <- 1

p1.d.plot <-
    data.frame('pi' = seq(0, 1, length.out = 5000)) %>%
    mutate(Prior.A = dbeta(pi, 
                           shape1 = p1.a.alpha.0, 
                           shape2 = p1.a.beta.0), 
           Posterior.2009 = dbeta(pi, 
                               shape1 = p1.b.alpha.0, 
                               shape2 = p1.b.beta.0), 
           Posterior.2011 = dbeta(pi, 
                               shape1 = p1.c.alpha.0, 
                               shape2 = p1.c.beta.0)) %>%
    gather(Distribution, Density, -pi) %>%
    ggplot() +
    geom_line(aes(x=pi, 
                  y=Density, 
                  color = Distribution), 
              stat = 'identity', 
              size = p1.d.line.size) +
    labs(title = 'Comparing Distributions from Problem 1', 
         x = 'Probability of Chosing B and C') +
    theme_dark()
