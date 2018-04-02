#URL of data given in assignment
www <- "http://www.biostat.umn.edu/~lynn/iid/wolf.river.dat"

#Read in the data
pollutants <- read.table(www, header=TRUE, stringsAsFactors = F)

#Filter down to data this study is concerned with
pollutants %<>%
    mutate(id = rep(seq(1, 10, by = 1), 3)) %>%
    select(-Aldrin) %>%
    spread(Depth, HCB) %>%
    select(-id, -Middepth)

#mu
prior.center <- 0
#k
prior.precision <- 0
#alpha
prior.shape <- -1/2
#beta
prior.scale <- 'infinity'


#Number of Observations
n <- nrow(pollutants)

#Bottom ====
#Sample Mean
bottom.mean <- sum(pollutants$Bottom) / n
#Sum of Squared Deviations
bottom.ssd <- sum((pollutants$Bottom - bottom.mean) ^ 2)
#Center
bottom.center <- ((prior.precision * prior.center) + (n * bottom.mean)) / (prior.precision + n)
#Precision
bottom.precision <- prior.precision + n
#shape
bottom.shape <- prior.shape + 1/2 * n
#scale
bottom.scale <- (1/2 * bottom.ssd + (prior.precision * n * (bottom.mean - prior.center) ^ 2) / (2 * (prior.precision + n))) ^ -1
#Spread
bottom.spread <- 1 / sqrt(bottom.precision * bottom.scale * bottom.shape)
#Degrees of Freedom
bottom.dof <- 2 * bottom.shape

#95% Credible Interval
#theta
bottom.t.975 <- qt(0.975, bottom.dof)
bottom.theta.025 <- bottom.center - bottom.t.975 * bottom.spread
bottom.theta.975 <- bottom.center + bottom.t.975 * bottom.spread
bottom.theta.ci <- sprintf('[%s, %s]',
                            round(bottom.theta.025, 2), 
                            round(bottom.theta.975, 2))
#P
bottom.P.025 <- qgamma(.025, shape = bottom.shape, scale = bottom.scale)
bottom.P.975 <- qgamma(.975, shape = bottom.shape, scale = bottom.scale)
bottom.P.ci <- sprintf('[%s, %s]',
                        round(bottom.P.025, 2), 
                        round(bottom.P.975, 2))

#Density
bottom.density <-
    data.frame('Mean' = seq(-10, 10, length.out = 200)) %>%
    mutate('Density' = dt(Mean, df = bottom.dof)/bottom.spread,
           Depth = 'Bottom')



#Surface ====
#Sample Mean
surface.mean <- sum(pollutants$Surface) / n
#Sum of Squared Deviations
surface.ssd <- sum((pollutants$Surface - surface.mean) ^ 2)
#Center
surface.center <- ((prior.precision * prior.center) + (n * surface.mean)) / (prior.precision + n)
#Precision
surface.precision <- prior.precision + n
#shape
surface.shape <- prior.shape + 1/2 * n
#scale
surface.scale <- (1/2 * surface.ssd + (prior.precision * n * (surface.mean - prior.center) ^ 2) / (2 * (prior.precision + n))) ^ -1
#Spread
surface.spread <- 1 / sqrt(surface.precision * surface.scale * surface.shape)
#Degrees of Freedom
surface.dof <- 2 * surface.shape

#95% Credible Interval
#theta
surface.t.975 <- qt(0.975, surface.dof)
surface.theta.025 <- surface.center - surface.t.975 * surface.spread
surface.theta.975 <- surface.center + surface.t.975 * surface.spread
surface.theta.ci <- sprintf('[%s, %s]',
                            round(surface.theta.025, 2), 
                            round(surface.theta.975, 2))
#P
surface.P.025 <- qgamma(.025, shape = surface.shape, scale = surface.scale)
surface.P.975 <- qgamma(.975, shape = surface.shape, scale = surface.scale)
surface.P.ci <- sprintf('[%s, %s]',
                        round(surface.P.025, 2), 
                        round(surface.P.975, 2))

#Density
surface.density <-
    data.frame('Mean' = seq(-10, 10, length.out = 200)) %>%
    mutate('Density' = dt(Mean, df = surface.dof)/surface.spread,
           Depth = 'Bottom')



#Compare
bind_rows(bottom.density, surface.density) %>%
    ggplot(aes(x = Mean,
               y = Density, 
               color = Depth)) +
    geom_line(stat = 'identity') +
    labs(title = 'Joint Posterior Distributions') +
    theme_classic()

data.frame('Parameter' = c('Theta', 'P'),
           'Surface' = c(surface.theta.ci, surface.P.ci),
           'Bottom' = c(bottom.theta.ci, bottom.P.ci))
