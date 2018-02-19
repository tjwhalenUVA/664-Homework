cars <- c(0, 1, 2, 3, 4, 5)
occurences <- c(3, 5, 7, 3, 3, 0)

#A====
gamma_shape <- 1
gamma_scale <- 10000
#Find alpha
sum_xi <- sum(cars * occurences)
alpha_star <- gamma_shape + sum_xi

#Find beta
N <- sum(occurences)
beta_star <- gamma_scale / (1 + N * gamma_scale)

#Plot Posterior
p1_df <- NULL
p1_df$lambda <- seq(0, 5, by=.001)
p1_df$posterior <- dgamma(p1_df$lambda, 
                        shape = alpha_star, 
                        scale = beta_star)

#Mean
p1_mean <- alpha_star * beta_star

#SD
p1_sd <- sqrt(alpha_star * beta_star ^ 2)

#Median
p1_median <- qgamma(.5, shape = alpha_star, scale = beta_star)

#Mode
p1_mode <- (alpha_star - 1 ) * beta_star

#Conf Interval
p1_lower <- qgamma(.025, shape = alpha_star, scale = beta_star)
p1_upper <- qgamma(.975, shape = alpha_star, scale = beta_star)

p1_post_plot <-
    p1_df %>%
    as.data.frame(.) %>%
    gather(Probability, Density, -lambda) %>%
    ggplot() +
    geom_vline(aes(xintercept = p1_mean, color = 'mean')) +
    geom_vline(aes(xintercept = p1_median, color = 'median')) +
    geom_vline(aes(xintercept = p1_mode, color = 'mode')) +
    geom_line(aes(x=lambda, 
                  y=Density, 
                  color=Probability), 
              stat = 'identity', 
              show.legend = F, 
              size=1.5) +
    xlim(c(0.75, 3.5)) +
    theme_gray() +
    labs(color = NULL) +
    theme(legend.position = c(0.9, 0.75))

#HW3
hw3_mean <- 1.95
hw3_sd <- 0.305
hw3_median <- 2
hw3_mode <- 2
hw3_lower <- 1.4
hw3_upper <- 2.6

p1_stats <- NULL
p1_stats$Problem <- c('HW 3 Problem 2', 'HW 4 Problem 1')
p1_stats$Mean <- c(hw3_mean, p1_mean)
p1_stats$StDev <- c(hw3_sd, p1_sd)
p1_stats$Median <- c(hw3_median, p1_median)
p1_stats$Mode <- c(hw3_mode, p1_mode)
p1_stats$Lower <- c(hw3_lower, p1_lower)
p1_stats$Upper <- c(hw3_upper, p1_upper)


