#Find alpha
p3_alpha_star <- p2_alpha + sum_xi

#Find beta
N <- sum(occurences)
p3_beta_star <- p2_beta / (1 + N * p2_beta)

#Plot Posterior
p3_df <- NULL
p3_df$lambda <- seq(0, 5, by=.001)
p3_df$p3_density <- dgamma(p3_df$lambda, 
                        shape = p3_alpha_star, 
                        scale = p3_beta_star)
p3_df$p2_density <- dgamma(p3_df$lambda, 
                           shape = p2_alpha, 
                           scale = p2_beta)

p3.plot <-
    p3_df %>%
    as.data.frame(.) %>%
    gather(Probability, Density, -lambda) %>%
    ggplot() +
    geom_line(aes(x=lambda, 
                  y=Density, 
                  color=Probability), 
              stat = 'identity', 
              size=1.5) +
    theme_dark() +
    labs(title='Posterior using Prior from Problem 2')
#B====
#Mean
p3_mean <- p3_alpha_star * p3_beta_star

#SD
p3_sd <- sqrt(p3_alpha_star * p3_beta_star ^ 2)

#Median
p3_median <- qgamma(.5, shape = p3_alpha_star, scale = p3_beta_star)

#Mode
p3_mode <- (p3_alpha_star - 1 ) * p3_beta_star

#C====
p3_lower <- qgamma(.025, shape = p3_alpha_star, scale = p3_beta_star)
p3_upper <- qgamma(.975, shape = p3_alpha_star, scale = p3_beta_star)


p3_stats <- p1_stats
p3_stats$Problem <- c(p1_stats$Problem, 'HW 4 Problem 3')
p3_stats$Mean <- c(p1_stats$Mean, p3_mean)
p3_stats$StDev <- c(p1_stats$StDev, p3_sd)
p3_stats$Median <- c(p1_stats$Median, p3_median)
p3_stats$Mode <- c(p1_stats$Mode, p3_mode)
p3_stats$Lower <- c(p1_stats$Lower, p3_lower)
p3_stats$Upper <- c(p1_stats$Upper, p3_upper)
