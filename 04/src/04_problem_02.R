
#Known
p2_q10 <- 1.5
p2_q50 <- 3.75
p2_q90 <- 7

p2_known <- NULL
p2_known$Parameters <- c('10th Percentile', '50th Percentile', '90th Percentile')
p2_known$Cars <- c(p2_q10, p2_q50, p2_q90)

# p2_q90 / p2_q50

f <- function(k){qgamma(.9, k, 1)/qgamma(.5, k, 1)}
p2_alpha <- uniroot(function(k){f(k)-p2_q90 / p2_q50}, c(0.1, 4))$root
p2_beta <- p2_q50 / qgamma(.5, p2_alpha, 1)

p2_parameters <- NULL
p2_parameters$Parameters <- c('Shape', 'Scale')
p2_parameters$Cars <- c(p2_alpha, p2_beta)

x <- seq(0, 12, by=0.001)

#Prior
p2_prior <- dgamma(x, shape = p2_alpha, scale = p2_beta)

p2.plot <-
    data.frame('X' = x,
               'prior' = p2_prior) %>%
    gather(Probability, Density, -X) %>%
    ggplot() +
    geom_line(aes(x=X, 
                  y=Density, 
                  color=Probability), 
              stat = 'identity', 
              show.legend = F, 
              size=1.5) +
    labs(title = 'Prior Distribution') +
    theme_dark()

p2_comparison <- NULL
p2_comparison$Parameters <- p2_known$Parameters
p2_comparison$Engineer <- p2_known$Cars
p2_comparison$Gamma <- c(qgamma(0.1, shape = p2_alpha, scale = p2_beta),
                         qgamma(0.5, shape = p2_alpha, scale = p2_beta),
                         qgamma(0.9, shape = p2_alpha, scale = p2_beta))



#Is it good?

anyGood_comparison <- NULL
anyGood_comparison$Percentile <- seq(0.1, .99, by = 0.1)
anyGood_comparison$Data <- qgamma(anyGood_comparison$Percentile, shape = alpha_star, scale = beta_star)
anyGood_comparison$Derived <- qgamma(anyGood_comparison$Percentile, shape = p2_alpha, scale = p2_beta)
