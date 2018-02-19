x <- seq(0, 6, by=0.001)

#Prior
p4_prior <- dgamma(x, shape = p2_alpha, scale = p2_beta)

#Posterior
p4_post <- dgamma(x, shape = p3_alpha_star, scale = p3_beta_star)

#Norm Like
p4_nl=dgamma(x, shape=sum(sum_xi)+1,scale=1/sum(occurences)) 


p4.plot <-
    data.frame('X' = x,
           'prior' = p4_prior,
           'post' = p4_post,
           'normLik' = p4_nl) %>%
    gather(Probability, Density, -X) %>%
    ggplot() +
    geom_line(aes(x=X, 
                  y=Density, 
                  color=Probability), 
              stat = 'identity', 
              size=1.5) +
    theme_dark() +
    labs(title='Triplot of Car Arrival Rates')
