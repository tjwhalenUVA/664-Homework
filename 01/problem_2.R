#Problem 2====
#pos given commit
sensitivity.2 <- .8
#p neg given not commit
specificity.2 <- .85

loss.noAdminister.Innocent <- 0
loss.Administer.Guilty <- 1
loss.Administer.Innocent <- 10
loss.noAdminister.Guilty <- 100

# Find the policy that minimizes expected loss.
df.p2 <- NULL
df.p2$State <- c('Commit, Positive', 
                 'Commit, Negative', 
                 'No Commit, Positive', 
                 'No Commit, Negative')
df.p2$Probability <- c('.8*P(Commit)', 
                       '.2*P(Commit)', 
                       '.15*(1-P(Commit))', 
                       '.85*(1-P(Commit))')
df.p2$Action <- c('Sanction', 
                  'No Sanction', 
                  'Sanction', 
                  'No Sanction')
df.p2$Loss <- c(loss.Administer.Guilty, 
                loss.noAdminister.Guilty, 
                loss.Administer.Innocent, 
                loss.noAdminister.Innocent)

df.p2 <- as.data.frame(df.p2)

# For the decision of whether to administer sanctions to 
# an individual who may have considered a security violation 
# as described in Problem 3, find the range of prior 
# probabilities for which considering the evidence results 
# in lower expected loss than ignoring or not collecting the evidence. 
# Discuss your results.
p2.df.plot <- NULL
p2.df.plot$xVals <- seq(from = 0, to = 1, by = .01)
p2.df.plot$noSanction <- p2.df.plot$xVals * loss.noAdminister.Guilty
p2.df.plot$sanction <- p2.df.plot$xVals + (1 - p2.df.plot$xVals) * loss.Administer.Innocent
p2.df.plot$followTest <- 19.3 * p2.df.plot$xVals + 1.5

int.df <- NULL
int.df$x <- c(1.5/80.7, 8.5/28.3)
int.df$y <- 19.3 * int.df$x + 1.5
int.df <- as.data.frame(int.df)

p2.df.plot <- 
    as.data.frame(p2.df.plot) %>%
    rename(`No Sanction` = noSanction, 
           Sanction = sanction, 
           `Follow Test` = followTest) %>%
    gather(Policy, value, -xVals)

p2.plot <-
    ggplot() +
    geom_line(data=p2.df.plot, 
              aes(x=xVals, y=value, group=Policy, color=Policy), 
              stat='identity') +
    geom_point(data = int.df, 
               aes(x=x, y=y)) +
    geom_text(data = int.df, 
              aes(x=x, y=y, label=round(x, 4)), 
              vjust=-1, 
              size=3) +
    theme_bw() +
    labs(title='Minimizing Expected Loss', 
         x='P(Commit)', 
         y='Expected Loss') +
    ylim(c(0, 15))
