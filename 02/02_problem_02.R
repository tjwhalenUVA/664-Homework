p2.df <- NULL
p2.df$theta <- seq(0.025, 0.975, length.out=20)
p2.df$prior <- 1/20

#A====
a.rats <- 13
a.tumor.rats <- 2

p.A <- p2.df$prior*p2.df$theta^a.tumor.rats*(1-p2.df$theta)^(a.rats-a.tumor.rats)
p2.df$postA <- p.A/sum(p.A)


#B====
b.rats <- 18
b.tumor.rats <- 1

p.B <- p2.df$postA*p2.df$theta^b.tumor.rats*(1-p2.df$theta)^(b.rats-b.tumor.rats)
p2.df$postB <- p.B/sum(p.B)

#C====
c.rats <- 31
c.tumor.rats <- 3

p.C <- p2.df$prior*p2.df$theta^c.tumor.rats*(1-p2.df$theta)^(c.rats-c.tumor.rats)
p2.df$postC <- p.C/sum(p.C)

#Plot====
p2.sum.df <-
    p2.df %>%
    as.data.frame(.) %>%
    gather(Problem, Posterior, -theta, -prior) %>%
    mutate(Posterior = Posterior * 20, 
           Problem = factor(Problem, 
                            levels=c('postA', 'postB', 'postC'), 
                            labels=c('A', 'B', 'C')), 
           tmp = theta * Posterior) %>%
    group_by(Problem) %>%
    summarise(Mean = mean(tmp, na.rm=T)) 

p2.sum.df <-
    p2.sum.df %>%
    mutate(SD = c(sqrt(sum((p2.df$theta-filter(p2.sum.df, Problem == 'A')$Mean)^2*p2.df$postA)), 
                  sqrt(sum((p2.df$theta-filter(p2.sum.df, Problem == 'B')$Mean)^2*p2.df$postB)), 
                  sqrt(sum((p2.df$theta-filter(p2.sum.df, Problem == 'C')$Mean)^2*p2.df$postC))), 
           PlusSD = Mean + SD, 
           MinusSD = Mean - SD) 

p2.plot <- function(problem){
    result <-
        p2.df %>%
        as.data.frame(.) %>%
        gather(Problem, Posterior, -theta, -prior) %>%
        mutate(Problem = factor(Problem, 
                                levels=c('postA', 'postB', 'postC'), 
                                labels=c('A', 'B', 'C'))) %>%
        filter(Problem == problem) %>%
        ggplot(aes(x=theta, 
                   y=Posterior)) +
        geom_bar(stat = 'identity') +
        geom_vline(data=filter(p2.sum.df, 
                               Problem == problem), 
                   aes(xintercept=PlusSD, 
                       color='One SD')) +
        geom_vline(data=filter(p2.sum.df, 
                               Problem == problem), 
                   aes(xintercept=MinusSD, 
                       color='One SD')) +
        geom_vline(data=filter(p2.sum.df, 
                               Problem == problem), 
                   aes(xintercept=Mean, 
                       color='Mean'), 
                   size=1) +
        geom_label(data=filter(p2.sum.df, 
                               Problem == problem), 
                   aes(label=round(Mean, 3), 
                       x=Mean, 
                       y=0.4, 
                       fill='Mean'), 
                   show.legend = F) +
        theme_bw() +
        labs(title=paste("Posterior Distribution:", 
                         problem, 
                         sep=" "), 
             color='Metric') +
        ylim(c(0, 0.5))
    
    return(result)
}
