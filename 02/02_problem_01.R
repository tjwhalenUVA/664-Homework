#Gamble 1====
g1.df <- NULL
g1.df$x <- seq(0, 1, by=0.01)
g1.df$A <- 0.33 + 0.66 * g1.df$x
g1.df$B <- g1.df$x

#Gamble 2====
g2.df <- NULL
g2.df$x <- seq(0, 1, by=0.01)
g2.df$C <- 0.33
g2.df$D <- 0.34 * g2.df$x

#Intersections====
int.df <- NULL
int.df$x <- .33/.34
int.df$Utility <- c(.33/.34, .33)
int.df$Gamble <- c('Gamble 1', 'Gamble 2')
int.df <- as.data.frame(int.df)

#Plots====
g1.plot <-
    g1.df %>%
    as.data.frame(.) %>%
    gather(Option, Utility, -x) %>%
    mutate(Gamble = 'Gamble 1') %>%
    ggplot(aes(x=x, 
               y=Utility, 
               color=Option)) +
    geom_line(stat = 'identity') +
    geom_point(data = filter(int.df, Gamble == 'Gamble 1'), 
               aes(x=x, 
                   y=Utility), 
               color='black') +
    geom_text(data = filter(int.df, Gamble == 'Gamble 1'), 
              aes(x=x, 
                  y=Utility, 
                  label=round(x, 4)), 
              color='black', 
              vjust=1) +
    labs(title='Gamble 1 Expected Utility', 
         x='U($2,400)', 
         y='E[U]') +
    theme_bw() +
    ylim(c(0, 1))

g2.plot <-
    g2.df %>%
    as.data.frame(.) %>%
    gather(Option, Utility, -x) %>%
    mutate(Gamble = 'Gamble 2') %>%
    ggplot(aes(x=x, 
               y=Utility, 
               color=Option)) +
    geom_line(stat = 'identity') +
    geom_point(data = filter(int.df, Gamble == 'Gamble 2'), 
               aes(x=x, 
                   y=Utility), 
               color='black') +
    geom_text(data = filter(int.df, Gamble == 'Gamble 2'), 
              aes(x=x, 
                  y=Utility, 
                  label=round(x, 4)), 
              color='black', 
              vjust=-1) +
    labs(title='Gamble 2 Expected Utility', 
         x='U($2,400)', 
         y='E[U]') +
    theme_bw() +
    ylim(c(0, 1))

gamble.plot <-
    g1.df %>%
    as.data.frame(.) %>%
    gather(Option, Utility, -x) %>%
    mutate(Gamble = 'Gamble 1') %>%
    bind_rows(., 
              g2.df %>%
                  as.data.frame(.) %>%
                  gather(Option, Utility, -x) %>%
                  mutate(Gamble = 'Gamble 2')) %>%
    ggplot(aes(x=x, 
               y=Utility, 
               color=Option)) +
    geom_line(stat = 'identity') +
    geom_point(data = int.df, 
               aes(x=x, 
                   y=Utility), 
               color='black') +
    geom_text(data = int.df, 
              aes(x=x, 
                  y=Utility, 
                  label=round(x, 4)), 
              color='black', 
              vjust=c(1, -1), 
              hjust=c(1,1)) +
    labs(title='Expected Utility', 
         x='U($2,400)', 
         y='E[U]') +
    theme_bw() +
    ylim(c(0, 1)) +
    facet_wrap(~Gamble, nrow=1)
