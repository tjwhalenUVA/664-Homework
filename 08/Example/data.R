#Packages
library(tidyverse)
library(magrittr)

#Data URL
www <- "http://www.stat.columbia.edu/~gelman/book/data/schiz.asc"

#Read and Wrangle
ppl <- read.table(www, skip = 5) %>% 
    mutate(condition = rep(c("non-schizophrenics", "schizophrenics"), 
                           c(11, 6)))

schiz <- ppl %>%
    filter(., condition == 'schizophrenics') %>% 
    select(-condition) %>%
    mutate(subject = sprintf("p%s", 1:6)) %>%
    gather(reaction, ms, -subject) %>%
    mutate(lms = log(ms))

nschiz <- ppl %>%
    filter(., condition == 'non-schizophrenics') %>% 
    select(-condition) %>%
    mutate(subject = sprintf("p%s", 1:11)) %>%
    gather(reaction, ms, -subject) %>%
    mutate(lms = log(ms))

#Shapiro
result.shapiro <- NULL
for(s in unique(nschiz$subject)){
    rt <- filter(nschiz, subject == s)$lms
    st <- shapiro.test(rt)
    result.shapiro <- rbind(result.shapiro,
                            c(s, st$p.value, st$statistic))
}

df.sh <-
    data.frame('subject' = result.shapiro[,1],
           'PValue' = as.numeric(result.shapiro[,2]),
           'W' = as.numeric(result.shapiro[,3])) %>%
    arrange(PValue)


#QQ Plot
qqplot.data <- function (vec){
    y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    return(list('slope'=slope, 'int'=int))
}
sl.int <- NULL
for(s in unique(nschiz$subject)){
    rt <- filter(nschiz, subject == s)$lms
    if(is.null(sl.int)){
        sl.int <- data.frame('subject' = s,
                             'slope'=qqplot.data(rt)$slope,
                             'int'=qqplot.data(rt)$int)
    }
    else{
        sl.int <- bind_rows(sl.int,
                            data.frame('subject' = s,
                                       'slope'=qqplot.data(rt)$slope,
                                       'int'=qqplot.data(rt)$int))
    }
    
}

ggplot() +
    stat_qq(data = nschiz %>% 
                mutate(subject = factor(subject,
                                        levels = df.sh$subject)), 
            aes(sample=lms, color = subject),
            show.legend = F) +
    geom_abline(data = sl.int %>% 
                    mutate(subject = factor(subject,
                                            levels = df.sh$subject)),
                aes(slope = slope, intercept = int)) +
    facet_wrap(~subject, scales = 'free')


#Density Plot
nschiz %>%
    ggplot() +
    geom_density(aes(x=lms,
                     fill=subject),
                 show.legend = F) +
    facet_wrap(~subject)



#Variance common?
nschiz %>%
    group_by(subject) %>%
    summarise(variance = var(lms)) %>%
    mutate(subject = factor(subject, levels = unique(nschiz$subject))) %>%
    ggplot(aes(x=subject, y=variance)) +
    geom_bar(aes(fill=variance),
             stat = 'identity',
             show.legend = F) +
    geom_text(aes(label=round(variance, 3),
                  y=variance/2)) +
    scale_fill_gradient(low='blue', high='red') +
    theme_classic() +
    labs(title='Individual Subject Sample Variances',
         x='Subject',
         y='Sample Variance')

#Variance
result.var <- NULL
for(s1 in unique(nschiz$subject)){
    for(s2 in unique(nschiz$subject)){
        #Subjects being compared
        p1 <- filter(nschiz, subject == s1)$lms
        p2 <- filter(nschiz, subject == s2)$lms
        #Test
        res <- var.test(p1, p2, alternative = "two.sided")
        #Save results
        result.var <- rbind(result.var,
                            c(s1, 
                              s2,
                              res$statistic))
    }
}


data.frame('subjectA' = result.var[,1],
           'subjectB' = result.var[,2],
           'VarRatio' = as.numeric(result.var[,3])) %>%
    mutate(subjectA = factor(subjectA, levels = unique(nschiz$subject)),
           subjectB = factor(subjectB, levels = unique(nschiz$subject)),
           VarRatio = ifelse(subjectA == subjectB, NA, VarRatio)) %>%
    ggplot(aes(subjectA, subjectB)) +
    geom_tile(aes(fill = cut(VarRatio, 
                             breaks=seq(0.25, 4, .5), 
                             labels=seq(0.5, 3.5, .5))), 
              color = "black") +
    geom_text(aes(label = round(VarRatio, 2))) +
    scale_fill_manual(drop=FALSE, 
                      values=colorRampPalette(c("white","red"))(7), 
                      na.value="gray") +
    theme_classic() +
    labs(fill="Ratio of\nVariances",
         x="Subject A",
         y="Subject B",
         title="Heat Map of Sample Variance Ratios") +
    theme(axis.line = element_blank())



data.frame('subjectA' = result.var[,1],
           'subjectB' = result.var[,2],
           'VarRatio' = as.numeric(result.var[,3])) %>%
    mutate(subjectA = factor(subjectA, levels = unique(nschiz$subject)),
           subjectB = factor(subjectB, levels = unique(nschiz$subject)),
           VarRatio = ifelse(subjectA == subjectB, NA, VarRatio),
           sim_.75_1.25 = ifelse(VarRatio >= 0.75 & VarRatio <= 1.25, 1, 0),
           sim_.5_1.5 = ifelse(VarRatio >= 0.5 & VarRatio <= 1.5, 1, 0)) %>%
    filter(!is.na(VarRatio)) %>%
    summarise(N = n(),
              sim_.75_1.25 = sum(sim_.75_1.25),
              sim_.5_1.5 = sum(sim_.5_1.5))

#Parameter Estimation
ns.param <- 
    nschiz %>%
    group_by(subject) %>%
    summarise(variance = var(lms)) %>%
    mutate(precision = 1 / variance) %>%
    ungroup()

ns.beta <- var(ns.param$precision)/mean(ns.param$precision)
ns.alpha <- mean(ns.param$precision) / ns.beta

ns.mu <- mean(nschiz$lms)
ns.k <- nschiz %>%
    group_by(subject) %>%
    summarise(smean = mean(lms)) %>%
    ungroup() %>%
    summarise(svar = var(smean)) %>%
    mutate(sprec = 1/ svar,
           sk = sprec / mean(ns.param$precision)) %>%
    select(sk)
ns.k <- ns.k[[1]]




#Calculate Posterior Distributions
findPost <- function(s){
    subjSamples <- filter(nschiz, subject == s)$lms
    nSubj <- nrow(filter(nschiz, subject == s))
    xbarSubj <- mean(subjSamples)
    #Alpha
    aStar <- ns.alpha + nSubj / 2
    #Beta
    b.p1 <- 0.5 * sum((subjSamples - xbarSubj)^2)
    b.p2 <- ns.k * nSubj * (xbarSubj - ns.mu)^2 / (2 * (ns.k + nSubj))
    bStar <- (ns.beta^-1 + b.p1 + b.p2)^-1
    #Mu
    muStar <- (ns.k * ns.mu + sum(subjSamples)) / (ns.k + nSubj)
    #k
    kStar = ns.k + nSubj
    
    res <- list("mu" = muStar, "k" = kStar, "alpha" = aStar, "beta" = bStar)
    return(res)
}

nsPost <- NULL
for(s in unique(nschiz$subject)){
    if(is.null(nsPost)){
        nsPost <- data.frame(subject = s, findPost(s))
    }
    else{
        nsPost %<>% bind_rows(.,
                              data.frame(subject = s, findPost(s)))
    }
}



nsPost %>%
    mutate(ThetaCI = sprintf("[%s, %s]",
                             round(qnorm(0.025, mu, sqrt(1/(alpha * beta * k))), 2),
                             round(qnorm(0.975, mu, sqrt(1/(alpha * beta * k))), 2)),
           PCI = sprintf("[%s, %s]",
                         round(qgamma(0.025, alpha, beta), 2),
                         round(qgamma(0.975, alpha, beta), 2))) %>%
    select(subject, ThetaCI, PCI)



nsNorm <-
    nsPost %>%
    mutate(sigma = sqrt(1/(alpha * beta * k))) %>%
    select(mu, sigma) %>%
    as.matrix()

nsPostDens <- data.frame(theta = seq(5.5,6,length.out = 1000))
for(r in 1:nrow(nsNorm)){
    s <- sprintf("p%s", r)
    nsPostDens[s] <- dnorm(nsPostDens$theta, 
                           nsNorm[r,'mu'], 
                           nsNorm[r,'sigma'])
}

nsPostDens %>%
    gather(subject, density, -theta) %>%
    ggplot() +
    geom_line(aes(x=theta,
                  y=density,
                  color=subject),
              size=1) +
    theme_classic() +
    theme(legend.position = 'top') +
    guides(col = guide_legend(nrow = 1, byrow = TRUE))



nsPost %>%
    mutate(ThetaLow = qnorm(0.025, mu, sqrt(1/(alpha * beta * k))),
           ThetaHigh = qnorm(0.975, mu, sqrt(1/(alpha * beta * k))),
           PLow = qgamma(0.025, shape=alpha, scale=beta),
           PHigh = qgamma(0.975, shape=alpha, scale=beta)) %>%
    select(-mu, -k, -alpha, -beta) %>%
    gather(CI, Value, -subject) %>%
    mutate(Group = ifelse(CI == 'PLow' | CI == 'PHigh', 'Precision', 'Theta'),
           End = ifelse(CI == 'PLow' | CI == 'ThetaLow', 'Low', 'High')) %>%
    select(-CI) %>%
    spread(End, Value) %>%
    mutate(Group = factor(Group, levels=c('Theta', 'Precision')),
           subject = factor(subject, levels=unique(nschiz$subject))) %>%
    ggplot() +
    geom_segment(aes(x=Low, xend=High, y=subject, yend=subject, color=subject),
                 size=4, show.legend = F) +
    facet_wrap(~Group, scales = 'free_x') +
    theme_classic() +
    labs(title='95% Credible Intervals',
         x='Credible Interval Values',
         y='Subject') +
    theme(panel.grid.major.y = element_line(color='gray90'),
          panel.grid.major.x = element_line())
