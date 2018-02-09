#Read in the data
p1.df <- read_excel("C:/Users/e481340/Documents/GMU MASTERS/SYST 664/664 - Homework/03/src/03_data.xlsx",
                    sheet = "p1") %>%
    gather(son, Joint, -father)

#A====
p1.a <-
    p1.df %>%
    group_by(father) %>%
    summarise(Marginal = sum(Joint))

p1.a.plot <-
    p1.a %>%
    ggplot(aes(x=reorder(father, -Marginal), 
               y=Marginal)) +
    geom_bar(stat = 'identity', 
             fill='lightblue') +
    geom_text(aes(label=Marginal, 
                  y=Marginal/2)) +
    theme_dark() +
    labs(title="Marginal Distribution of Fathers' Occupations", 
         y='Marginal Probability', 
         x="Fathers' Occupation") +
    ylim(c(0,1))
#B====
p1.b <- 
    p1.df %>%
    group_by(son) %>%
    summarise(Marginal = sum(Joint))

p1.b.plot <-
    p1.b %>%
    ggplot(aes(x=reorder(son, -Marginal), 
               y=Marginal)) +
    geom_bar(stat = 'identity', 
             fill='lightgreen') +
    geom_text(aes(label=Marginal, 
                  y=Marginal/2)) +
    theme_dark() +
    labs(title="Marginal Distribution of Sons' Occupations", 
         y='Marginal Probability', 
         x="Sons' Occupation") +
    ylim(c(0,1))

#C====
p1.c <- 
    p1.df %>%
    filter(father == 'farm') %>%
    mutate(Conditional = Joint/sum(Joint))

p1.c.plot <-
    p1.c %>%
    ggplot(aes(x=reorder(son, -Conditional), 
               y=Conditional)) +
    geom_bar(stat = 'identity', 
             fill='lightcoral') +
    geom_text(aes(label=round(Conditional, 3), 
                  y=Conditional/2)) +
    theme_dark() +
    labs(title="Conditional Distribution of Sons' Occupation\nGiven the Father is a Farmer", 
         y='Conditional Probability', 
         x="Sons' Occupation") +
    ylim(c(0,1))

#D====
p1.d <- 
    p1.df %>%
    filter(son == 'farm') %>%
    mutate(Conditional = Joint/sum(Joint))

p1.d.plot <-
    p1.d %>%
    ggplot(aes(x=reorder(father, -Conditional), 
               y=Conditional)) +
    geom_bar(stat = 'identity', 
             fill='khaki1') +
    geom_text(aes(label=round(Conditional, 3), 
                  y=Conditional/2)) +
    theme_dark() +
    labs(title="Conditional Distribution of Fathers' Occupation\nGiven the Son is a Farmer", 
         y='Conditional Probability', 
         x="Fathers' Occupation") +
    ylim(c(0,1))
#E====