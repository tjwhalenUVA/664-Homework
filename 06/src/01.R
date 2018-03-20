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



