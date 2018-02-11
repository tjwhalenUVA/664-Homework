suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(readxl))
suppressMessages(library(scales))
suppressMessages(library(gridExtra))
suppressMessages(library(MASS))
suppressMessages(library(fitdistrplus))


#The inter arrival time is the time between each arrival into the system and the next.
interArrival <- c(12, 2, 6, 2, 19, 5, 34, 4, 1, 4, 
                  8, 7, 1, 21, 6, 11, 8, 28, 6, 4, 
                  5, 1, 18, 9, 5, 1, 21, 1, 1, 5, 
                  3, 14, 5, 3, 4, 5, 1, 3, 16, 2)