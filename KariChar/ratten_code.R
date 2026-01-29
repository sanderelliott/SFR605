# Data load ----
#setwd("KariChar")

# note - the top temp logger only took one point of data on the first day
#        so I didn't include it
hobo_2 <- read.csv("10040750(2nd_down) - 10040750(2nd_down).csv")
hobo_3 <- read.csv("10040752_0(3rd_down) - 10040752_0(3rd_down).csv")
hobo_4 <- read.csv("10040751(4th_down) - 10040751(4th_down).csv")
hobo_5 <- read.csv("10040749(5th_down) - 10040749(5th_down).csv")
hobo_6 <- read.csv("10040748(Bottom) - 10040748(Bottom).csv")

# Library Load ----
library(tidyverse)

# Data tidying ----
# . Adding column for hobo logger number and depth ----
hobo_2$depth <- 3
hobo_3$depth <- 5
hobo_4$depth <- 7
hobo_5$depth <- 9
hobo_6$depth <- 11

# . Adding column for hobo logger number ----
hobo_2$hobo_num <- 2
hobo_3$hobo_num <- 3
hobo_4$hobo_num <- 4
hobo_5$hobo_num <- 5
hobo_6$hobo_num <- 6

# . Smooshing everything together ----
hobo_list <- list(hobo_2, hobo_3, hobo_4, hobo_5, hobo_6)
br <- bind_rows(hobo_list)

# . Just checking ----
head(br)
unique(br$depth)
unique(br$hobo_num)





