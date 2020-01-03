# Monty Hall Problem - vectorised solutions
set.seed(278319)
simNum <- 1000

# data.table solutino
library(data.table)

simdt <- CJ(sim=1:simNum, door=1:3)

mhdt <-  simdt[, `:=`(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1)), switch=0), .(sim)][
    order(sim, -guess, true)][
    rowid(sim)==3, switch:=1][
    , .(stayWin = sum(guess*true)/simNum, switchWin=sum(switch*true)/simNum)]

mhdt

# tidyverse solution
library(tidyr)
library(dplyr)

simtv <- expand_grid(sim=1:simNum, door=1:3)

mhtv <- simtv %>%
    group_by(sim) %>%
    mutate(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1))) %>%
    arrange(sim, -guess, true) %>%
    mutate(switch = as.numeric(row_number(sim)==3)) %>%
    ungroup %>%
    summarise(stayWin = sum(guess*true)/simNum,
              switchWin=sum(switch*true)/simNum)

mhtv

# Base R solution
mhbr <- expand.grid(sim=1:simNum, door=1:3)
by(mhbr, mhbr[, "sim"], sample(c(0, 0, 1)))


