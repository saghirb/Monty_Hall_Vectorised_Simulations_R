## ----simSetup-------------------------------------------------------------------------------------------
set.seed(663948)
simNum <- 100000  # Number of simulations
doorNum <- 3      # Number of doors (not strictly necessary)


## ----brSim----------------------------------------------------------------------------------------------
# A function to simulate randomly chosen doors for the truth door and the guess.
rDoors <- function(sims){
  unlist(lapply(1:sims, function(x) sample(c(0, 0, 1), replace = FALSE)))
}

# Note that for this method to work the data must be sorted by "sim" then the door 
# (hence the order in the expand.grid() function).
mhbrGrid <- expand.grid(door=1:doorNum, sim=1:simNum)
mhbrGrid$true <- rDoors(simNum)
mhbrGrid$guess <- rDoors(simNum)

mhbr <- mhbrGrid[with(mhbrGrid, order(sim, -guess, true)), ]
mhbr$switch <- rep(c(0, 0, 1), simNum)
mhbr$stayWin <- with(mhbr, guess*true)
mhbr$switchWin <- with(mhbr, switch*true)

mhBaseR <- rbind(list(stayWin = sum(mhbr$stayWin)/simNum, 
                      switchWin = sum(mhbr$switchWin)/simNum))
mhBaseR


## ----dtSim, message=FALSE-------------------------------------------------------------------------------
library(data.table)

simdt <- CJ(sim=1:simNum, door=1:doorNum)

mhdt <-  simdt[, `:=`(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1)), switch=0), .(sim)][
    order(sim, -guess, true)][
    rowid(sim)==doorNum, switch:=1][
    , .(stayWin = sum(guess*true)/simNum, switchWin = sum(switch*true)/simNum)]

mhdt


## ----tvSim, message=FALSE-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)

simtv <- expand_grid(sim=1:simNum, door=1:doorNum)

mhtv <- simtv %>%
    group_by(sim) %>%
    mutate(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1))) %>%
    arrange(sim, -guess, true) %>%
    mutate(switch = as.numeric(row_number(sim)==doorNum)) %>%
    ungroup %>%
    summarise(stayWin = sum(guess*true)/simNum,
              switchWin = sum(switch*true)/simNum)

mhtv

