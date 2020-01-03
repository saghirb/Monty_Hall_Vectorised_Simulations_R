
<!-- README.md is generated from README.Rmd. Please edit that file    -->

<!-- Origin: https://github.com/hrbrmstr/ggalt/blob/master/README.Rmd -->

<!-- Thanks to Bob Rudis for sharing.                                 -->

# Monty Hall Problem - Vectorised Simulations

## `data.table` Simulation

``` r
set.seed(663948)
simNum <- 10000

library(data.table)

simdt <- CJ(sim=1:simNum, door=1:3)

mhdt <-  simdt[, `:=`(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1)), switch=0), .(sim)][
    order(sim, -guess, true)][
    rowid(sim)==3, switch:=1][
    , .(stayWin = sum(guess*true)/simNum, switchWin=sum(switch*true)/simNum)]

mhdt
##    stayWin switchWin
## 1:  0.3343    0.6657
```

## Tidyverse Simulation

``` r
library(tidyr)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union

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
## # A tibble: 1 x 2
##   stayWin switchWin
##     <dbl>     <dbl>
## 1   0.336     0.664
```

## Base R Simulation

``` r
mhbrGrid <- expand.grid(door=1:3, sim=1:simNum, guess=NA, true=NA, switch=0)

mhbr <- do.call(rbind, lapply(split(mhbrGrid, mhbrGrid[, "sim"]), function(x){ 
  x$guess <- sample(c(0, 0, 1))
  x$true <- sample(c(0, 0, 1))
  mh <- x[with(x, order(sim, -guess, true)), ]
  mh$switch[nrow(x)] <- 1
  mh$stayWin <- with(mh, guess*true)
  mh$switchWin <- with(mh, switch*true)
  mh
  }))

cat("\n\t   stayWin = ", sum(mhbr$stayWin)/simNum, 
    "\n\t switchWin = ", sum(mhbr$switchWin)/simNum, "\n")
## 
##     stayWin =  0.3311 
##   switchWin =  0.6689
```
