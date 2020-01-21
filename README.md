
<!-- README.md is generated from README.Rmd. Please edit that file    -->

<!-- Origin: https://github.com/hrbrmstr/ggalt/blob/master/README.Rmd -->

<!-- Thanks to Bob Rudis for sharing.                                 -->

# Monty Hall Problem - Vectorised Simulations

![Monty Hall Problem Image](README_files/Monty_open_door.svg)

This repository presents three vectorised simulations to the “Monty Hall
Problem” in [R](https://r-project.org) using:

1.  base R.
2.  a [tinyverse](http://tinyverse.org/) approach using the `data.table`
    package.
3.  a [tidyverse](https://tidyverse.org) approach using `dplyr` and
    `tidyr` packages.

## Monty Hall Problem

There is a good write up and discussion about the Monty Hall Problem on
Wikipedia from which I quote:

*The Monty Hall problem is a brain teaser, in the form of a probability
puzzle, loosely based on the American television game show Let’s Make a
Deal and named after its original host, Monty Hall.*

**Question**

> Suppose you’re on a game show, and you’re given the choice of three
> doors: Behind one door is a car; behind the others, goats. You pick a
> door, say No. 1, and the host, who knows what’s behind the doors,
> opens another door, say No. 3, which has a goat. He then says to you,
> “Do you want to pick door No. 2?” Is it to your advantage to switch
> your choice?

**Standard Assumptions**

  - *The host must always open a door that was not picked by the
    contestant.*
  - *The host must always open a door to reveal a goat and never the
    car.*
  - *The host must always offer the chance to switch between the
    originally chosen door and the remaining closed door.*

**Source:** <https://en.Wikipedia.org/wiki/Monty_Hall_problem>

## Objective

> Use a vectorised approach to simulating the probability of winning for
> both staying with the chosen door and switching door when offered.

## Simulations

We set the following parameters to make these simulations reproducible :

``` r
set.seed(663948)
simNum <- 100000
doorNum <- 3
```

Each of the simulations below will create the following variables:

  - `sim` – simulation number.
  - `door` – door number.
  - `true` – behind true door: 1 = car & 0 = goat.
  - `guess` – contestant chosen door: 1 = car & 0 = goat.
  - `switch` – door offered to the contestant to switch to.

These are then summarised as follows:

  - `stayWin` – proportion of winning by staying with `guess` door.
  - `switchWin` – proportion of winning by switching to `switch` door.

## Base R (R Zeroverse) Simulation

The first simulation only uses Base R functions. I call it “R zeroverse”
as CRAN contributed packages are not allowed (just vanilla
R).

``` r
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
##      stayWin switchWin
## [1,] 0.3315  0.6685
```

## `data.table` Simulation

A neat [tinyverse](http://tinyverse.org/) simulation using
[`data.table`](http://r-datatable.com/) package.

``` r
library(data.table)

simdt <- CJ(sim=1:simNum, door=1:doorNum)

mhdt <-  simdt[, `:=`(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1)), switch=0), .(sim)][
    order(sim, -guess, true)][
    rowid(sim)==doorNum, switch:=1][
    , .(stayWin = sum(guess*true)/simNum, switchWin = sum(switch*true)/simNum)]

mhdt
##    stayWin switchWin
## 1: 0.33156   0.66844
```

## Tidyverse Simulation

A neat [tidyverse](https://tidyverse.org) simulation using `dplyr` and
`tidyr` packages.

``` r
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
## # A tibble: 1 x 2
##   stayWin switchWin
##     <dbl>     <dbl>
## 1   0.333     0.667
```

## Summary

  - All three simulations lead to the same conclusion – that it is
    probabilistically better to switch.
  - I provide three vectorised solutions as the R community is diverse
    and with different preferences.
  - Using a `for` loop based approach is perfectly fine for this
    problem. I just wanted code it using a vectorised approach as it was
    bugging me ;)

**Thanks for reading.**

## Acknowledgements

Thank to the [R core](https://www.r-project.org/contributors.html),
[`data.table`](https://rdatatable.gitlab.io/data.table/authors.html) and
[tidyverse](https://tidyverse.org) authors, maintainers and
contributors.
