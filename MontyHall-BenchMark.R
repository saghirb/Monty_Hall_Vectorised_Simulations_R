library(data.table)
library(microbenchmark)

simNum <- 1000000
sim <- CJ(sim=1:simNum, door=1:3)

microbenchmark(
mh1 = sim[, `:=`(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1)), switch =0), .(sim)][
    order(sim, -guess, true)][
    rowid(sim)==3, switch:=1][
    , .(guessProb = sum(guess*true)/simNum, switchProb=sum(switch*true)/simNum)],

mh2 = sim[, `:=`(guess=sample(c(0, 0, 1)), true=sample(c(0, 0, 1)), switch =0), .(sim)][
    order(sim, -guess, true)][
    rowid(sim)==3, switch:=1][
    , `:=`(stayWin = guess*true, switchWin = switch*true)][
    , .(guessProb = sum(stayWin)/simNum, switchProb=sum(switchWin)/simNum)],
times = 20)

