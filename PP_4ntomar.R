library("lpSolveAPI")
x <- read.lp("PP_4ntomar.lp")
x
solve(x)
get.objective (x)
get.constraints(x)
get.variables(x)