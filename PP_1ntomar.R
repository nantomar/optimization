library("lpSolveAPI")
z <- read.lp("PP_1ntomar.lp")
z

solve(z)
get.objective (z)

get.variables(z)
