library("lpSolveAPI")
y <- read.lp("PP_5ntomar.lp")
y
solve(y)
get.objective (y)
get.constraints(y)
get.variables(y)
