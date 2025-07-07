library(xega)

source("Abgabe/PropEnvs.R")

f1 <- F1(2)
f1_max<- xegaRun(penv=f1, algorithm="sga", max=TRUE, generations=500, popsize=200, verbose=0)
f1_min<- xegaRun(penv=f1, algorithm="sga", max=FALSE, generations=500, popsize=200, verbose=0)

f1_max$solution
f1_min$solution

f2 <- F2(4)
f2_max<- xegaRun(penv=f2, algorithm="sga", max=TRUE, generations=500, popsize=200, verbose=0)
f2_min<- xegaRun(penv=f2, algorithm="sga", max=FALSE, generations=500, popsize=200, verbose=0)

f2_max$solution
f2_min$solution

f3 <- F3(4)
f3_max<- xegaRun(penv=f3, algorithm="sga", max=TRUE, generations=500, popsize=200, verbose=0)
f3_min<- xegaRun(penv=f3, algorithm="sga", max=FALSE, generations=500, popsize=200, verbose=0)

f3_max$solution
f3_min$solution

# depending on max or min set the penalty term to + or - for F4 and F5
f4 <- F4(2)
f4_max<- xegaRun(penv=f4, algorithm="sga", max=TRUE, generations=200, popsize=500, verbose=0)
f4_min<- xegaRun(penv=f4, algorithm="sga", max=FALSE, generations=200, popsize=500, verbose=0)

f4_max$solution
f4_min$solution

f5 <- F5(2)
f5_max<- xegaRun(penv=f5, algorithm="sga", max=TRUE, generations=200, popsize=500, verbose=0)
f5_min<- xegaRun(penv=f5, algorithm="sga", max=FALSE, generations=200, popsize=500, verbose=0)

f5_max$solution
f5_min$solution