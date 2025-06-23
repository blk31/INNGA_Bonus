source("knapsack_env.R")
library(xega)

values <- c(48, 30, 42, 36, 22, 43, 18, 24, 36, 29, 30, 25, 19, 41, 34, 32, 27, 24, 18)
weights <- c(10, 30, 12, 22, 12, 20, 9, 9, 18, 20, 25, 18, 7, 16, 24, 21, 21, 32, 9) 
volume <- c(15, 20, 18, 20, 5, 12, 7, 7, 24, 30, 25, 20, 5, 25, 19, 24, 19, 14, 30)
weight_limit <- 250
volume_limit <- 250
print(sum(weights))
print(sum(volume))
print(sum(values))

k <- knapsack_env_factory(values, weights, volume, length(values))
k_max <- xegaRun(penv=k, algorithm="sga", max=TRUE, generations=500, popsize=200, verbose=0)
print(k_max$solution)
cat("Value: ", k_max$solution$phenotypeValue, "Volume: ", k_max$solution$phenotype %*% volume, "Weight: ", k_max$solution$phenotype %*% weights, "\n")
cat("indexes for max values according to GA: ", k_max$solution$phenotype)