# heuristic solution
# weight_per_value <- weights / values
# volume_per_value <- volume / values
# X <- cbind(values, weights, weight_per_value, volume, volume_per_value)
# colnames(X) <- c("value", "weight", "weight/value", "volume", "volume/value")
# print(X)
# X_weight_volume <- X[order(X[, "weight/value"], X[, "volume/value"]), ]
# print(X_weight_volume)

# weight_packed <- 0
# volume_packed <- 0
# value_packed <- 0
# while ((weight_packed < weight_limit) & (volume_packed < volume_limit)) {
#     value_packed <- value_packed + X_weight_volume[1,1]
#     weight_packed <- weight_packed + X_weight_volume[1, 2]
#     volume_packed <- volume_packed + X_weight_volume[1, 4]
#     X_weight_volume <- X_weight_volume[-1, ]
# }
# cat("Value: ", value_packed, "Volume: ", volume_packed, "Weight: ", weight_packed)


# library(TSP)
# library(xega)
# library(xegaSelectGene)
# source("newTSP_bin.R")

# load data
# data <- read_TSPLIB("ftv47.atsp")
# D <- as.matrix(data)

# create asymmetric TSP
# tsp_binary <- newTSP_bin(D = D, Cities = 48,  Name = "ftv47_binary")
# simple binary coded algorithm
# cat("-------------------------------------------------------\n binary coded:\n")
# tsp_binary_solution <- xegaRun(penv=tsp_binary, algorithm="sga", max=FALSE, generations=100, popsize=10, genemap = "Permutation")
# tsp_binary_solution_by_generation <- -1 * tsp_binary_solution$popStat[,6]
# print(tsp_binary_solution$popStat)
# print(tsp_binary_solution$solution$phenotypeValue)

# tsp_binary_solution_a <- xegaRun(penv=tsp_binary, algorithm="sga", max=FALSE, generations=10, popsize=10, genemap = "Permutation", verbose=0)
# tsp_binary_solution_by_generation_a <- -1 * tsp_binary_solution_a$popStat[,6]
# print(tsp_binary_solution_a$popStat)
# print(tsp_binary_solution_a$solution$phenotypeValue)


# plot(tsp_binary_solution_by_generation, type = "l", xlab = "generations", ylab = "Route Cost", col = "red", ylim = c(0, 7000))
# lines(tsp_binary_solution_by_generation_a, type = "l", col = "blue")
# legend("topright", legend=c("Binary", "Permutation"),
#        col=c("red", "blue"), lwd=2)


# print(tsp_binary_solution$timer$tMainLoop)




# cost_binary <- tsp_binary_solution$solution$phenotypeValue
# cost_permutation <- tsp_permutation_solution$solution$phenotypeValue
# cost_greedy <- tsp_greedy_solution$solution$phenotypeValue
# cost_best_greedy <- tsp_best_greedy_solution$solution$phenotypeValue
# cost_lin_kernighan <- tsp_lin_kernighan_solution$solution$phenotypeValue
# plot the final result
# costs <- c(cost_binary, cost_permutation, cost_greedy, cost_best_greedy, cost_lin_kernighan)
# barplot(costs,
#         main = "Cost of the final TSP-Route",
#         ylab = "Cost",
#         col = c("red", "#00d0ff", "green", "purple", "#eaff00"),
#         names.arg = c("Binary", "Permutation", "Greedy", "Best Greedy", "Lin-\nKernighan"))