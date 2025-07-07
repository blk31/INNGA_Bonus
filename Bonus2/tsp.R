library(TSP)
library(xega)
library(xegaSelectGene)
source("Bonus2/newTSP_bin.R")

# load data
data <- read_TSPLIB("Bonus2/ftv47.atsp")
D <- as.matrix(data)

# create asymmetric TSP
tsp_binary <- newTSP_bin(D = D, Cities = 48,  Name = "ftv47_binary")
# simple binary coded algorithm
cat("-------------------------------------------------------\n binary coded:\n")
tsp_binary_solution <- xegaRun(penv=tsp_binary, algorithm="sga", max=FALSE, generations=200, popsize=200, genemap = "Permutation", verbose=0)
tsp_binary_solution_by_generation <- -1 * tsp_binary_solution$popStat[,6] # save the solution for each generation
time_binary <- tsp_binary_solution$timer$tMainLoop # save the time the algorithm needed


# create asymmetric TSP
tsp_permutation <- newTSP(D = D, Cities = rownames(D), Name = "ftv47_permutation")
# permutation-coded
cat("--------------------------------------------------\n permutation coded:\n")
tsp_permutation_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=200, popsize=200, genemap = "Identity", verbose=0)
tsp_permutation_solution_by_generation <- -1 * tsp_permutation_solution$popStat[,6] # save the solution for each generation
time_permutation <- tsp_permutation_solution$timer$tMainLoop # save the time the algorithm needed

# using heuristics
# greedy
cat("-------------------------------------------------------\n greedy coded:\n")
tsp_greedy_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=200, popsize=200, genemap = "Identity", mutation = "MutateGeneGreedy", verbose=0)
tsp_greedy_solution_by_generation <- -1 * tsp_greedy_solution$popStat[,6] # save the solution for each generation
time_greedy <- tsp_greedy_solution$timer$tMainLoop # save the time the algorithm needed
# best greedy
cat("--------------------------------------------------\n best greedy coded:\n")
tsp_best_greedy_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=200, popsize=200, genemap = "Identity", mutation = "MutateGeneBestGreedy", verbose=0)
tsp_best_greedy_solution_by_generation <- -1 * tsp_best_greedy_solution$popStat[,6] # save the solution for each generation
time_best_greedy <- tsp_best_greedy_solution$timer$tMainLoop # save the time the algorithm needed
# Lin-Kernighan
cat("------------------------------------------------\n Lin-Kernighan coded:\n")
tsp_lin_kernighan_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=200, popsize=200, genemap = "Identity", mutation = "MutateGenekOptLK", verbose=0)
tsp_lin_kernighan_solution_by_generation <- -1 * tsp_lin_kernighan_solution$popStat[,6] # save the solution for each generation
time_lin_kernighan <- tsp_lin_kernighan_solution$timer$tMainLoop # save the time the algorithm needed


# plot the results for all algorithms over the generations
plot(tsp_binary_solution_by_generation, type = "l", xlab = "generations", ylab = "Route Cost", col = "red", lwd=2, ylim = c(0, 7000))
lines(tsp_permutation_solution_by_generation, type = "l", col = "#00d0ff", lwd=2)
lines(tsp_greedy_solution_by_generation, type = "l", col = "green", lwd=2)
lines(tsp_best_greedy_solution_by_generation, type = "l", col = "purple", lwd=2)
lines(tsp_lin_kernighan_solution_by_generation, type = "l", col = "#eaff00", lwd=2)
legend("topright", legend=c("Binary", "Permutation", "Greedy", "Best Greedy", "Lin-Kernighan"),
       col=c("red", "#00d0ff", "green", "purple", "#eaff00"), lwd=2)

# plot the time needed for each algorithm
times <- c(time_binary, time_permutation, time_greedy, time_best_greedy, time_lin_kernighan)
barplot(times,
        main = "Execution Time of TSP Algorithms",
        ylab = "Time (seconds)",
        col = c("red", "#00d0ff", "green", "purple", "#eaff00"),
        names.arg = c("Binary", "Permutation", "Greedy", "Best Greedy", "Lin-\nKernighan"))