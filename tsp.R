library(TSP)
library(xega)
library(xegaSelectGene)
source("newTSP_bin.R")

# load data
data <- read_TSPLIB("ftv47.atsp")
D <- as.matrix(data)

# create asymmetric TSP
tsp_binary <- newTSP_bin(D = D, Cities = 48,  Name = "ftv47_binary")
# simple binary coded algorithm
cat("-------------------------------------------------------\n binary coded:\n")
tsp_binary_solution <- xegaRun(penv=tsp_binary, algorithm="sga", max=FALSE, generations=250, popsize=250, genemap = "Permutation", verbose=0)
print(tsp_binary_solution$solution)

# create asymmetric TSP
tsp_permutation <- newTSP(D = D, Cities = rownames(D), Name = "ftv47_permutation")
# permutation-coded
cat("--------------------------------------------------\n permutation coded:\n")
tsp_permutation_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=250, popsize=250, genemap = "Identity", verbose=0)
print(tsp_permutation_solution$solution)

# using heuristics
# greedy
cat("-------------------------------------------------------\n greedy coded:\n")
tsp_greedy_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=50, popsize=50, genemap = "Identity", mutation = "MutateGeneGreedy", verbose=0)
print(tsp_greedy_solution$solution)
# best greedy
cat("--------------------------------------------------\n best greedy coded:\n")
tsp_best_greedy_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=50, popsize=50, genemap = "Identity", mutation = "MutateGeneBestGreedy", verbose=0)
print(tsp_best_greedy_solution$solution)
# Lin-Kernighan
cat("------------------------------------------------\n Lin-Kernighan coded:\n")
tsp_lin_kernighan_solution <- xegaRun(penv=tsp_permutation, algorithm="sgperm", max=FALSE, generations=50, popsize=50, genemap = "Identity", mutation = "MutateGenekOptLK", verbose=0)
print(tsp_lin_kernighan_solution$solution)