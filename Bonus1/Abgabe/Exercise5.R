library(xega)

source("PropEnvs.R")

#Function 1

popsize <- 500
generations <- 200

f1 <- F1(2)
f1_max<- xegaRun(penv=f1, algorithm="sga", max=TRUE, generations=generations, popsize=popsize, verbose=0, log=TRUE)
f1_min<- xegaRun(penv=f1, algorithm="sga", max=FALSE, generations=generations, popsize=popsize, verbose=0, log=TRUE)

f1_min$solution

#Plot Function 

F1_function <- function(x1, x2) {
  x1^2 + 2 * x2^2 - 0.3 * cos(3 * pi * x1) - 0.4 * cos(4 * pi * x2) + 0.7
}

x1_vals <- seq(-5, 5, length.out = 100)
x2_vals <- seq(-5, 5, length.out = 100)

# Generate a matrix of z-values
z <- outer(x1_vals, x2_vals, Vectorize(F1_function))

# Plot
persp(x1_vals, x2_vals, z, theta = 30, phi = 30,
      col = "lightblue", xlab = "x1", ylab = "x2", zlab = "f(x)",
      main = "Bohachevsky Function (F1)")

#Plot Convergence
log_data <- readRDS(f1_max$log)


best_fitness <- sapply(0:(generations - 1), function(g) {
  start <- g * popsize + 1
  end <- start + popsize - 1
  gen_data <- log_data[start:end]
  max(sapply(gen_data, function(ind) ind$fit))  #MIN / MAX
})

plot(0:(generations - 1), best_fitness, type = "l",
     xlab = "Generation", ylab = "Best Fitness",
     main = "Convergence Curve (F1)", col = "blue", lwd = 2)



f2 <- F2(4)
f2_max<- xegaRun(penv=f2, algorithm="sga", max=TRUE, generations=generations, popsize=popsize, verbose=0, log=TRUE)
f2_min<- xegaRun(penv=f2, algorithm="sga", max=FALSE, generations=generations, popsize=popsize, verbose=0, log=TRUE)

#Plot Convergence
log_data <- readRDS(f2_max$log)


best_fitness <- sapply(0:(generations - 1), function(g) {
  start <- g * popsize + 1
  end <- start + popsize - 1
  gen_data <- log_data[start:end]
  max(sapply(gen_data, function(ind) ind$fit))  #MIN / MAX
})

plot(0:(generations - 1), best_fitness, type = "l",
     xlab = "Generation", ylab = "Best Fitness",
     main = "Convergence Curve (F2)", col = "blue", lwd = 2)
