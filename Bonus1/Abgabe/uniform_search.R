library(xega)

source("Abgabe/PropEnvs.R")

#Uniform Random Search

uniform_random_search <- function(fenv, iterations = 10000) {
  lb <- fenv$lb()
  ub <- fenv$ub()
  dim <- length(lb)
  
  best_val <- Inf
  best_point <- NULL
  
  for (i in 1:iterations) {
    point <- runif(dim, min = lb, max = ub)
    val <- fenv$f(point)
    if (val < best_val) {
      best_val <- val
      best_point <- point
    }
  }
  
  return(list(best_val = best_val, best_point = best_point))
}


f1 <- F1(2)
urs_result_f1 <- uniform_random_search(f1)

f2 <- F2(4)
urs_result_f2 <- uniform_random_search(f2)

f3 <- F3(4)
urs_result_f3 <- uniform_random_search(f3)

f4 <- F4(2)
urs_result_f4 <- uniform_random_search(f4)

f5 <- F5(2)
urs_result_f5 <- uniform_random_search(f5)

print(urs_result_f1)
print(urs_result_f2)
print(urs_result_f3)
print(urs_result_f4)
print(urs_result_f5)


