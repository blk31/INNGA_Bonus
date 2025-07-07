library(xega)
library(ggplot2)

source("PropEnvs.R")

# set pop and gen size 
pop_size = c(10,100,1000)
gen_size = c(10,100,1000)


# Define dataframes to store results 
results_min <- data.frame(
  pop_size = numeric(0),
  gen_size = numeric(0),
  best_fitness = numeric(0)
)

results_max <- data.frame(
  pop_size = numeric(0),
  gen_size = numeric(0),
  best_fitness = numeric(0)
)

# Loop through all combinations
for (p in pop_size) {
  for (g in gen_size) {
    cat("Running GA with population =", p, "and generations =", g, "\n")
    
    f2 <- F2(4)
    f2_max<- xegaRun(penv=f2, algorithm="sga", max=TRUE, generations=g, popsize=p, verbose=0)
    f2_min<- xegaRun(penv=f2, algorithm="sga", max=FALSE, generations=g, popsize=p, verbose=0)
    
    # Store the result
    results_max <- rbind(results_max, data.frame(
      pop_size = p,
      gen_size = g,
      best_fitness = f2_max$solution$phenotypeValue 
    ))
    
    results_min <- rbind(results_min, data.frame(
      pop_size = p,
      gen_size = g,
      best_fitness = f2_min$solution$phenotypeValue 
    ))
  }
}

results_min$gen_size <- as.numeric(as.character(results_min$gen_size))
results_min$pop_size <- as.factor(results_min$pop_size)  

# Plot
ggplot(results_min, aes(x = gen_size, y = best_fitness, color = pop_size, group = pop_size)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "PhenotypeValue vs. Generations for Different Population Sizes (F2, Min)",
    x = "Generation Size",
    y = "PhenotypeValue",
    color = "Population Size"
  ) +
  theme_minimal()



