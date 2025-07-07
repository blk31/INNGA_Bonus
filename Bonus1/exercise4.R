library(xega)
source("F.R")

f2 <- F2()
generations <- c(10, 100, 1000)
pop_size <- c(10, 100, 1000)

phenotypeValue <- matrix(nrow = 3, ncol = 3)
colnames(phenotypeValue) <- c(10, 100, 1000)
rownames(phenotypeValue) <- c(10, 100, 1000)

for (g in generations) {
    for (p in pop_size) {
        result <- xegaRun(penv=f2, algorithm="sga", max=FALSE, generations=g, popsize=p, verbose=0)
        phenotypeValue[as.character(g), as.character(p)] <- result$solution$phenotypeValue
    }
}

print(phenotypeValue)
