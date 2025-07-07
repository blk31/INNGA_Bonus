library(xega)

numberOfNNParms<-function(topology)
{
  n<-0
  l<-length(topology)-1
  for (j in (1:l)) {n<-n+(1+topology[j])*topology[j+1]}
  return(n)
}


#' 
rndParms<-function(n, lb=-1, ub=1)
{   return(lb+runif(n)*(ub-lb)) }


ReLU<-function(z)
{
  r<-z
  r[r<0]<-0
  return(r)
}

printNNSolution<-function(parm)
{
  ni<-3; nh<-5; no<-2
  npl1 <- (ni + 1) * nh 
  W1<-matrix(parm[(1:npl1)], nrow=(ni+1), ncol=nh, byrow=TRUE)
  cat("L1 (b|W)^T: \n")
  print(W1) 
  W2<-matrix(parm[npl1+(1:(length(parm)-npl1))], ncol=no, byrow=TRUE)
  cat("L2 (b|W)^T: \n")
  print(W2) 
}

#===================================Implementation of NN for Full Adder=============================================================================


adderData <- matrix(c(
  0, 0, 0, 0, 0,
  0, 0, 1, 1, 0,
  0, 1, 0, 1, 0,
  0, 1, 1, 0, 1,
  1, 0, 0, 1, 0,
  1, 0, 1, 0, 1,
  1, 1, 0, 0, 1,
  1, 1, 1, 1, 1
), nrow = 8, byrow = TRUE)

numberOfNNParms<-function(topology)
{
  n<-0
  l<-length(topology)-1
  for (j in (1:l)) {n<-n+(1+topology[j])*topology[j+1]}
  return(n)
}

NNAdder352 <- function(parm, data, sigmoid = TRUE) {
  ni <- 3
  nh <- 5
  no <- 2
  i <- cbind(1, data[, 1:3]) # input + bias
  npl1 <- (ni + 1) * nh
  W1 <- matrix(parm[1:npl1], nrow = ni + 1, ncol = nh, byrow = TRUE)
  L1 <- i %*% W1
  OL1 <- ReLU(L1)
  
  i2 <- cbind(1, OL1) # hidden + bias
  W2 <- matrix(parm[(npl1 + 1):length(parm)], nrow = nh + 1, ncol = no, byrow = TRUE)
  L2 <- i2 %*% W2
  
  if (sigmoid) {
    return(1 / (1 + exp(-L2)))  # probabilities for classification
  } else {
    return(L2)  # raw output for regression (R²)
  }
}


RsquareAdder <- function(res, data) {
  sum((res - data[, 4:5])^2)
}

envAdderNN <- function() {
  self <- list()
  self$name <- function() {"AdderNN352"}
  self$bitlength <- function() {rep(10, numberOfNNParms(c(3, 5, 2)))} #hab hier auch mal bitlength 10 angenommen wie in XOR, passt das? 
  self$genelength <- function() {sum(self$bitlength())}
  self$lb <- function() {rep(-1, numberOfNNParms(c(3, 5, 2)))}
  self$ub <- function() {rep(1, numberOfNNParms(c(3, 5, 2)))}
  self$f <- function(parm, gene=0, lF=0) {
    RsquareAdder(NNAdder352(parm, adderData, sigmoid=TRUE), adderData)
  }
  # early stopping condition 
  self$terminate <- function(solution) {
    pred <- NNAdder352(solution$phenotype, adderData) > 0.5
    actual <- adderData[, 4:5]
    return(all(pred == actual))
  }
  return(self)
}

p <- envAdderNN()
t1 <- xegaRun(penv = p, generations = 1000, popsize = 250, 
              evalmethod = "Deterministic", max = FALSE, verbose = 2)

# Print solutions
printNNSolution(t1$solution$phenotype)

s<-NNAdder352(t1$solution$phenotype, adderData)
cat("The activation values for the xor data set:\n")
print(s)

cat("Fitness:", t1$solution$fitness, "Better than 0.5? \n")
s1<-NNAdder352(t1$solution$phenotype, adderData)>0.5
print(s1)

#===================================NNs with negative log-likelihood loss function =============================================================================

# ------------Full Adder-----------

NLLLoss <- function(pred, data) {
  y <- data[, 4:5]
  epsilon <- 1e-10 # avoid log(0)
  -sum(y * log(pred + epsilon) + (1 - y) * log(1 - pred + epsilon))
}


envAdderNN_logloss <- function() {
  self <- list()
  self$name <- function() {"AdderNN352"}
  self$bitlength <- function() {rep(10, numberOfNNParms(c(3, 5, 2)))} #hab hier auch mal bitlength 10 angenommen wie in XOR, passt das? 
  self$genelength <- function() {sum(self$bitlength())}
  self$lb <- function() {rep(-1, numberOfNNParms(c(3, 5, 2)))}
  self$ub <- function() {rep(1, numberOfNNParms(c(3, 5, 2)))}
  self$f <- function(parm, gene=0, lF=0) {
    NLLLoss(NNAdder352(parm, adderData, sigmoid=TRUE), adderData)
  }
  # early stopping condition 
  self$terminate <- function(solution) {
    pred <- NNAdder352(solution$phenotype, adderData) > 0.5
    actual <- adderData[, 4:5]
    return(all(pred == actual))
  }
  return(self)
}

p_logloss<- envAdderNN_logloss()
t1_logloss <- xegaRun(penv = p_logloss, generations = 1000, popsize = 100, 
              evalmethod = "Deterministic", max = FALSE, verbose = 2)

printNNSolution(t1_logloss$solution$phenotype)

s<-NNAdder352(t1_logloss$solution$phenotype, adderData)
cat("The activation values for the xor data set:\n")
print(s)

cat("Fitness:", t1_logloss$solution$fitness, "Better than 0.5? \n")
s1<-NNAdder352(t1_logloss$solution$phenotype, adderData)>0.5
print(s1)

# ------------XOR-----------

NLLLoss <- function(pred, data) {
  y <- data[, 3]
  epsilon <- 1e-10
  -sum(y * log(pred + epsilon) + (1 - y) * log(1 - pred + epsilon))
}


xorData<-matrix(c(0, 0, 0,
                  0, 1, 1, 
                  1, 0, 1,
                  1, 1, 0), 
                nrow=4, ncol=3, byrow=TRUE)


parm<-c(0, -1, 0, 1, 1, 1, 1, 1, 1, 0, 1, -2, 0) 

sigmoid <- function(x) { 1 / (1 + exp(-x)) }

NNXOR231<-function(parm, data)
{
  # number of input (ni), hidden (nh), output (no) nodes
  ni<-2	
  nh<-3
  no<-1  
  # input data with constants row.
  i<-cbind(matrix(1, nrow=nrow(data), ncol=1), data[, c(1, 2)])
  # number of parameters for the combined weigths bias matrix.
  npl1<-((ni*nh)+nh)
  W1<-matrix(parm[(1:npl1)], nrow=(ni+1), ncol=nh, byrow=TRUE)
  L1<-i%*%W1
  OL1<-ReLU(L1)
  # output layer with constants row.
  i2<-cbind(matrix(1, nrow=nrow(OL1), ncol=1), OL1)
  W2<-matrix(parm[npl1+(1:(length(parm)-npl1))], ncol=no, byrow=TRUE)
  L2<-i2%*%W2 
  return(sigmoid(L2))
}


printNNSolution<-function(parm)
{
  ni<-2; nh<-3; no<-1
  npl1<-(1+2)*3
  W1<-matrix(parm[(1:npl1)], nrow=(ni+1), ncol=nh, byrow=TRUE)
  cat("L1 (b|W)^T: \n")
  print(W1) 
  W2<-matrix(parm[npl1+(1:(length(parm)-npl1))], ncol=no, byrow=TRUE)
  cat("L2 (b|W)^T: \n")
  print(W2) 
}

#### The GA as a solver.

#' Generate a problem environment for the XOR problem.
envXORNN231_logloss<-function()
{
  self<-list()
  self$name<-function() {"XORNN231"}
  self$bitlength<-function() {rep(10, 13)}
  self$genelength<-function() {sum(self$bitlength())}
  self$lb<-function() {rep(-1, 13)}
  self$ub<-function() {rep(1, 13)}
  self$f<-function(parm, gene=0, lF=0)
  {
    NLLLoss(NNXOR231(parm, xorData), xorData)
  }
  self$terminate<-function(solution)
  {
    #cat("Testing early termination ...\n")
    s1<-NNXOR231(solution$phenotype, xorData)>0.5
    if (4==sum(s1==xorData[,3])) {return(TRUE)} else {return(FALSE)}
  }
  return(self)
}

# Build problem environment
p_xor_logloss<-envXORNN231_logloss()

# Run GA
t1_xor_logloss<-xegaRun(penv=p_xor_logloss, generations=500, popsize=100, 
            evalmethod="Deterministic",  
            max=FALSE, verbose=2)

# Print solutions
printNNSolution(t1_xor_logloss$solution$phenotype)

s<-NNXOR231(t1_xor_logloss$solution$phenotype, xorData)
cat("The activation values for the xor data set:\n")
print(s)

cat("Fitness:", t1_xor_logloss$solution$fitness, "Better than 0.5? \n")
s1<-NNXOR231(t1_xor_logloss$solution$phenotype, xorData)>0.5
print(s1)

##===================================Topology  =============================================================================

# Full Adder more complex problem. Theoretically two hidden layers would be better because then you could model sum as two consecutive XOR functions
# first checking that sum is only one if one or three inputs are one. To keep to the implemantation we have added a lot more neurons to adjust for the complexity
# Unfortunately even with top = (3, 25, 2) we could not get a perfect result. The hardest to get is the case when all 3 inputs are 1.
# Everything else works fine

##===================================Compare the resullts =============================================================================



# XOR mit negativem Log-Likelihood (NLL):
# Vorhersagen:
# [1,] FALSE  → 0
# [2,]  TRUE  → 1
# [3,]  TRUE  → 1
# [4,] FALSE  → 0
#
# Alle 4 Vorhersagen stimmen exakt mit der XOR-Wahrheitstabelle überein. Das Netzwerk hat die XOR-Logik erfolgreich gelernt. => Perfekte Klassifikation

# Bewertung von R2
# Vorhersagen:
# [1,] FALSE FALSE
# [2,]  TRUE FALSE
# [3,]  TRUE FALSE
# [4,] FALSE  TRUE
# [5,]  TRUE FALSE
# [6,] FALSE  TRUE
# [7,] FALSE  TRUE
# [8,] FALSE  TRUE
#
#
# - 6 von 8 Vorhersagen korrekt
# - Fehler bei Zeile 7 (Summe falsch) und evtl. Zeile 8 (Summe falsch)


#  Bewertung neg Log-Likelihood
# Vorhersagen:
# [1,] FALSE FALSE
# [2,]  TRUE FALSE
# [3,]  TRUE FALSE
# [4,] FALSE  TRUE
# [5,]  TRUE FALSE
# [6,] FALSE  TRUE
# [7,]  TRUE  TRUE
# [8,] FALSE  TRUE
#
# - 7 von 8 Vorhersagen korrekt
# - Fehler bei Zeile 8 (Summe falsch)
#
# Zusammenfassung:
# - XOR wird perfekt gelöst, egal ob mit R² oder NLL.
# - R2 → 6/8 korrekt
# - NLL → 7/8 korrekt
# - NLL Klassifiziert hier besser als R2. Full adder ist komplexer als XOR, weswegen hier ein Unterschied deutlich wird
# ----------------------------------------------------------

