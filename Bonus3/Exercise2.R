library(xega)

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

NNAdder352 <- function(parm, data) {
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
  
  # Use sigmoid output for probability
  return(1 / (1 + exp(-L2)))
}

RsquareAdder <- function(res, data) {
  sum((res - data[, 4:5])^2)
}

envAdderNN <- function() {
  self <- list()
  self$name <- function() {"AdderNN352"}
  self$bitlength <- function() {rep(10, numberOfNNParms(c(3, 5, 2)))}
  self$genelength <- function() {sum(self$bitlength())}
  self$lb <- function() {rep(-1, numberOfNNParms(c(3, 5, 2)))}
  self$ub <- function() {rep(1, numberOfNNParms(c(3, 5, 2)))}
  self$f <- function(parm, gene=0, lF=0) {
    RsquareAdder(NNAdder352(parm, adderData), adderData)
  }
  self$terminate <- function(solution) {
    pred <- NNAdder352(solution$phenotype, adderData) > 0.5
    actual <- adderData[, 4:5]
    return(all(pred == actual))
  }
  return(self)
}

p <- envAdderNN()
t1 <- xegaRun(penv = p, generations = 500, popsize = 100, 
              evalmethod = "Deterministic", max = FALSE, verbose = 2)

