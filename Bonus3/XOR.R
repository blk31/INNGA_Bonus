library(xega)

# Data for the xor problem.
xorData<-matrix(c(0, 0, 0,
                  0, 1, 1, 
                  1, 0, 1,
                  1, 1, 0), 
                nrow=4, ncol=3, byrow=TRUE)

# An (optimal) parameter set for solving the xor problem with a (2, 3, 1) NN.
parm<-c(0, -1, 0, 1, 1, 1, 1, 1, 1, 0, 1, -2, 0) 

# topology vector, which defines the network structure
top<-c(2, 3, 1)

#' Computes the number of parameters of a NN for a given topology vector.
#'
#' @param topology   an integer vector. The i-th element defined the 
#'                   number of nodes of the i-th layer.
#' @result The number of parameters.
#'
#' @examples
#'   numberOfNNParms(c(2, 3, 1))
numberOfNNParms<-function(topology)
{
  n<-0
  l<-length(topology)-1
  for (j in (1:l)) {n<-n+(1+topology[j])*topology[j+1]}
  return(n)
}

#' Generates a random vector of length n in an n-dimensional hypercube
#'
#' @param  n     number of elements
#' @param  lb    lower bound
#' @param  ub    upper bound
#' @result A random vector of length n with elements in [lb, ub].
#' @examples
#' rndParms(numberOfNNParms(c(2, 3, 1)))
#' 
rndParms<-function(n, lb=-1, ub=1)
{   return(lb+runif(n)*(ub-lb)) }

#' Activation function of a rectified linear unit.
#'
#' @param z   a (real) matrix or vector.
#'
#' @result   In r is identical to z except for the negative elements of z.
#'           These are set to 0.
#' @examples
#'  
#' a<-rndParms(numberOfNNParms(c(2, 3, 1)))
#' a
#' ReLU(a)
ReLU<-function(z)
{
  r<-z
  r[r<0]<-0
  return(r)
}

#' A neural network with FIXED topology (2, 3, 1).
#' 
#' @param  parm  a parameter vector of length numberOfNNParms(c(2, 3, 1))
#' @parm   data  the data set e.g. for xor.
#'
#' @result a vector of activation values of the output layer.
#'
#' @examples 
#' NNXOR231(rndParm(13), xorData)
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
  return(L2)
}

# Squared error
RsquareNN<-function(res, data)
{
  sum((res-data[,3])^2)
}

#' Print the parameter vector of a NN with topology c(2, 3, 1) 
#'
#' @param parm   a parameter vector of length 13.
printNNSolution231<-function(parm)
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
envXORNN231<-function()
{
  self<-list()
  self$name<-function() {"XORNN231"}
  self$bitlength<-function() {rep(10, 13)}
  self$genelength<-function() {sum(self$bitlength())}
  self$lb<-function() {rep(-1, 13)}
  self$ub<-function() {rep(1, 13)}
  self$f<-function(parm, gene=0, lF=0)
  {
    RsquareNN(NNXOR231(parm, xorData), xorData)
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
p<-envXORNN231()

# Run GA
t1<-xegaRun(penv=p, generations=500, popsize=100, 
           evalmethod="Deterministic",  
           max=FALSE, verbose=2)

# Print solutions
printNNSolution231(t1$solution$phenotype)

s<-NNXOR231(t1$solution$phenotype, xorData)
cat("The activation values for the xor data set:\n")
print(s)

cat("Fitness:", t1$solution$fitness, "Better than 0.5? \n")
s1<-NNXOR231(t1$solution$phenotype, xorData)>0.5
print(s1)
