LagEnvFactory<-function(){
  self<-list()
  self$name<-function() {"Benchmark B10"}
  self$optimize<-function() {"Min"}
  self$bitlength<-function() {rep(64,8)}
  self$genelength<-function() {sum(self$bitlength())}
  self$lb<-function() {10*c(10, 100, 100, 1, 1, 1, 1, 1)}
  self$ub<-function() {1000*c(10, 10, 10, 1, 1, 1, 1, 1)}
  self$obj<-function(x) {x[1] + x[2] + x[3]}
  self$c1<-function(x) {-1 + 0.0025*(x[4]+x[6])}
  self$c2<-function(x) {-1 + 0.0025*(-x[4]+x[5]+x[7])}
  self$c3<-function(x) {-1 + 0.01*(-x[5]+x[8])}
  self$c4<-function(x) {100*x[1] - x[1]*x[6] + 833.33252*x[4] - 83333.333}
  self$c5<-function(x) {x[2]*x[4] - x[2]*x[7] - 1250*x[4] + 1250*x[5]}
  self$c6<-function(x) {x[3]*x[5] - x[3]*x[8] - 2500*x[5] + 1250000}
  
  # Penalty
  self$pen<-function() {10000}
  
  ### Lagrange function
  self$f<-function(parm, gene=0, lF=0) 
  {
    x<-parm
    goal<-self$obj(x)
    p1<-max(self$c1(x),0)*self$pen() # penalty for violation of constraint 1
    p2<-max(self$c2(x),0)*self$pen() # penalty for violation of constraint 2
    p3<-max(self$c3(x),0)*self$pen() # penalty for violation of constraint 3
    p4<-max(self$c4(x),0)*self$pen() # penalty for violation of constraint 4
    p5<-max(self$c5(x),0)*self$pen() # penalty for violation of constraint 5
    p6<-max(self$c6(x),0)*self$pen() # penalty for violation of constraint 6
    r<-goal+p1+p2+p3+p4+p5+p6
    return(r)
  }
  # diagnostics for identifying violations
  self$fdiag<-function(x)
  {
    # diagnostic block to find violated constraints
    cat("x   :", x, "\n")
    cat("obj :", self$obj(x), "\n")
    cat("pobj:", self$f(x), "\n")
    cat("c1  :", self$c1(x), "\n")
    cat("pc1 :", (max(self$c1(x),0)*self$pen()), "\n")
    cat("c2  :", self$c2(x), "\n")
    cat("pc2 :", (max(self$c2(x),0)*self$pen()), "\n")
    cat("c3  :", self$c2(x), "\n")
    cat("pc3 :", (max(self$c2(x),0)*self$pen()), "\n")
    cat("c4  :", self$c2(x), "\n")
    cat("pc4 :", (max(self$c2(x),0)*self$pen()), "\n")
    cat("c5  :", self$c2(x), "\n")
    cat("pc5 :", (max(self$c2(x),0)*self$pen()), "\n")
    cat("c6  :", self$c2(x), "\n")
    cat("pc6 :", (max(self$c2(x),0)*self$pen()), "\n")
    return(self$f(x))}
  
  # Optimal solution
  self$solution<-function(){
    s<-list()
    s$minimum<-7049.3307
    s$minpoints<-list(list(c(579.3167, 1359.943, 5110.071, 182.0174, 295.5985, 217.9799, 286.4162, 395.5979)))
    return(s)
  }
  return(self)
}