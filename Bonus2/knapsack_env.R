knapsack_env_factory<-function(v, w, vol, n){
  self<-list()
  values <- v
  weights <- w
  volume <- vol
  self<-c(self, name=function() {"Knapsack problem"})
  self<-c(self, bitlength=function() {rep(1, n)}) # all binary decision variables
  self<-c(self, genelength=function() {sum(self$bitlength())})
  self<-c(self, lb=function() {rep(0, n)}) # all binary decision variables
  self<-c(self, ub=function() {rep(1, n)}) # all binary decision variables
  self$obj<-function(param) {t(param) %*% values}
  
  self$c_weight<-function(param) {250 - t(param) %*% weights}
  self$c_volume<-function(param) {250 - t(param) %*% volume}
  self$pen<-function() {10000}
  self$f<-function(param, gene=0, lF=0) 
  {
    goal<-self$obj(param)
    p1<-min(self$c_weight(param),0)*self$pen() # penalty for violation of weight constraint
    p2<-min(self$c_volume(param),0)*self$pen() # penalty for violation of volume constraint
    r<-goal+p1+p2
    return(r)
  }
  return(self)
}