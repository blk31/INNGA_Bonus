F1<-function(){
  self<-list()
  self<-c(self, name=function() {"F1"})
  self<-c(self, bitlength=function() {c(20, 20)})
  self<-c(self, genelength=function() {sum(self$bitlength())})
  self<-c(self, lb=function() {c(-100,-100)})
  self<-c(self, ub=function() {c(100, 100)})
  self<-c(self, f=function(param, gene=0, lF=0) {param[1]**2 + 2 * param[2]**2 -0.3*cos(3*pi*param[1]) - 0.4*cos(4*pi*param[2]) + 0.7})
  return(self)
}


F2<-function(){
  self<-list()
  self<-c(self, name=function() {"F2"})
  self<-c(self, bitlength=function() {c(20, 20, 20, 20)})
  self<-c(self, genelength=function() {sum(self$bitlength())})
  self<-c(self, lb=function() {c(-10,-10,-10,-10)})
  self<-c(self, ub=function() {c(10, 10, 10, 10)})
  self<-c(self, f=function(param, gene=0, lF=0) {(param[1]-2)**2 + sum(sapply(2:4, function(i) i * (2 * param[i]^2 - param[i-1])**2))})
  return(self)
}


F3<-function(){
  self<-list()
  self<-c(self, name=function() {"F3"})
  self<-c(self, bitlength=function() {c(20, 20, 20, 20)})
  self<-c(self, genelength=function() {sum(self$bitlength())})
  self<-c(self, lb=function() {c(-1,-1,-1,-1)})
  self<-c(self, ub=function() {c(1, 1, 1, 1)})
  self<-c(self, f=function(param, gene=0, lF=0) {(exp(-param[1])-param[2])**4 + 100*(param[2]-param[3])**6 + (tan(param[3]-param[4]))**4 + param[1]**8})
  return(self)
}


F4<-function(){
  self<-list()
  self<-c(self, name=function() {"F4"})
  self<-c(self, bitlength=function() {c(20, 20)})
  self<-c(self, genelength=function() {sum(self$bitlength())})
  self<-c(self, lb=function() {c(13,0)})
  self<-c(self, ub=function() {c(100, 100)})
  # self<-c(self, f=function(param, gene=0, lF=0) {(param[1]-10)**3 + (param[2]-20)**3})
  self$obj<-function(param) {(param[1]-10)**3 + (param[2]-20)**3}
  self$c1<-function(param) {-(param[1]-5)**2 - (param[2]-5)**2 + 100}
  self$c2<-function(param) {(param[1] - 6)^2 + (param[2] - 5)^2 - 82.81}
  self$pen<-function() {-1000000000000000000000}

  self$f<-function(param, gene=0, lF=0) 
  {
    goal<-self$obj(param)
    p1<-max(self$c1(param),0)*self$pen() # penalty for violation of constraint 1
    p2<-max(self$c2(param),0)*self$pen() # penalty for violation of constraint 2
    r<-goal+p1+p2
    return(r)
  }

  return(self)
}


F5<-function(){
  self<-list()
  self<-c(self, name=function() {"F5"})
  self<-c(self, bitlength=function() {c(20, 20)})
  self<-c(self, genelength=function() {sum(self$bitlength())})
  self<-c(self, lb=function() {c(0,0)})
  self<-c(self, ub=function() {c(10, 10)})
  self$obj<-function(param) {-((sin(2*pi*param[1]))**3 * sin(2*pi*param[2])) / (param[1]**3 * (param[1]+param[2]))}
  
  self$c1<-function(param) {param[1]**2 - param[2] + 1}
  self$c2<-function(param) {1 - param[1] + (param[2]-4)**2}
  self$pen<-function() {-1000000000000}
  self$f<-function(param, gene=0, lF=0)
  {
    goal<-self$obj(param)
    p1<-max(self$c1(param),0)*self$pen() # penalty for violation of constraint 1
    p2<-max(self$c2(param),0)*self$pen() # penalty for violation of constraint 2
    r<-goal+p1+p2
    return(r)
  }
  return(self)
}
