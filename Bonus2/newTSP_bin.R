newTSP_bin<-function(D, Cities=0, Name, Solution=NA, Path=NA)
{
  parm<-function(x){function() {return(x)}}	
  d<-dim(D)
  try (if (!length(d)==2) stop("n times n matrix expected"))
  try (if (!d[1]==d[2]) stop("n times n matrix expected"))
  # constant functions
  self<-list()
  self$name<-parm(Name)
  self$bitlength<-parm(rep(40,d[1]))
  self$genelength<-function() {sum(self$bitlength())}
  self$dist<-parm(D)
  if (identical(Cities,0)) {cit<-1:length(Path)} else {cit<-Cities}
  self$cities<-parm(cit)
  self$solution<-parm(Solution)
  self$path<-parm(Path)
  # f
  self$f<-function(permutation, gene=0, lF=0, tour=TRUE)
  { cost<-0
  l<-length(permutation)-1
  for (i in 1:l)
  { cost<- cost+self$dist()[permutation[i], permutation[i+1]]}
  if (tour==TRUE) {cost<-cost+self$dist()[permutation[l+1], permutation[1]]}
  return(cost)}
  # show. p is a path.
  self$show<-function(p)
  { l<-length(p)-1
  pl<-0
  for (i in 1:l)
  {d<-self$dist()[p[i], p[i+1]]
  pl<-pl+d
  cat(i,"Length:", pl, 
      " from:", self$cities()[p[i]], " to ", self$cities()[p[i+1]],
      " Distance: ", d, "\n")}
  d<-self$dist()[p[l+1], p[1]]
  pl<-pl+d
  cat(i,"Length:", pl, 
      " from:", self$cities()[p[l+1]], " to ", self$cities()[p[1]],
      " Distance: ", d, "\n")}
  a<-force(self$name())
  a<-force(self$bitlength())
  a<-force(self$genelength())
  a<-force(self$dist())
  a<-force(self$cities())
  a<-force(self$solution())
  a<-force(self$path())
  return(self) }
