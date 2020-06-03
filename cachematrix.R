#Title- Caching the inverse of matrix in R

## This function takes input and make inverse

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
    
  }
  get<-function(){x}
  setInverse<- function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
# Cachesolve matrix first get data in cache memory and then turn it into Inverse.

cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
    
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}



