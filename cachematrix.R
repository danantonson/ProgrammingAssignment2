## This is the code for Coursera Assignment 2  
## The goal is to cache a matrix with a function, and 'solve' the matrix with another function

## makeCacheMatrix creates the initial cache of the matrix. 

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This the function that 'solves' makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get ()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
