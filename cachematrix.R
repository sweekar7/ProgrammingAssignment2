

## This function caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL

  ## This function sets the value of special matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## This function returns the special matrix
  get<-function() x

  ## This function caches the inverse matrix calulated
  setinverse<-function(solve) m<<- solve

  ##This functiom returns the cached inverse of the matrix
  getinverse<-function() m
  list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)

}


## This function calculates the inverse of the input matrix

cacheSolve <- function(x, ...) {
m<-x$getinverse()
## Here we check if the inverse is already calculated or not
## If it is already calculated, then inverse is retrived from cache and returned, otherwise it is calculated in this function and stored in cache
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  ## Calculate inverse
  m<-solve(data, ...)
  ## Store inverse of matrix n cache
  x$setinverse(m) 
  m
        ## Return a matrix that is the inverse of 'x'
}
