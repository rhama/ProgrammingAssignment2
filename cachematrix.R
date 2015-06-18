## Put comments here that give an overall description of what your
## functions do 

# makeCacheMatrix returns a list containing functions to 
# 1. set the value of a matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  minv<-NULL;
  set<-function(y) {
    x <<- y
    minv <<- NULL
  }
  get<-function() x
  setinverse<-function(inv) minv<<-inv
  getinverse<-function() minv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# cacheSolve returns the inverse of a given matrix
# This function checks if the inverse is already available in the cache
# If present in the cache, returns the inverse from the cache
# Else, computes the inverse by calling solve() and sets up the cache also

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getinverse()
      if (!is.null(inv)) {
        message("Getting data from cache")
        return(inv)
      }
      else message("Data not found in Cache")
      data <- x$get()
      inv<- solve(data)
      x$setinverse(inv)
      inv
  }

