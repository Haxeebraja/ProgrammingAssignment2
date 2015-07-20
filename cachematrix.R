## This function takes a matrix as its arguments and set it in a variable in different environment using <<- operator.
## This function also has methods to set and get matrix and its inverse.
## This function returns a special vector that is actually a list with functions to set and get matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-  y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function uses makeCacheMatrix and checks whether an inverse was calculated before for the given matrix.
## If inverse was already calculated before that cached inverse is return. Else if inverse was not calculated before
## this will calculate the inverse and set it in cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
