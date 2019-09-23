## Put comments here that give an overall description of what your
## functions do

## The below function stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
         n <- NULL
         set <- function(o) {
           x <<- o
           n <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) n <<- inverse
         getinverse <- function() n
         list(set = set,
              get = get,
              setinverse = setinverse
              getinverse = getinverse
              )
}


## This function computes the Inverse of the Special matrix returned from the above function "makeCacheMatrix"
## If the Inverse is already calculated, then cacheSolve should get inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse
  if (!is.NUll(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
