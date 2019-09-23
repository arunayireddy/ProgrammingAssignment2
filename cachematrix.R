## The below functions creates a special objects that stores a matrix and caches its inverse

## makeCacheMatrix creates a special matrix, which is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

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
              setinverse = setinverse,
              getinverse = getinverse
              )
}


## This function computes the Inverse of the Special matrix returned from the above function "makeCacheMatrix"
## If the Inverse is already calculated, then cacheSolve should get inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
