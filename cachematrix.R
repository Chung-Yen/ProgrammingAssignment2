## It's a pair of functions that cache the inverse of a matrix 
## without computing it repeatly

## makeCacheMatrix  creates a special "matrix" which is really a list containing a function to
## 1.set the value of the metrix 2.get the value of the matrix ## 3.set the inverse of the matrix 4.get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheslovecalculates the inverse of the special "martrix" created with the above function 
## cache and skips the computation if it has already been calculated
## otherwise it calculate the inverse and set the value in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
