## The following two functions create a matrix and calculate the inverse of this matrix
## The inverse is stored in the cache such that the calculation only needs to be done once



## Creates a list containing a function to set and get the values 
## of a matrix and set and get the value of the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Calculate the inverse of the matrix created. However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache. Otherwise, it calculates the inverse of the matrix and stores it in the cache.

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


#test the code
#testmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
#testmatrix$get()
#cachesolve(testmatrix)
#cachesolve(testmatrix)


