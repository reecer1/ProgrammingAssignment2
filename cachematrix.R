## This assignment is to create a cache for an object so that it does not have to be
## recalculated 

## The first function, makeCacheMatrix creates a special "vector", which is a list containing a function to
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse
##      get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrixInverse <<-inverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This functions solves the matrix inversion if it's not in the cache

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}

cat("\014")  ## clear Console
originalMatrix <- makeCacheMatrix(matrix(c(2,2,1,4),2,2))
originalMatrix$get()
cacheSolve(originalMatrix)
originalMatrix$getInverse()