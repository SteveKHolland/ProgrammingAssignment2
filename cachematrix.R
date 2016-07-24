## The following functions allow users to cache potentially time-consuming
## computations when solving for the inverse of a matrix.


## The function below creates a special matrix object that can cache its
## inverse.


makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL                   ## "i" stands for "inverse"
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve function should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {

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
