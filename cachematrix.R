# The makeCacheMatrix function creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


# The cacheSolve function returns the inverse of the matrix returned by the makeCacheMatrix function.
# If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
# If not, cacheSolve computes and returns the inverted matrix.

cacheSolve <- function(x, ...) {
       inverse <- x$getinverse()
       if(!is.null(inverse)) {
              message("getting cached data")
              return(inverse)
       }
       data <- x$get()
       inverse <- solve(data)
       x$setinverse(inverse)
       inverse
}
