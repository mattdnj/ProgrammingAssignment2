## makeCacheMatrix and cacheSolve work together to create an object to store a matrix
## and cache its inverse. This prevents having to recalculate the inverse every time
## you run the function.

## This function stores the variable and the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


##This function solves the inverse of the function if it has not yet been calculated or it 
##will retrieve the cached inverse if it has already been calculated 

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
