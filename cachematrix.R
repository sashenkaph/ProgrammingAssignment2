## Caching/calculating the inverse of a square matrix assuming that 
## the matrix is invertible

## Enclosing environment for set/get functions to set/get initial matrix and 
## for setsolve/getsolve ones to set/get the inverse of an initial matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve_m) m <<- solve_m 
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Returns cached inversed matrix or
## calculates and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached inversed matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, diag(ncol(data)), ...)
    x$setsolve(m)
    m
}
