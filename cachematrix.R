## Caching/calculating the inverse of a square matrix assuming that 
## the matrix is invertible

## Enclosing environment for set/get functions to set/get initial matrix and 
## for setsolve/getsolve ones to set/get the inverse of an initial matrix

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setsolve <- function(solve_m) inv <<- solve_m 
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Returns the inverse of a matrix 
## If the inverse of a matrix has not been stored in a cache yet then 
## the function calculates and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of initial one 'm'
    inv <- x$getsolve()
    if(!is.null(inv)){
        message("getting the inverse a matrix from the cache")
        return(inv)
    }
    m <- x$get()
##    inv <- solve(m, diag(ncol(m)), ...)
    inv <- solve(m, ...)
    x$setsolve(inv)
    inv
}
