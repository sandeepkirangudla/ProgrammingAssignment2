## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The below function caches the inverse of matrix, if already not present.
makeCacheMatrix <- function(x = matrix()) {
    mat.inv <- NULL
    set <- function(y) {
        x <<- y
        mat.inv <<- NULL
    }
    get <- function() x
    # solve function results the inverse of a matrix
    setinv <- function(solve) mat.inv <<- solve
    getinv <- function() mat.inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## The below function results the inverse of a matrix if not present already
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat.inv <- x$getinv()
    if(!is.null(mat.inv)) {
        message("getting cached data")
        return(mat.inv)
    }
    data <- x$get()
    mat.inv <- solve(data, ...)
    x$setinv(mat.inv)
    mat.inv
}
