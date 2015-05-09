## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #set the value of the vector
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get the value of the vector
    get <- function() x
    #set and get the value of the mean
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    # if the mean has already been calculated, gets the mean from the cache and skips the computation
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if mean hasn't already calculated, calculate mean
    data <- x$get()
    m    <- solve(data, ...)
    x$setinv(m)
    m
}
