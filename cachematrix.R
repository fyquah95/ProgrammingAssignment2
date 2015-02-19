makeCacheMatrix <- function(mat = matrix()) {
    # makeCacheMatrix returns a list which allows a matrix inversion to be cached
    
    # inv is the variable that caches the inverse of the matrix
    # mat is the variable that stores the matrix itself
    inv <- NULL 
    
    set <- function(m) {
        # set : sets the matrix to a certain desired matrix and reset the inverse to null
        # reinitialize the inv variable iff the mat is different
        if(!isTRUE(all.equal(m, mat))) {
            mat <<- m
            inv <<- NULL 
        }
        mat
    }
    get <- function() {
        # get : returns the matrix
        mat
    }
    setinv <- function(m) {
        # setinv : sets the inverse 
        inv <<- m
    }
    getinv <- function() {
        # getinv : returns the inverse of the matrix
        inv
    }
    list(set= set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    # If the results is cached, the cached result will be returned, rather than recomputing the
    # inverse
    
    inv <- x$getinv()
    if (!is.null(inv) ) {
        message("Getting cached data")
        return(inv)
    }
    
    m = solve(x$get(), ...)
    x$setinv(m)
    m
}
