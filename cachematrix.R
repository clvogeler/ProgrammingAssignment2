##Code to calculate the inverse of matrix. Results are cached and if called
##for later the cached results are accessed as opposed to the results
##being calculated again.


##`makecacheMatrix` stores matrix, calucalutes it inverse, and caches the
##inverse.  It creates a list of functions to:
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }       
        get <- function() x     
        setinv <- function(solve) m <<- solve  
        getinv <- function() m
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)             
}


##cacheSolve calculates the inverse of the special matrix
##created with makeCacheMatrix. However, it first checks to see if the
##inverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the `setinv`
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inv(data, ...)
        x$setinv(m)
        m       
}
