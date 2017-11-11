
## makeCacheMatrix takes an input matrix and returns a list of function
##cacheSolve function takes an input matrix and returns its inverse


## makeCacheMatrix creates a cache for an input matrix
## set  --> Sets the value of matrix
## get  --> Retrieves the value of x
## setinv --> sets the inverse of matrix for caching
## getinv --> Returns the cached value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function()
        x
    setinv <- function(inv)
        invx <<- inv
    getinv <- function()
        invx
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## cacheSolve takes a cached matrix object and returns its inverse. 
## It checks if inverse is already calculated and stored in the cache
## Returns cached inverse matrix if available
## else, calculates inverse, stores in cache and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(class(x$get())!="matrix")
    {
        print("Input is not a valid matrix!")
        return(-1)
    }
    if(ncol(x$get())!=nrow(x$get()))
    {
        print("Input matrix is not inversible. Input should be a square matrix!")
        return(-1)
    }
    invx <- x$getinv()
    if(!is.null(invx)) {
        message("getting cached data 1")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data)
    x$setinv(invx)
    invx
}
