##Together, these functions are a testbed through which a matrix can be stored
##and it's inverse can be stored as well


## The first function is a storage object that creates a list of four functions
## These four functions together allow one to interact with a matrix
## and store its inverse once calculated

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## If the value is stored already in the makeCacheMatrix object x, then 
## that value is returned
## otherwise, the inverse is computed, stored in object x, and
## then returned

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
