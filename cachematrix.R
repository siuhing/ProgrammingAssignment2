##===================================================================================================
##
##  This program contains two functions: makeCacheMatrix and cacheSolve. The functions read in
##  a matrix and return its inverse. The functions would first check if the inverse has already
##  been calculated and stored in cache. If so, the cached value of the inverse would be 
##  returned. Otherwise, if the inverse does not exist, it would be calculated, cached, and 
##  returned.
##
##===================================================================================================
##
##  Function:  makeCacheMatrix
##  Usage:     This function creates a list that can cache and retrieve a matrix and its inverse.
##  Argument:  x: a matrix object
##  Output:    This function returns a list that contains the following objects:
##                 set  - cache the value of the input matrix
##                 get  - retrieve the value of the input matrix
##                 setinverse - cache the value of the inverse matrix
##                 getinverse - retrieve the value of the inverse matrix
##
   
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             # Create a variable inv and initialize its value to NULL

        set <- function(y) {
                x <<- y         # assign value to a variable x in the containing environment
                inv <<- NULL    # initialize the inverse matrix to NULL in the containing environment
        }

        # retreive the value of the variable x in the containing environment
        get <- function() x     

        # assign value to the variable inv in the containing environment
        setinverse <- function(solve) inv <<- solve  

        # retrieve the value of the variable inv in the containing environment
        getinverse <- function() inv     

        # return the list of 4 objects: set, get, setinverse, and getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##
##  Function:  cacheSolve 
##  Usage:     This function outputs the inverse of the object returned by makeCacheMatrix above. If the inverse 
##             already exists in cache, then cacheSolve would return the inverse from cache. Otherwise, the
##             inverse would be calculated, cached and returned.
##  Argument:  x: an object returned by makeCacheMatrix
##  Output:    The inverse of the argument
##


cacheSolve <- function(x, ...) {

        # retrieve the inverse from the containing environment
        inv <- x$getinverse()

        # If the inverse already exists, prints a message and returns the inverse.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # If the inverse does not exist, retrieve the input matrix from the containing environment.
        data <- x$get()

        # calculate the inverse of the input matrix
        inv <- solve(data, ...)

        # store the inverse matrix to the variable inv in the containing environment
        x$setinverse(inv)

        # return the inverse matrix
        inv
}
