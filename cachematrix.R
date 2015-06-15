## Below are two functions that are used to create a special object that stores 
## a numeric matrix and cache's its inverse.


## The first function, makeCacheMatrix accepts a matrix as input argument and 
## outputs a list where each element is a function. There are four in total: 
## a) "set" sets the value of the matrix
## b) "get" gets the value of the matrix
## c) "setinverse" sets the value of the inverse
## d) "getinverse" gets the value of the inverse
## This function is very similar to the example "makeVector" function, 
## the argument now being a matrix instead of a vector.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y            ## The <<- operator is used in order to assign 
                m <<- NULL         ## the values of m outside the scope of the 
                                   ## "set" function
        }
        
        get <- function() x
        setinverse <- function(inversematrix) m <<- inversematrix ## same here
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the matrix passed to the 
## above function and returns it. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix and
## sets the value of the inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        ## if the inverse has already been calculated, return the cached data and 
        ## exit the function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## otherwise, get the data matrix, calculate the inverse, set it and 
        ## return it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
