## A pair of functions that cache the inverse of a matrix.

## Computing the inverse of a square matrix can be done with the solve function in R. 
## If X is a square invertible matrix, then solve(X) returns its inverse. 
## For this example, assume that the matrix supplied is always invertible.


## 1. makeCacheMatrix:  This function creates a special "matrix" object that  
##                      can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL  #the cache is initially empty
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcache <- function(cache) m <<- cache
        getcache <- function() m
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## 2.cacheSolve:        This function computes the inverse of the special 
##                      "matrix" returned by makeCacheMatrix above. If the 
##                      inverse has already been calculated (and the matrix has 
##                      not changed), then the cachesolve should retrieve the 
##                      inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getcache()  ##see if there is anything in the cache, 
                           ##if there is then return the message and the value 
                           ##of 'm'via the if code section below
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        cachematrix <- x$get()          ##If the cache is empty then get the matrix 'x'
        
        m <- solve(cachematrix, ...)    ##Return a matrix that is the inverse of 'x', 
                                        ##set to the variable 'm'
        
        x$setcache(m)                   ##Set the matrix that is the inverse of 'x' 
                                        ##'m' to the cache
                                        
        m                               ##Return 'm' to the console
}
