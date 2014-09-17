## Overview
#
# Functions to calculate and cache the inverse of a square matrix assumed to be
# invertible. 
#
# makeCacheMatrix: creates a special "matrix" object that can store the inverse 
#                  of a matrix. 
#
# cacheSolve:      Computes the inverse of a matrix via the "solve()" function. 
#                  
#
# Notes: Need a robust way to check if the inverse of a matrix has changed 
# without recomputing the inverse ... Hm. 

## makeCacheMatrix:
#    Creates a "special" matrix object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize inverse as NULL (has not been solved yet)
        inv <- NULL
        
        # Assign Calculated INVerse (cinv) to inv. <<- assignment operator 
        # forces assignment in parent environment (makeCacheMatrix)
        #
        # Reminder to CWB: Parent dependencies are determined at function 
        # definition time rather than run time with lexical scoping. So the 
        # "inv" variable in setinv is equivalent to the inv variable
        # set in makecachematrix.
        setinv <- function(cinv) inv <<- cinv
        
        # Retrun the cached inverse
        getinv <- function() inv
        
        # Return matrix
        get <- function() x
        
        # Return matrix 
        # Create list containing the matrix and function objects
        #
        # R returns the last expression, so no need to call return
        list(get = get, 
             getinv = getinv, 
             setinv = setinv)

} # makeCacheMatrix


## cacheSolve
# Function to solve the inverse of a matrix if the inverse has not already been 
# cached. If the inverse HAS been cached, then use the cached solution.
#
# x: a cache matrix as defined by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        
        # Get cached inverse value
        inv <- x$getinv()
        
        # Is inverse cached? If so, return the cached inverse
        if(!is.null(inv)){
                message("Using cached inverse.")
                return(x$getinv())
        } # if(~is.null(inv))
        
        message("No cached inverse found. Solving and caching.")
        # Otherwise, solve and cache the inverse.
        x$setinv(solve(x$get()))
        
        # Return the inverse
        return(x$getinv()) 
        
}
