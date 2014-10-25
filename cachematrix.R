# Programming Assignment 2
# Coursera - R Programming - Week 3
# 	Matrix inversion is usually a costly computation and there may be some benefit
# 	to caching the inverse of a matrix rather than compute it repeatedly. The
# 	following two functions are used to cache the inverse of a matrix.


# The makeCacheMatrix function creates a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of inverse of the matrix
# - get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix. 
# If the inverse has already been calculated, the function gets the result.
# In that case, the function does not have to compute the inverse. 
# If the inverse has not yet been calculated, the function computes the inverse, 
# and sets the value in the cache by calling the setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("- getting cached data....")
    } else {
		message("- no cached data - calculating inverse.....")
		data <- x$get()
		inv <- solve(data)
		x$setinverse(inv)
	}
    inv
}

