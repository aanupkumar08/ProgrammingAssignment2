## Put comments here that give an overall description of what your
# Doing Matrix inversion is a costly computation and also some benefit
# By caching the inverse of a matrix in place of compute it repeatedly. 
# Below two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. setting the value of the matrix
# 2. getting the value of the matrix
# 3. setting the value of inverse of the matrix
# 4. getting the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, inverse=getinverse)
}


# Below mention function returns the inverse of the matrix. 
# This function first checks if the inverse has already been computed. 
# If computation happen, it gets the result and skips the computation.
# If computation does not happen, it computes the inverse, sets the value in the # cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
