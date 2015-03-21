# Two functions are defined here to compute the inverse of a matrix in a way 
# to save computations if repeated evaluations are expected.
#
# makeCacheMatrix(), creates a special list of functions to set the value of a 
# matrix, get the value of the matrix, set the value of the inverse and get 
# the value of the inverse.
#
# cacheSolve(x), takes the list defined with makeCacheMatrix() with a value of
# the matrix assigned with the set function, and returns the inverse. The 
# inverse is only computed the first time the function is called. Sucessive 
# calls use a cached value of the inverse, saving computations.
#
# Usage example:
#
# > x <- makeCacheMatrix(x=matrix(c(1/2,0,0,1/2), nrow=2, ncol=2))
# > cacheSolve(x)
#
# The solution can be verified with
#
# > x$get() %*% x$getinv()
#
# that should return the identity matrix of appropriate dimensions 
#

# makeCacheMatrix returns a list of functions to
# 1.- set the value of a matrix (list$set)
# 2.- get the value of a matrix (list$get)
# 3.- set the inverse of a matrix (list$setinv)
# 4.- get the inverse of a matrix (list$getinv)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

# cacheSolve returns the inverse of the matrix stored in the list x, which is 
# defined with makeCacheMatrix.
# The value of the inverse is only computed the first time the function is 
# called. Succesive calls use a cached value saved during the first call.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
