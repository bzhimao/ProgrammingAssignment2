## makeCacheMatrix takes a matrix as an argument contains four sub-functions:
##     1. Sets the value of the matrix (with a function)
##     2. Gets the value of the matrix
##     3. Sets the value of the inverse matrix
##     4. Gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y) {
        x <<- y			# <<- operator allows setMatrix to work
        i <<- NULL		# outside this environment
    }
    getMatrix <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i

    # Return a list of functions 
    list(setMatrix = setMatrix, getMatrix = getMatrix,
	   setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the matrix, but check first to see 
## if it has already been calculated. If so, get the cached
## inverse matrix by calling makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()		# Access the list from makeCacheMatrix()
    if (!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }

    message("compute inverse for first time")
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setInverse(i) 		# Cache the inverse

    message("inverse matrix: ")
    i					# Return a matrix that is the inverse of 'x'
}
