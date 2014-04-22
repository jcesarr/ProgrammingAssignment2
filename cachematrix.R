# Calculates the inverse of a matrix using a cache.

# This function returns a matrix that can be cached. 
# It contains operations to get the calculation and  rite a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	set <- function(y) {
	        x <<- y
	        i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv 
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse (solve) of a matrix. 
## If the calculation was performed earlier, then it 
## returns the value from cache. 

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        # If it has been calculated before, return it from cache.
        if (!is.null(i)) {
                return(i)
        }
        data <- x$get()
        # Calculate the inverse of the matrix
        i <- solve(data, ...)
        x$setinv(i)
        i
}
