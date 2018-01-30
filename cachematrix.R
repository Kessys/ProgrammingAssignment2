## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Caching the Inverse of a Matrix:
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# The following function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
myMatrix$get()

# No cache in the first run
cacheSolve(myMatrix)

# Retrieving from the cache in the second run
cacheSolve(myMatrix)
