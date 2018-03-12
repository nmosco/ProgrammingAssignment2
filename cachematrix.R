# Creates a "cacheble matrix" object that's capable of caching it's own inverse
# Provides conveniece functions to get and set values

makeCacheMatrix <- function(originalMatrix = matrix()) {
    inverseMatrix <- NULL
    set <- function(newMatrix) {
        originalMatrix <<- newMatrix
        inverseMatrix <<- NULL
    }
    get <- function() originalMatrix
    setinverse <- function(the_inverse) inverseMatrix <<- the_inverse
    getinverse <- function() inverseMatrix
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

# Returns the cached value for the inverted matrix (if exists).
# Otherwise It Calculates the inverse of the matrix and saves it to the cache.

cacheSolve <- function(originalCacheMatrix) {
    # Gets the cached value for the inverse of the matrix from the cached matrix object.
    inverseMatrix <- originalCacheMatrix$getinverse()

    # If it exists, return the cached value.
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    # If it doesn't exist, calculate the value, save it to the cache and return the value.
    originalMatrix <- originalCacheMatrix$get()
    inverse <- solve(originalMatrix)
    originalCacheMatrix$setinverse(inverse)
    inverse
}