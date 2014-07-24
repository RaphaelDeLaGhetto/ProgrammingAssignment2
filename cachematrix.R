## 
## The `makeCacheMatrix` and `cacheSolve` functions complement the `solve`
## function, which returns the inverse of an invertible matrix. By saving
## the value returned by `solve` on an unchanging matrix, we don't
## consume system resources needlessly.
##
## `makeCacheMatrix` provides an object to serve as the cache.
##
## `cacheSolve` caches the value returned by `solve`
## (in an object returned by `makeCacheMatrix`)
##

##
# Create an object to access and update a matrix
# and its inverted value
#
# @param matrix - this must be an invertible matrix
##
makeCacheMatrix <- function(m = matrix()) {

    # The cached inverted matrix
    inverse <- NULL

    #
    # Set the value of the matrix and clear
    # the old inverse value
    # 
    # @param matrix - the new matrix to assign to this object
    #
    set <- function(newm) {
        m <<- newm 
        inverse <<- NULL
    }

    #
    # Get the matrix currently stored in the `m` variable
    #
    # @returns matrix
    # 
    get <- function() m

    #
    # Set the value of inverse variable
    #
    # @param matrix
    #
    setinverse <- function(newinverse) inverse <<- newinverse

    #
    # Get the value currently stored in the `inverse` variable
    #
    # @returns matrix
    #
    getinverse <- function() inverse

    #
    # Return the functions necessary to access the matrix
    # and its inverted value
    #
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


##
# Find the inverse of a invertible matrix using `solve`
# and put the result into the cache
#
# @param cacheMatrix - as returned by `makeCacheMatrix`
##
cacheSolve <- function(cache, ...) {

    # Check the cache for an existing inverted matrix
    inverse <- cache$getinverse()

    # Return the cached value, if it exists
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # No cached value? Calculate the inverse of the
    # matrix stored in the cache
    data <- cache$get()
    inverse <- solve(data, ...)

    # Stash the inverse in cache and return
    cache$setinverse(inverse)
    inverse
}

