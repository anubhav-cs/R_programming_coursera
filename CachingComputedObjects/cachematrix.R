#################################################################################
#                                                                              #
# R Programming Assignment : Coursera                                          #
#                                                                              #
# Problem:  Caching computing objects(vector/matrix) in R                      #
#                                                                              #
# Author:   Anubhav Singh                                                      #
#                                                                              #
# References:                                                                  #
#   1. https://www.coursera.org/learn/r-programming/peer/                      #
#       tNy8H/programming-assignment-2-lexical-scoping                         #                              #
#                                                                              #
################################################################################

## Functions to reduce the overhead of re-calculating inverse of a matrix
## everytime, by maintaining a cache of the previously computed matrix inverse.

## Creates a cached matrix, which essentially contains a list of functions to:
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the matrix-inverse
##     4. get the value of the matrix-inverse

makeCacheMatrix <- function(x = matrix()) {

    inverse <-  NULL

    set     <-  function(y){
        x       <<- y
        inverse <<- NULL
    }
    get     <-  function() x
    setinverse  <-  function(inv) inverse <<- inv
    getinverse  <-  function() inverse

    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Calculates matrix-inverse, but if it was already computed before, then
## it retrieves the cached value and returns that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        # attempted to get cached data
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        # if no cached data, then get matrix and calculate its inverse
        data    <-  x$get()
        inverse <-  solve(data)
        # cache the matrix-inverse
        x$setinverse(inverse)
        inverse
}
