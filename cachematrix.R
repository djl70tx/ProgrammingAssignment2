## R Programming Homework 2
## This file contains two function definitions that together provide a mechanism
## for caching the results of a potentially resource-intensive computation of the
## inverse of a given matrix alongside the original matrix itself.
##
## Passing an invertible matrix to the "makeCacheMatrix" function will create a list
## object with a new environment containing four functions that provide setter/getter
## functionality for the incoming matrix and its inverse when calculated.
##
## The "cacheSolve" function may then be used to consume one of these lists to retrieve
## the inverse of the matrix if it has already been calculated and cached in that list's
## environment, or to calculate and store the inverse in the list before returning it if
## there is nothing in the cache.

## makeCacheMatrix
## This function returns a list of four functions that provide getter/setter access
## to an environment where an invertible matrix and its inverse may be stored. Initially
## there is a NULL value provided for the inverted matrix. This function takes an
## invertible matrix as input.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverted <- function(inverted_matrix) im <<- inverted_matrix
    getinverted <- function() im
    list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}


## cacheSolve
## This function returns the inverse of the matrix stored in a cachematrix list object as
## output from the "makeCacheMatrix" function above. It checks whether or not the inverted
## matrix is present in the cache: if so, it is returned as-is; if not, this function
## calculates the inverse matrix and calls one of the setter functions to store it in the
## cachematrix list object's environment before returning the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverted()
    if (!is.null(im)) {
        message("Retrieved inverted matrix from cache.")
        return(im)
    }
    stored_matrix <- x$get()
    im <- solve(stored_matrix, ...)
    x$setinverted(im)
    im
}
