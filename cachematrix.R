## This file contains a pair of functions that cache the inverse
## of a matrix to save computation cycles during execution

## makeCacheMatrix
##
## - Creates a special 'matrix' object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
    matx <- NULL                              ## ensure the local inverse matrix is NULL
    set <- function(y) {                      ## set function:
        x <<- y                               ## - that sets the initial matrix in diff Env
        matx <<- NULL                         ## - and ensures the local inverse matrix is NULL in diff Env
    }
    get <- function() x                       ## return the initial matrix
    getInverse <- function() matx             ## return the inverse matrix
    setInverse <- function(inv) matx <<- inv  ## set the inverse matrix to whatever is passed in (from ext)
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## return 'special' matrix
                                                                                 ## of functions
}


## cacheSolve
##
## - Computes the inverse of the special 'matrix' returned by
##   makeCacheMatrix above, retrieving the inverse from cache
##   if it hasn't changed
##
## - Computing the inverse of a square matrix can be done with
##   the solve function in R, e.g. if X is a square invertible
##   matrix then solve(X) returns its inverse
##
## - Assumes the supplied matrix will always be invertible
##
cacheSolve <- function(x, ...) {
    matx <- x$getInverse()              ## query the x matrix cache
    if(!is.null(matx)) {                ## if there is a cache
        message("getting cached data")
        return(matx)                    ## then return the cache, no computation needed
    }
    data <- x$get()                     ## if there's no cache
    matx <- solve(data, ...)            ## we need to compute the inverse matrix here
    x$setInverse(matx)                  ## and save the result back into x's cache
    matx                                ## return the inverse matrix
}


## // Test Code
##
## a <- makeCacheMatrix(matrix(1:4,2))
## a$get()
##        [,1] [,2]
##   [1,]    1    3
##   [2,]    2    4
##
## a$getInverse()
##   NULL
##
## a$set(matrix(5:8,2))
## a$get()
##        [,1] [,2]
##   [1,]    5    7
##   [2,]    6    8
##
## cacheSolve(a)
##        [,1] [,2]
##   [1,]   -4  3.5
##   [2,]    3 -2.5
##
## cacheSolve(a)
##   getting cached data
##        [,1] [,2]
##   [1,]   -4  3.5
##   [2,]    3 -2.5
##
## a$getInverse()
##        [,1] [,2]
##   [1,]   -4  3.5
##   [2,]    3 -2.5
##
## // Test Inverse Correctness
##
## b = a$getInverse()
## a$get() %*% b              # matrix multiplication should show identity matrix
##         [,1]         [,2]
##   [1,]    1 3.552714e-15
##   [2,]    0 1.000000e+00

## // More Test Code
## 
## a <- makeCacheMatrix(matrix(nrow=2, ncol=2, c(2,3,4,5)))
## a$get()
##        [,1] [,2]
##   [1,]    2    4
##   [2,]    3    5
##
## cacheSolve(a)
##        [,1] [,2]
##   [1,] -2.5    2
##   [2,]  1.5   -1
##
## // Checked result from: http://www.bluebit.gr/matrix-calculator/
## 
##
