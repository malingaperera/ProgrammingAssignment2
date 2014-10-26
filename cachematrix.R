## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse
## of a matrix.
## @author:Malinga Perera <malingaonfire@gmail.com>

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## this can be used to set the starting matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## this can be used to get the starting matrix
    get <- function() {
        x
    }
    
    ## this can be used to set the starting inverse matrix
    setInverse <- function(mean) {
        inverse <<- mean
    }
    
    ## this can be used to get the starting inverse matrix
    getInverse <- function() {
        inverse
    }
    
    ## finally this will out put a list with 4 functions defined above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

## Before solving using this function you need to create a cacheble atric from 
## above method and set the matrix that you need to find the inverse using the set
## method

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## once the matrix is solved it get cached and if you call the solve on the 
    ## cacheble matrix for the second time you get the cached object
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If the inverse is NULL that means this is the first time that the user 
    ## running this method. So the original matrix will be taken and solved for the
    ## inverce
    originalMatrix <- x$get()
    m <- solve(originalMatrix)
    
    ## We will cache result for future use
    x$setInverse(m)
    m
}
