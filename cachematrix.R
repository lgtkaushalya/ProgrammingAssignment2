## Put comments here that give an overall description of what your
## functions do

## This function will accept a matrix and will provide getters and setters for matrix data
## and matrix inverse data

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(im) {
        inverseMatrix <<- im
    }
    
    getInverse <- function() {
        inverseMatrix
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function accept a special matrix object created with makeCacheMatrix function
## and return the inverse of the matrix. It first check whether inverse of the matrix
## is already saved with the given object and if so it return the cached inverse.
## if not it calculates the inverse newly using object data and cache it in the object
## as well as return it

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("Getting Cached Data")
        return(inverse)
    }
    matrixData <- x$get()
    inverse <- solve(matrixData)
    x$setInverse(inverse)
    inverse
}
