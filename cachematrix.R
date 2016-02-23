## The below functions calculate and stores the inverse of a matrix and
## Stores it in cache so that it is unnecesary to recalculate the inverse
## constantly.

## Requires an inversible matrix as input. Creates a list which stores
## the functions to set and get the value of the matrix, and 
## furthtermore sets and gets the the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NUL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## takes the result from the makeCahcheMatrix function and calculates
## the inverse of the matrix and stores it in cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("retrieving cached data")
                return(inv)
        } 
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
