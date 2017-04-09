## This function creates a special matrix, which is actually a list and containing
## functions to:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    get <- function()x
    setinverse <- function(inverse)inv <<- inverse
    getinverse <- function()inv
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the matrix, 
## or returns the cached value if the matrix has been
## previously calculated and has not changed

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    inv_data <- x$get()
    inv <-solve(inv_data, ...)
    x$setinverse(inv)
    inv
}
