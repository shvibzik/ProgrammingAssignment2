## This function creates a special matrix, which is actually a list and containing
## functions to:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x<<-y #sets input argument to x
        inv<<-NULL
    }
    get <- function()x #gets the matrix
    setinverse <- function(inverse)inv <<- inverse #stores inverse of the matrix as inv; the inverse of the matrix is actually computed
    ## in the cacheSolve function
    getinverse <- function()inv #gets the inverse of the matrix, inv
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the matrix, 
## or returns the cached value if the matrix has been
## previously calculated and has not changed

cacheSolve <- function(x, ...) {
    #checks to see if the inverse has already been computed and returns the previously-computed value if so
    inv <- x$getinverse() #retrieves the cached inverse of the matrix and sets to "inv" 
    if(!is.null(inv)){
        message("getting cached data") #prints message
        return(inv)
    }
    #computes the inverse of the matrix
    inv_data <- x$get() #gets x from the parent environment and sets inv_data to x
    inv <-solve(inv_data, ...) #generates inverse of matrix and sets to inv
    x$setinverse(inv) #stores the inverse of the matrix
    inv
}
