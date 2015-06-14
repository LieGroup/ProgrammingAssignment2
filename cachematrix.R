## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing functions:
### 'set`: set the matrix
### `get`: get the matrix
### `setinverse`: set the inverse of the matrix
### `getinverse`: get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # inverse matrix of x
    
    ## update x and reset the inverse matrix of x
    set <- function(newmatrix) {
        x <<- newmatrix
        inverse <<- NULL
    }
    
    get <- function() x # get x
    # set the inverse matrix of x
    setinverse <- function(inversematrix) inverse <<- inversematrix
    # get the inverse matrix of x
    getinverse <- function() inverse
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function calculates the inverse matrix of the special "matrix" created with the above 'makeCacheMatrix' function. 
## It first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse matrix from the 
## cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the 'inverse' 
## in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    # if the inverse matrix has been calculated, return it from the cache
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # otherwise, calculate the inverse matrix and update the cache
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
    inverse
}
