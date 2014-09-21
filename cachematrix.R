## The following functions allow for creating a matrix, then setting its inverse
## and caching the result. 
## This code is based on the cachemean example given in class.

## The makeCacheMatrix function takes a matrix as its argument,
## and generates a list of functions to:
## set the value of matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

## The value entered for x must be a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## If the setmatrix function is called to enter a new matrix value (y),
        ## the value for the inverse (i) will be reset to null.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes a matrix as its argument and
## checks to see if the inverse has already been computed.
## If there is a cached value for the inverse, it returns the cached value.
## If there is no cached value for the inverse, it computes the inverse,
## sets the new cached value for the inverse and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        
        ## Checks to see if there is a value for the inverse already computed
        ## and stored in the getinverse function in makeCacheMatrix
        i <- x$getinverse()
        if(!is.null(i)) {
                
                ## If valuee for inverse is cached, returns that value 
                message("getting cached data")
                return(i)
        }
        
        ## If value for inverse is not cached, computes the inverse
        ## sets new cached value, and returns the result
        newinverse <- x$get()
        i <- solve(newinverse, ...)
        x$setinverse(i)
        i
}
