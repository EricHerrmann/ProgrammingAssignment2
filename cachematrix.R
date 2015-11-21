## -------------------------------------------------------------------- ##
## This area contains a brief description of all functions in this file
## -------------------------------------------------------------------- ##
## -------------------------------------------------------------------- ##
## makeCacheMatrix: Creates a matrix object that can cache it's inverse
## cacheSolve:      Computes the inverse of a matrix object
##                  Returns the cached object when there is no change                    
## -------------------------------------------------------------------- ##


## -------------------------------------------------------------------- ##
## makeCacheMatrix: Creates a matrix object that can cache it's inverse
## Assumption: An inverse matix exist for all input matricies
##             input X is a matrix
## -------------------------------------------------------------------- ##
makeCacheMatrix <- function(x = matrix()) {
    inverseLocal <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inverseLocal <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    #solve returns the inverse of the matrix
    setInverse <- function(solve) inverseLocal <<- solve
    getInverse <- function() inverseLocal
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}



## -------------------------------------------------------------------- ##
## cacheSolve:      Computes the inverse of a matrix object
## -------------------------------------------------------------------- ##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #if x == cache$x then return cache$x else solve for x
    # call getInverse using the object x 
    # if x exists then return that value
    inverseLocal <- x$getInverse()
    if(!is.null(inverseLocal)) {
        message("getting cached inverse")
        return(inverseLocal)
    }
    #The inverse of x did not exist so get x
    data <- x$get()
    #calculate the inverse
    inverseLocal <- solve(data, ...)
    #save x (with it's mean)
    x$setInverse(inverseLocal)
    #return the inverse
    inverseLocal
}
