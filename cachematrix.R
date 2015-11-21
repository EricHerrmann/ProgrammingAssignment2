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
    cachedInverse <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    #NOTE:  It is possible to write the get/set functions on a single line, but I find that 
    # confusing -- so I made them look like standard functions.
    # get the value of the matrix
    get <- function(){
        x
    }
    
    #solve returns the inverse of the matrix
    #set the value of our cachedInverse matrix
    setInverse <- function(solve){
        cachedInverse <<- solve
    }

    #return the cached inverse matrix
    getInverse <- function(){
        cachedInverse
    }
    
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
    cachedInverse <- x$getInverse()

    # if x exists then we've already calculated it's inverse -- return the calculated inverse
    if(!is.null(cachedInverse)) {
        message("getting cached inverse")
    }
    else{
        #The inverse of x did not exist so get x which is a new matrix
        nMatrix <- x$get()
        #calculate the inverse of the new Matrix (nMatrix)
        cachedInverse <- solve(nMatrix, ...)
        #save x (with it's mean)
        x$setInverse(cachedInverse)
        message("inverse not cached: solved and saved")
    }
    #return the inverse
    cachedInverse
}
