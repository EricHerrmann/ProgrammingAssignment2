## -------------------------------------------------------------------- ##
## This area contains a brief description of all functions in this file
## -------------------------------------------------------------------- ##
## -------------------------------------------------------------------- ##
## makeVector: Creates a vectpr object that can cache it's mean
## cachemean : Computes the mean of a vector object
##             Returns the cached object when there is no change                    
## -------------------------------------------------------------------- ##


## -------------------------------------------------------------------- ##
## makeCacheMatrix: Creates a vectpr object that can cache it's mean
##      The vector Object is a list that contains a function
##      1. set the value of the vector
##      2. get the value of the vector
##      3. set the value of the mean of the vector
##      4. get the value of the mean of the vector
## -------------------------------------------------------------------- ##
makeVector <- function(x = numeric()) {
    m <- NULL
    # set the value of the vector
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get the value of the vector
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    # call getmean using the object x 
    # if x exists then return that value
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #The mean of x did not exist so get x
    data <- x$get()
    #calculate the mean
    m <- mean(data, ...)
    #save x (with it's mean)
    x$setmean(m)
    #return the mean
    m
}