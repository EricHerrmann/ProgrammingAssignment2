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
    #initial the local mean to NULL
    cachedMean <- NULL
    
    # cache the vector
    set <- function(y) {
        x <<- y
        cachedMean <<- NULL
    }
    
    # return the cached vector
    get <- function(){
        x
    }
    
    setmean <- function(mean){ 
        cachedMean <<- mean
    }
    
    getmean <- function(){
        cachedMean
    }
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    # call getmean using the object x 
    # if x exists then we've already calculated it's mean -- 
    # return the calculated mean -- done as the last line of the function
    cachedMean <- x$getmean()
    if(!is.null(cachedMean)) {
        message("getting cached Mean")
    }
    else{
        #The mean of x did not exist so get x, which is a new vector
        nVector <- x$get()
        #calculate the mean on the new Vector
        cachedMean <- mean(nVector, ...)
        #cache x (with it's mean)
        x$setmean(cachedMean)
        message("mean not cached: Calculated and saved")
    }
    #return the mean
    cachedMean
}