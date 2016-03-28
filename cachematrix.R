## The first function "makeCacheMatrix" will store the inverse of a matrix in temporary storage on memory
## to be re-used for multiple calculations


## Set and get the value of Matrix; Set and Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y){
    x <<- y
    inve <<- NULL
    }
get <- function() x
setinverseCache <- function(inverseCache) inve <<- inverseCache
getinverseCache <- function() inve
list(set = set, get = get,
     setinverseCache = setinverseCache,
     getinverseCache = getinverseCache)
}

## The "cacheSolve" function will check to see if the inverse has already been calculated 
## and stored in Cache, if it is than it will utilize that value otherwise It'd have to 
## recalculated it.

cacheSolve <- function(x, ...) {
          inve <- x$getinverseCache()
          if(!is.null(inve)){
              message("getting inversed of Matrix Cached")
            return(inve)
          }
          data <- x$get()
          inve <- solve(data, ...)
          x$setinverseCache(inve)
          inve
}