## This functions calculate and store in cache result of computation of solve function on square valid matrix of data
##Example usage:
# > source(file = "cachematrix.R")
# > test <- matrix(data = c(1:4), nrow = 2, ncol = 2)
# > test
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheMatrix <- makeCacheMatrix(test)
# > cacheMatrix$get
# function() x
# <environment: 0x39ebb58>
# > cacheMatrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheMatrix$getsolve
# function() cacheSolve
# <environment: 0x4b2e8c8>
# > cacheMatrix$getsolve()
# NULL
# > cacheSolve(cacheMatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## makeCacheMatrix
## function that returns an object that can set and get data for computation and storing and getting the result from cache.
## Params:
## x - a cached matrix that represents the data.
## Returns: List object that can compute and cache the data.

makeCacheMatrix <- function(x = matrix()) {
  cacheSolve <- NULL ## If we are forced to calculate matrix all over again we clear cache first.
  set <- function(y) { ## Create a promise that will be lazy evaluated on execution
    x <<- y ## Assign function param in enviroment of this function
    cacheSolve <<- NULL ## Clear cache while setting new value
  }
  get <- function() x ## Promise that lazy evaluates itself on call
  setsolve <- function(inverse) cacheSolve <<- inverse ## Function that sets cached result in function enviroment
  getsolve <- function() cacheSolve ## Function that retrieves cached result if set (please mind it might be null)
  list(set = set, get = get, ## Special object that returns a way to set and get data and set and get result.
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve
## function that returns result if cached and if not it is caculating it and caching.
## Params:
## x - object returned by makeCacheMatrix function that allows core operations
## ... - all other arguments that will be handeled to solve function
## Returns: Result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getsolve()
        if(!is.null(cache)) {
          message("getting cached data")
          cache
        } else {
          data <- x$get()
          cache <- solve(data, ...)
          x$setsolve(cache)
          cache
        }      
}
