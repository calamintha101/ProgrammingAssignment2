# In statistics, some matrix algebra computations are notoriously expensive, such as calculating the inverse 
# of a matrix. Therefore, if one needs to use the same inverted matrix for subsequent computations, it is 
# advantageous to cache it in memory instead of repeatedly calculating the inverse.
#
# R uses LEXICAL SCOPING; as a genereal concept, the value of y in the function g is looked up in the 
# environment in which the function was *defined*. Therefore, the output of 'makeCacheMatrix' contains a complete 
# copy of the environment for cacheSolve(), including any objects that are defined within makeCacheMatrix() at 
# design time (i.e., when it was coded).
#
# The function 'makeCacheMatrix' takes a matrix as input and provides the functions to store and retrieve 
# a calculated result. These function pointers are returned as a list.
# The function pointers serve as input for the function 'cacheSolve'. This function does the actual calculation
# of inverting a matrix; however it first checks whether the result is already available in the cache and 
# performs the calculation only if it is not yet available. It will then store the result in the cache for a next
# time. 
#
#
# Example usage:
#
# n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
# myMatrixCache <- makeCacheMatrix(n2)  # provide input matrix and initialize the cache
# cacheSolve(myMatrixCache)  # thhis does the inversion calculation and stores result into internal cache
# cacheSolve(myMatrixCache)  # this time the result is returned from the cache
#
# myMatrixCache <- makeCacheMatrix(n2) # this will reinitialize the cache by setting the 'cachedValue' to NULL; 
                                       # therefore even though the input is the same matrix, it will be calculated again.
# cacheSolve(myMatrixCache)  # therefore this will calculate the inversion again
#



## This function takes a matrix as input and constructs the methods for caching a result.
#
# input: matrix which is invertable
# output: list with function pointers to be used as input to the function cacheSolve()
#         - getIntputValue: retrieves the input matrix
#         - setCacheValue stores the calculated inverted matrix to the cache
#         - getCacheValue: retrieves the last inverted matrix from the cache
#
# note: the provided vector example also defined a 'set' function; since it is not needed, I omit it here.

makeCacheMatrix <- function(inputValue = matrix()) {
  # reset the cached value
  cachedValue <- NULL

  # store the inputValue and make it accessible with function getInnputValue
  getInputValue <- function() inputValue
  # store the 'result' in the cache
  setCacheValue <- function(result) cachedValue <<- result
  # retrieve the stored result from the cache
  getCachedValue <- function() cachedValue
  # output a list of function pointers for use with the function 'cacheSolve'
  list(getInputValue = getInputValue,
       setCacheValue = setCacheValue,
       getCachedValue = getCachedValue)
}

## Return a matrix that is the inverse of 'x'.
## If this inverse matrix has already been calculated, return the value from the cache.
## If it has not yet been calculated, then calculate it and store its value to the cache for next time use.
cacheSolve <- function(x, ...) {
    
   # check whether a cached result is available
   invValue <- x$getCachedValue()
   if(!is.null(invValue))
   {
     # return the cached result
     message("getting cached data")
     return(invValue)
   }
   
   # retrieve the input matrix
   data <- x$getInputValue()
   # calculate the inverse of the input matrix
   invValue <- solve(data, ...)
   # cache the calculated result
   x$setCacheValue(invValue)
   # return the calculated result
   invValue
}

