## The first function "makeCacheMatrix" creates a list containing a function to set and get the matrix,
## and to set and get the inverse of the matrix
## The second function "cacheSolve" solves for the inverse of the matrix, by first checking if 
## that value is cached to avoid re-computing it, and if not it will calculate the inverse and 
## set it in the cache for future use.

## provides caching functions with set and get functions to store and retrieve values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Solves for matrix inverse using makeCacheMatrix to check for cached answer first to save compute time
## then if no cached value is found, it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
