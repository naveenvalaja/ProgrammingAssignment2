## These below functions are to set value of the Matrix and cache the inverse of the matrix
## If the matrix is not changed then the inverse from cache will be returned else inverse is
## calculated and stored in cache

## Set the matrix and set the inverse matrix
##Input to this Function is a matrix which will be set to a variable
##Output of this function is a list and should be stored to a variable to use in CacheSolve func


makeCacheMatrix <- function(x = matrix()) {
  matinv <- matrix(numeric(0),0,0)
  set <- function(y) {
    mat <<- y
    matinv <<- matrix(numeric(0),0,0)
  }
  get <- function() mat
  setinv <- function(inv) matinv <<- inv
  getinv <- function() matinv
  set(x)
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Look if matrix is existing and inverse is existing in cache, if existing return the cache
## Else create the inverse, set in cache and return 
##Input to this function is the output of makeCacheMatrix
##Matrix value is defaulted to set value, f a new value is passed then inverse will be
## Computed and stored to cache else pulled from cache
cacheSolve <- function(x, m=mat,...) {
  if(identical(m,mat)){
  matinv <- x$getinv()
  if(nrow(matinv)!=0 & ncol(matinv)!=0) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data)
  x$setinv(matinv) 
  matinv
  }
  x$set(m)
  data <- x$get()
  matinv <- solve(data)
  x$setinv(matinv) 
  matinv
  ## Returning the inverse of matrix
}
