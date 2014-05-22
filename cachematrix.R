## makeCacheMatrix function caches the input matrix y and inverse matrix inv.
## makeCacheMatrix return a special vector(list) consisting of getter and setter for matrix y and it's inverse
## the returned list is used by cacheSolve function to create and cache the inverse of matrix y
## cacheSolve function create inverse of the matrix y if there is no cache, if there is already cache inverse, return the cached inverse matrix


## this function makeCacheMatrix cache the matrix y, and it's inverse 
## return a list consisting of getter and setter for matrix y and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_m <<- inv
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This cacheSolve function return the inverse of the input variable x
## x is the list returned by makeCacheMatrix
## check whether there is a cached inverse of x and return cached inverse of x if exist, otherwise create inverse and cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setinv(inv_m)
  inv_m
}
