## Pair of functions below may be used to calculate inverse of matrix in cache'able manner

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setim <- function(inv_matr) im <<- inv_matr
  getim <- function() im
  list(set = set, get = get,
       setim = setim,
       getim = getim)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## Inverse will be calculated only once, all subsequent calls will retrieve value from cache
cacheSolve <- function(x, ...) {
  im <- x$getim()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setim(im)
  im
}