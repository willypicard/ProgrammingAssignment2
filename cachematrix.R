makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this method creates a list of functions that are used to cache a matrix and its inversion.
# the cache variable contains the invertion
# the x variable contains the matrix
# the get and set methods concern the matrix
# the setCache and getCache methods concern the inversion
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setCache <- function(solve) cache <<- solve
  getCache <- function() cache
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
  
}


## Write a short comment describing this function

# the cacheSolve function inverts a matrix if the matrix has not been
# inverted. Otherwise, the cacheSolve function retrieves the cached version
# of the inverted matrix.
#
# the cacheSolve function takes advantage of the functions returned by the
# matkeCacheMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getCache()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setCache(cache)
  cache
}
