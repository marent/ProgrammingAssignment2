## The next functions create, stores and gets a matrix and its inverse from the cache

## This function set the matrix and its inverse in the cache and is also able to retrieve/get both from the cache.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL  ## Inverse matrix result stored
set <- function(y) { ## save matrix in cache
  x <<- y
  m <<- NULL
}
get <- function() x ## recall matrix
setInv <- function(Inv) m <<- Inv ## save inverse matrix in cache under m value
getInv <- function() m ## recall inverse matrix
list(set = set, get = get,
     setInv = setInv,
     getInv = getInv) ## create list functions
}


## This function tries to get the inverse matrix from x. if it is stored in the cached, returns a message (getting cached data). If it is not stored
## it calculates the inverse matrix and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() ## recall inverse matrix from x
  if(!is.null(m)) { # if m is not null
    message("getting cached data")
    return(m) ## return the above message
  }
  data <- x$get() ## if m is NULL, get matrix
  m <- solve(data, ...) ## and solve the inverse matrix to m
  x$setInv(m) ## m is stored in setInv
  m ## return m
}
