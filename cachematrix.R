## caching a matrix inverse to save computation
# given a matrix mat, start by mat1 <- makeCacheMatrix(mat)
# note that mat1 is a complicated wrapper similar to an object

# to get the inverse use cacheSolve(mat1)
# the first time it is called after a change, it calculates and caches
# thereafter it retrieves from the cache until the next change

makeCacheMatrix <- function(mat = matrix()) {
  invmat <- NULL
  set <- function(m) {
    mat <<- m
    invmat <<- NULL
  }
  get <- function() mat
  setInv <- function(invm) invmat <<- invm
  getInv <- function() invmat
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(mat, ...) {
  invmat <- mat$getInv()
  if(!is.null(invmat)) {
    message("getting cached inverse")
    return(invmat)
  }
  temp <- mat$get()
  invmat <- solve(temp, ...)
  mat$setInv(invmat)
  invmat
}
