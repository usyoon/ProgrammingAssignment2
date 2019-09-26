## makeCacheMatrix returns a list of set, get, setInverse, and getInverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## refresh
    set <- function(y) {
          x <<- y
          i <<- NULL ##refresh
  }
  get <- function() x
  setinverse <- function(matrix) i <<- matrix ## put inverse matrix in i
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## return last thing
}


## cacheSolve gives an inverse of a matrix cache matrix

cacheSolve <- function(x) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i ## return last thing
}
