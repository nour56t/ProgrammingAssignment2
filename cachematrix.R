## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  N <- NULL
  set <- function(y){
    x <<- y
    N <<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) N <<- inverse
  getInverse <- function() N
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}





## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  N <- x$getInverse()
  ## if the inverse has already been calculated 
  if(!is.null(N)) {
    message("getting cached data")
    return(N)
  }
  mat <- x$get()
  N <- solve(mat,...)
  x$setInverse(N)
  return (N)
}

