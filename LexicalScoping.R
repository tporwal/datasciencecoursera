makeCacheMatrix <- function(x = matrix())
{
  j <- NULL
  set <- function(y) 
  {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set,get = get, setinverse = setinverse,getinverse = getinverse)
}
cacheSolve <- function(x, ...) 
{
  j<- x$getinverse()
  if (!is.null(j)) 
  {
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
}
B <- matrix(c(1,2,5,6),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
