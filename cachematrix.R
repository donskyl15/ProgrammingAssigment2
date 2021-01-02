#creating a function that creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
   x <<- y
   inv <<- NULL
  }
  get <- function() {x}                 #gets matrix
  setInverse <- function(inverse) {inv <<- inverse}   #sets matrix 
  getInverse <- function() {inv} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#cachesolve will compute the inverse of the matrix
cacheSolve <- function(x,...){
  inv <- x$getInverse()
  if(!is.null(inv)){                    #checks if inverse is null
    message("getting cached data")      #will retunrn 'getting cached data'
    return(inv)                         #returns inverse 
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
