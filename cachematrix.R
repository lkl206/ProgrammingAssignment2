## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialise inv as NULL
  set <- function(y) { #define set function
    x <<- y #value in parent env
    inv <<- NULL #reset inv to NULL if new matrix
  }
  get <- function() x #define get function
  setinverse <- function(inverse) inv <<- inverse #assigns inv value in parent env
  getinverse <- function() inv #gets inv value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

my_Matrix <- makeCacheMatrix(matrix(5:8, 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)
