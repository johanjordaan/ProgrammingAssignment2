## These functions allows for the caching of the inverse of a matrix 
## ** test() for usage examples
##  


## Creates a cache matrix that can cache the inverse of a matrix
## Retuns a 'object' with the the accessor methods and the inverse
##  and target matrix 'closed' in
## Expects a standard matrix as input (if none a defualr empty 1x1 matrix is created)
## 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix using a cache matrix 
## Returs the inverse (calculated using solve)
## Expects a cacheMatrix constraucte via makeCacheMatrix
## * Passses all extra parameters to solve
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    #message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# This is a simple test of the functionality of this module
#
test <- function() {
  # Create a random 3x3 matrix
  m = matrix(rexp(9, rate=.1), ncol=3) 
  
  # Create the cached version
  cm <- makeCacheMatrix(m)
  if(sum(cm$get() == m) != length(m))  { message("m not set properly"); return }
  if(!is.null(cm$getinverse())) { message("i not set properly"); return }
    
  # Calculate the inverse
  im <- cacheSolve(cm)

  # See if m multiplied with the Inverse give I 
  if(sum(round(im %*% m) == diag(dim(m)[1])) != length(m)) message("inverse not calculated correctly")

  # Check that the cached inverse is set  
  if(is.null(cm$getinverse())) { message("i not set properly after solving"); return }
  
}
