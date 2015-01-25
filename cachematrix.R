## A pair of functions that cache the inverse of a matrix for re-use


## makeCacheMatrix function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL

   set <- function(y) {
      x <<- y
      i <<- NULL
   }
  
   get <- function() x

   setinverse <- function(inverse) i <<- inverse
  
   getinverse <- function() i
  
   list  (set = set
         ,get = get
         ,setinverse = setinverse
         ,getinverse = getinverse
         )

}


## cacheSolve function
## Return the inverse of a matrix. If inverse has already be computed
## and held in cache then use this value instead to avoid performing
## potentially expensive work again.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

   i <- x$getinverse()

   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }

   data <- x$get()

   i <- solve(data, ...)

   x$setinverse(i)

   i
}
