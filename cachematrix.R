
# R Programming - Assignment 2 - Lexical Scoping

# makeCacheMatrix  store the matrix and the inverse
# cacheSolve       uses the solve function to inverse a matrix and store a copy so it doesn't need to be recalculated


## makeCacheMatrix -creates a special "matrix" object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
     
     inverse <- NULL
     
     # Set a new matrix, and null the inverse so that it will be recalculated the next time cacheSolve is called
     set <- function(y) {
          x <<- y 
          inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(new_inverse) inverse <<- new_inverse
     getinverse <- function() inverse
     
     list( set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
     
}


## cacheSolve -compute the inverse of the special "matrix"
##   if the cache has already been calculated, and the matrix hasn't changed
##      the cache is returned
##   otherwise calculate, cache and return the matrix
cacheSolve <- function(x, ...)
{
     inverse <- x$getinverse()
     if(!is.null(inverse))
     {
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data)
     x$setinverse(inverse)
     inverse
}
