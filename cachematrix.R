## These functions allow the usage of cached matrices and inverses
## (Only works for invertible matrices)


##This function, makeCacheMatrix creates a special "matrix"
##which is a list containing a function to set the value, get the
##value, as well as set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            i <-NULL
            set <-function(y){
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


## This function calculates the inverse of the matrix created above
## However it first checks for the inverse in the cache to skip
## uneccesary calculations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data,...)
      x$setinverse(i)
      i
}
