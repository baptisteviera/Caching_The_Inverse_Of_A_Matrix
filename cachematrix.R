
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  set <- function(y) 
  { x <<- y 
    inverse <<- NULL 
  }
  get <- function()x
  setinverse <- function(solve) inverse<<-solve # calcul of the inverse of the matrix
  getinverse <- function()inverse #we get the inverse of the matrix 
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
  }


# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) # true => already calculated
  {
    message("getting cached data")  
    return(inverse) #we get the inverse of the matrix from the cache
  }
  data <- x$get()
  inverse<- solve(data,...) #we get the inverse of the matrix thanks the solve fonction
  x$setinverse(inverse) 
  inverse #print the inverse
}
