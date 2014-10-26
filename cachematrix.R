#These functions create an object with a matrix and associated inverse.
#They allow caching the inverse so it doesn't have to be calculated multiple times.

#This function makes an object containing a matrix and the inverse of that
#matrix, along with methods to get and set the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #this stores the inverse
  set <- function(y){ #this stores the matrix
    x <<- y
    i <<- NULL
  }
  get <-function() x #this gets the matrix
  
  setinverse <- function(inverse){
    i <- inverse
  }
  getinverse <- function() i
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}

#this function returns the inverse of x, using the object defined in makeCacheMatrix.
#if the inverse is not solved, it calculates the inverse, 
#otherwise it returns the cached value.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...) #calculate inverse
  x$setinverse(i) #cache inverse
  
  return(i)
}
