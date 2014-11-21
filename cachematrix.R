##These functions calculates an inverse of a matrix, and stores it in a cache.  If the next call of the inverse function is already 
##in the cache, the inverse is already calculated and returned with a message.  If a new matrix is called, then the inverse is calculated 
##and stored in the cache.

## makeCacheMatrix function initializes the matrix that we will take the inverse on.
makeCacheMatrix <- function(x = matrix()) {  # input x will be a matrix
  m <- matrix    #  m will be our 'inverse' and it's reset to an empty matrix every time makeCacheMatrix is called
  
  set <- function(y) {    # takes an input matrix
    x <<- y         # saves the input matrix 
    m <<- matrix      # resets the inverse to empty matrix, basically what happens when a new object is generated.
  }
  
  get <- function() { x }   # this function returns the value of the original matrix
  
  setinverse <- function(solve)  { m <<- solve }
  # this is called by cacheSolve() during the first cacheSolve() access and it will store the value using superassignment
  
  getinverse <- function() { m } # this will return the cached value to cacheSolve() on subsequent accesses
  
  list(set=set, get = get,          #  This is accessed each time makeCacheMatrix() is called,       
       setinverse = setinverse,  #   that is, each time we make a new object.  This is a list of 
       getinverse = getinverse)  #   the internal functions ('methods') so a calling function knows how to access those methods.                            
}

## cacheSolve function checks to see if there is a matrix in the cache and prints it if there is.  
##If there is not, they will recalculate the inverse and store it in the cache.
cacheSolve <- function(x, ...) {   # the input x is an object created by makeVector
  m <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
  
  if(!is.null(dim(m))) {# if inverse was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the mean ... "return" ends  the function cacheinverse(), note  
  }
  data <- x$get()        # we reach this code only if x$getinverse() returned NULL
  m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
  x$setinverse(m)           # store the calculated inverse matrix in x (see setinverse() in makeVector
  m               # return the inverse matrix to the code that called this function
}