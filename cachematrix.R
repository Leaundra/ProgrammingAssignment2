##Below are two functions that are used to create a special object that stores a matrix
## and cache's its inverse.

#THIS FUNCTION CREATES A LIST CONTAINING A FUNCTION TO
#	1. SET THE VALUES OF THE MATRIX
#	2. GET THE VALUES OF THE MATRIX
#	3. SET THE VALUES OF THE MATRIX INVERSE
#	4. GET THE VALUES OF THE MATRIX INVERSE

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL		#reset the global vector to NULL
  
  #DEFINE SET FUNCTION
  set <- function(y) {              
    x <<- y 	#assign the passed argument to a global vector
    m <<- NULL 	#set this global vector to NULL
  }
  
  #DEFINE GET FUNCTION
  get <- function() x	#returns global vector X
  
  #DEFINE SETINVERSE FUNCTION
  setinverse <- function(inverse) m <<- inverse  #assigns the passed argument to global vector
  
  #DEFINE GETINVERSE FUNCTION
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	#returns a List of the defined functions
  
}


##THIS FUNCTION CHECKS TO SEE IF THE INVERSE OF A MATRIX IS "CACHED". IT THE INVERSE OF THE MATRIX IS NOT
##IN THE CACHE THEN THE INVERSE IS CALCULATED AND PUT INTO THE CACHE.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()  #call 'getinverse' function and assign the return value
  
  #If the 'getinverse' function call did NOT return a NULL value then print a message
  # and return the getinverse function call value.
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }			
  
  #else calculate the inverse of the matrix, by getting the matrix and using the solve function
  data <- x$get()		#get the matrix values
  m <- solve(data) 	#calculate the inverse of the matrix
  x$setinverse(m) 	#set the inverse of the matrix to the global vector
  
  m	#return the matrix inverse
  
}
