##Basically, The two function work in unison to achieve the purpose
##of caching.

##First you call makeCacheMatrix, which sets up an environment where 
##inverse of the matrix and the vector is cached and it also defines
##and provides a list of functions which can manipulate these cached 
##values.

##now the cacheSolve comes into play and it uses the list of functions 
##and "solve" fuction to either calculate the mean of the vector and 
##cache it or to provide a cached copy of the result 









## provides an environment for caching of inverse  in the variable 'm'
##, defines function which will be used for manipulation on the 
##caching variables and returns a list of such functions 


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  
  
}


## the function where the decesion is done whether to calculate the 
##inverse and cache it or to provide the cached copy of the result
##depending upon whether ot not the value of the variable 'm'
##is NULL or not in the parent environment 




cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}