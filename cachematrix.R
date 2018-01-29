# Provide sample matrix, we want to see that our function actually works right?
matrix <- matrix(sample(1:10,9),nrow=3,ncol=3)
print("We will be checking against the following matrix:")
print(matrix)


# makeCacheMatrix: 
#   This function creates a special "matrix" object that can cache its inverse.
#   Function based on 

makeCacheMatrix <- function(x = matrix()) {
  # create storage var with undefined content
  store <- NULL
  
  # pull input matrix, set in present environment
    # note: <<- means superassignment = assignment in the enclosing frame, works its way 
    # up towards the global environment until it finds a variable called $var
  set <- function(y){
    x <<- y
    store <<- NULL
  }
  
  # pull matrix
  get <- function() x
  # invert matrix and set as stored var  
  setInvM <-  function(inverse) store <<- inverse
  # pull inverted matrix
  getInvM <- function() store
  
  # push our new functions to the environment
  list(set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}


# cacheSolve: 
#   This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#   If the inverse has already been calculated (and the matrix has not changed), then the 
#   cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  # check cache
  var <- x$getInvM()
  
  # check if inverse is already in environment
  if(!is.null(var)) {
    message("fetch cached data")
    return(var)
  }
  
  # if inverse his not present, calculate inverse
  data <- x$get()
  var <- solve(data, ...)
  
  # set inverse
  x$setInvM(var)
  var
}

# Testing.... Return matrix
matrix_solve <- makeCacheMatrix(matrix)
cacheSolve(matrix_solve)
