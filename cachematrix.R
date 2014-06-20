# This function takes an invertible matrix and creates a special
# matrix that is able to cache the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) m <<- inverse
  get.inv <- function() m
  
  list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
  
}


# This function takes the "matrix" output of the
# "makeCacheMatrix" function as its input and checks 
# see if its inverse has already been computed, and 
# if it has it prints the inverse. If not, then it 
# proceeds to compute the inverse and then prints it.

cacheSolve <- function(x,...){
  
  m <- x$get.inv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  mat <- x$get()
  
  m <- solve(mat,...)
  
  x$set.inv(m)
  
  print(m)
}

