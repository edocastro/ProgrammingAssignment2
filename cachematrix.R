## Put comments here that give an overall description of 
## what your functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  set_inverse_matrix <- function(inverse) inverse_matrix <<- inverse
  get_inverse_matrix <- function() inverse_matrix
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inversed_matrix <- x$get_inverse_matrix()
  if(!is.null(inversed_matrix)) {
    message("the return matrix is based on the cached data")
    return(inversed_matrix)
  }
  matrix_data <- x$get()
  
  ##calculate the inversed matrix
  
  inversed_matrix <- solve(matrix_data, ...)
  x$set_inverse_matrix(inversed_matrix)
  inversed_matrix
  
  
}
