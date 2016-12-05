## our assignment is to write a pair of functions that cache the inverse of a matrix.
## functions do

## Stores a cache of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_matrix <- function(matrix) m <<- matrix
  get_matrix <- function() m
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix)

}


## This function inverses a matrix. 
## To test run the following: 
## y <- matrix((4,2,7,6), nrows=2, ncols=2)
## test_file <- makeCachematrix(y)
## cacheSolve(test_file)
## result is [0.6, -0.2,-0.7,0.4]

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_matrix(m)
  m
}
