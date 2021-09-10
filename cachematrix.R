##A pair of functions that cache the inverse of a matrix

##  This fucntion creates a special matrix in order to cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  ## intialize the inverse
  inversed <- NULL

  ## Method to set the matrix
  set <- function(y){
    x <<- y
    inversed <<- NULL
  }

  ## Method to get the Matix
  get <- function() x

  ## Method to set the matrix
  set_inverse <- function(solve_matrix) inversed <<- solve_matrix

  ## Method to get the inverse of the matrix
  get_inverse <- function() inversed

  ## Return a list of the methods
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$get_inverse()

        ## Return the inverse if its already set
        if(!is.null(inversed)){
          message("getting cached data")
          return(inversed)
        }

        ## Get the matrix from our object
        data <- x$get()

        ## Calculate the inverse
        inversed <- solve(data)

        ## Set the inverse to the object
        x$set_inverse(inversed)


        inversed

}
