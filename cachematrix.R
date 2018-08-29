makeCacheMatrix <- function(x = matrix()) {
      inv_matrix <- NULL
      set <- function(y) {
            x <<- y
            inv_matrix <<- NULL
      }
      get <- function() x
      set_inv <- function(z) inv_matrix <<- z
      get_inv <- function() inv_matrix
      list(set = set, get = get,
           set_inv = set_inv,
           get_inv = get_inv)
}


cacheSolve <- function(x, ...) {
            inv_matrix <- x$get_inv()
            if(!is.null(inv_matrix)) {
                  message("getting cached data")
                  return(inv_matrix)
            }
            data <- x$get()
            inv_matrix <- solve(data, ...)
            x$set_inv(inv_matrix)
            inv_matrix
      }