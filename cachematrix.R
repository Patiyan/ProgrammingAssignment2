# The two functions "makeCacheMatrix" and "cacheSolve" creates a matrix object and caches its inverse.


# This function allows the creation of a matrix and the possibility of modifying the matrix without rerunning the function from the start. 
# It uses the information generated in the "cacheSolve" function and allows the return of the matrix inverse.
# While the "cacheSolve" function only updates m locally, the "makeCacheMatrix" allows the m to be updated in the global environment

makeCacheMatrix <- function(x=matrix) {         # create a matrix
      m <- NULL                                 # sets m to null in the local environment
      set <- function(y) {                      # can be used to alter the matrix without rerunning the whole makeCacheMatrix function
            x <<- y                             # where y becomes the new x
            m <<- NULL                          # m is updated in the global environment
      }
      get <- function() x                       # will return matrix
      setSolve <- function(solve) m <<- solve   # runs the solve function on the matrix, and updates m in the global environment
      getinverse <- function() m                # returns the matrix inverse, first time run will return nothing as nothing is cached
      list(set = set, get =get,                 
           setSolve = setSolve,
           getinverse = getinverse)
}

# This function takes the above matrix as input, and calcuates, caches and returns the matrix inverse.
# If the matrix inverse has already been cached, these values will be returned.
# If the matrix has been modified using the set function, the new matrix inverse will be calculated, cached and retuned.

cacheSolve <- function(x, ...) {                # function to compute, cache and return matrix inverse
      m <- x$getinverse()                       # locally sets m to matrix inverse, first time run will be set to nothing
      if(!is.null(m)) {                         # checks if m is null, if not it will retun the cached values
            message("getting cached data")
            return(m)
      }
      data <- x$get()                           # put the matrix into data
      m <- solve(data, ...)                     # runs solve on the matrix to generate the matrix inverse
      x$setSolve(m)                             
      m                                         # return the values of m
}