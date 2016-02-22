# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly (there are 
# also alternatives to matrix inversion that we will not discuss here). 
# The assignment is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL                              # Matrix in Cache
        }
        get <- function() x                             # Get Matrix
        setInverse <- function(solve) m<<- solve        # Set Inverse Matrix
        getInverse <- function() m                      # Get Inverse Matrix
        list(set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
                )                                       # Create Functions List
 }

# The cacheSolve function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()                             # Get Matrix in Cache
        if(!is.null(m)) {                               # If Previous Cache Calculated
                message("getting cached data")          # Send Message
                return(m)                               # Return Cache
        }
        data <- x$get()                                 # Get Matrix Created By Prior Function
        m <- solve(data, ...)                           # Caculate Matrix Inverse
        x$setInverse(m)                                 # Save Matrix Inverse In Cache
        m
}
# a<-diag(4,4)
# a
#      [,1] [,2] [,3] [,4]
# [1,]    4    0    0    0
# [2,]    0    4    0    0
# [3,]    0    0    4    0
# [4,]    0    0    0    4
#
# Matrix_a <- makeCacheMatrix(a)
#
# cacheSolve(Matrix_a)
#      [,1] [,2] [,3] [,4]
# [1,] 0.25 0.00 0.00 0.00
# [2,] 0.00 0.25 0.00 0.00
# [3,] 0.00 0.00 0.25 0.00
# [4,] 0.00 0.00 0.00 0.25
