#Matrix inversion is usually a costly computation and there 
#may be some benefit to caching the inverse of a matrix rather 
#than compute it repeatedly. 
#The following functions  compute and cache the inverse of a matrix.


##
##input: a matrix
##output: a list of functions that
## - set the matrix
## - get the matrix
## - set the inverse of a matrix
## - get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set <- function(y)
    {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inverse <<- solve
    getInv <- function() inverse
    list(set = set, get = get,
    setInv = setInv,
    getInv = getInv)
}

##
##input: a list from function makeCacheMatrix()
##output: inverse of the original matrix that was 
##input to function makeCacheMatrix()
cacheSolve <- function(x, ...) {
        inverse <- x$getInv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse        
}

#how to run functions:
# > mat0 = matrix(seq(1:4), 2)
# > mat0
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4 
# > mat1 = makeCacheMatrix(mat0)
# > cacheSolve(mat1)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mat1)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


