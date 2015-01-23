## Computing the inverse of a matrix can be costly in terms of computer
## resources and so it may be beneficial to cache the inverse of a 
## matrix instead of computing it repeatedly.
## These functions cache the results of a matrix inversion and
## return the result from cache if the matrix has not changed.
## These functions assume that matrix x is always invertible

## This function creates a "matrix" object that can cache the inverse
## of matrix x  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of matrix x using the solve function
## If the inverse of the matrix has already been computed then it returns 
## the cached result and if not it computes the inverse and saves it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
# Test Results
#> x = rbind(c(1, 2), c(2, 1))
#> m<-makeCacheMatrix(x)
#> m$get()
#     [,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> cacheSolve(m)
#           [,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#> cacheSolve(m)
#getting cached data
#           [,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333

