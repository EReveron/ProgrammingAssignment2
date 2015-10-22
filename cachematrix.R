## cachematrix.R
## Implement a cache solution to calculate the inverse of a matrix using solve() function
## We assume that all the matrix are posible to invert
 
## Function: makeCacheMatrix
## Create a object called "CacheMatrix" from a matrix() element that include a list with four functions:
##	1) set = a function that assign x (the original matrix() element) and initialice the "s" inverse value to NULL
##	2) get = a function that returns x (the original matrix() element)
##	3) setsolve = a function that set the value of the inverse to "s"
## 	4) getsolve = a function that returns the value of "s" (the inverse of x)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function: cacheSolve
## Provide the inverse matrix of a matrix object create with the function makeCacheMatrix.
## This function assume that all the matriz can be inverted using the solve() function
## The first time that the function is called (the getsolve()) value is NULL so the
## function got the matrix thought get() function and calculate the inverse using solve(), at
## the end catch that value using setsolve() function.
## The next time that the function is called, return the cached value (s)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

