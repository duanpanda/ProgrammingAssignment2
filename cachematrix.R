## makeCacheMatrix() makes a matrix that can cache its inverse.
##     We call it a CacheMatrix.
## cacheSolve() returns the inverse matrix of a CacheMatrix.
##
## When the inverse of a CacheMatrix is first computed (by calling cacheSolve()),
## it does the real computation and store it inside the CacheMatrix object itself.
## After that, every time we retrieve the inverse (by calling cacheSolve()),
## it simply returns the cached inverse inside the CacheMatrix object.
##
## author: Ryan Duan <duanpanda@gmail.com>


## Make a matrix that can cache its inverse.
##
## This function returns a list of 4 functions.
##
## Each function in the internal list is a closure that encloses the function body
## and the internal environment which contains two variables: x and ix.
## x stores the matrix itself, ix is used to cache the inverse of the matrix.
##
## This closure and environment model implements R's lexical scoping.
## When we call set() and setInverse() from the Global environment (Console),
## these two functions use <<- operator to access and modify their internal 
## environment's variables.

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL # inverse of x
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                ix <<- inverse
        }
        getInverse <- function() {
                ix
        }
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


## Returns the inverse of the CacheMatrix x. The return value is of class matrix.
## If x has already cached its inverse, simply return it, if not, call solve()
## to compute it, and save it into x, then return the result of solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getInverse()
        if (!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setInverse(ix)
        ix
}
