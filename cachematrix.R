### Matrix inversion is usually a costly computation and their may be some benefit to caching
### the inverse of a matrix rather than compute it repeatedly. There is a pair of functions that 
### cache the inverse of a matrix

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# Usage of the function:
# m<-makeCacheMatrix()   :create list m with empty matrix x
# m$set(matr)            :set matrix x in list m to matrix matr
# m$get()                :get the matrix x from the list m
# m$setInverse(inv)      :cache the matrix inverse in variable inv within list m
# m$getInverse()         :return the cached value cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

# Usage of the function:
# cacheSolve(x) :returns the inverse of the "matrix" x
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}

# Example 
a <- makeCacheMatrix(matrix(c(1:8,15),3,3))
cacheSolve(a)
