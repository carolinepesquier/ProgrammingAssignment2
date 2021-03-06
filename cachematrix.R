## The 2 functions below create a special object that stores a matrix and caches its inverse.
## The code assume that the input matrix is invertible.

## 'makeCacheMatrix' : This first function returns a list that contains a set of functions 
## that are returned to the parent environment.
## The functions in the list store the 'x' matrix and create a cache that can store 
##the inversed matrix calculated from 'x' matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## 'cacheSolve' : This second function uses the output of the precedent function.
## It returns the inverse of the matrix 'x' if already stored in the cache created with 
##the previous function.
## If this cache is empty, it calculates the inverse of x and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
