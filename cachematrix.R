## Code to perform Matrix Inversion
## Assumption is that the matrix supplied is always invertible.
## Code adapted from makeVector and cacheMean from Assignment 2 Coursera R 
## Programming 
## 
## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 1.  set matrix
## 2.  get matrix
## 3.  set inverse
## 4.  get inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
## Using newly learnt "<<-" operator which can be used to assign a value to an 
## object in an environment that is different from the current environment
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
## Load im from first function and do IF check if inversion has been performed.         
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
## ELSE use Solve() function to calculate the inverse
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
