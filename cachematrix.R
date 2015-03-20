## Functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    valMtx <- NULL
    set <- function(y){
        x <<- y
        valMtx <<- NULL
    }
    get <- function() x
    setMtx <- function(Mtx) valMtx <<- Mtx
    getMtx <- function() valMtx 
    list(set = set, get = get, setMtx = setMtx, getMtx = getMtx)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    valMtx <- x$getMtx()
    if(!is.null(valMtx )){
        message("getting cached data")
        return(valMtx)
    }
    data <- x$get()
    valMtx <- solve(data)
    x$setMtx(valMtx )
    valMtx
}
