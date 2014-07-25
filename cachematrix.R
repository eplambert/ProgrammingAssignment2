## makeCacheMatrix and cacheSolve check to see if the previous input matrix has been inverted and if so
## return the cached inverted matrix. If the inverted matrix is different then compute the inverted matrix


## makeCacheMatrix returns a list to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

## NOTE: Problem states that the input WILL BE a matrix that is able to be inverted
## therefore no error handling for non-invertible matrices was constructed

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL                               ## inv stores inverted matrix
        ## set values
        set <- function(y) {
                x <<- y                         ## input matrix x is assigned to y
                i <<- NULL
        }
        ## get input matrix
        get <- function() x
        ## set inverse of matrix
        setinverse <- function(inverse) i <<- inverse
        ## get inverse of matrix
        getinverse <- function() i
        ## return a list of the above described four variables
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve first checks to see if the inverse as already been calculated if it has
## the cached inverse of the matrix is recalled. If the inverse of the matrix is not in memory
## then the inverse matrix is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()                     ## get the previously inverted matrix
        if(!is.null(i)) {                       ## check if the previously returned inverted matrix is the same
                message("getting cached data")
                return(i)                       ## return the previously stored inverted matrix
        }
        data <- x$get()                         ## get the input matrix
        i <- solve(data, ...)                   ## performs matrix inversion on input matrix
        x$setinverse(i)                         ## assigns new inverted matrix to setinverse to be stored in cache
        i                                       ## return the inverted matrix
}
