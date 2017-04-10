## makeCacheMatrix function will create matrix object
## naming the list allows use of $ commands
## such as resetting matrix object with another input matrix

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(z) n <<- z
        getinverse <- function() n
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function will return inverse matrix of the input matrix
## cacheSolve function will use cached data if input matrix is not changed

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        matrix1 <- x$get()
        n <- solve(matrix1)
        x$setinverse(n)
        n
}
