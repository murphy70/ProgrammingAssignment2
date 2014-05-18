## These functions calculate the inverse of a matrix. In case the inverse was 
## already calculated, it will get the inverse matrix directly from the 
## cached data.

## As the example given in the instructions, this first function, 
## "makeCacheMatrix" creates a matrix, that is a simple list containing
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) m <<- solve
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)

}


## This function calculates the inverse of the matrix created with the above
## function. First it checks whether the inverse matrix was already calculated,
## if yes, it retrieves the value from the cached data. In case it was not 
## calculated before, it will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
}
