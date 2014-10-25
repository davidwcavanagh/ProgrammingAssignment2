## These two functions together will calculate the inverse of a matrix
## and store the result for later recall so the computation does not need
## to be performed repeatedly.

## makeCacheMatrix() will create a list of 4 sub-functions to be performed
## and later recalled by the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {    ##Stores the data and assigns NULL to variable m
                x <<- y
                m <<- NULL
        }
        get <- function() x     ##Displays the stored data
        setinverse <- function(solve) m <<- solve       ##Solves for inverse and assigns result to m
        getinverse <- function() m      ##Displays the stored inverse
        list(set=set, get=get,          ##Creates a list of the 4 sub-functions
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve will check if there is a stored inverse for the data and display it.
## If the inverse is not already stored, it will calculate it and store it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()     ##Calls "getinverse" from makeCasheMatrix and stores in m
        if(!is.null(m)) {       ##If m is not NULL, the inverse will be printed from memory
                message("getting cached data")
                return(m)
        }
        data <- x$get()         ##Calls "get" from makeCasheMatrix and stores as data
        m <- solve(data, ...)   ##Calculates the inverse of data
        x$setinverse(m)         ##Stores result of line 32 for future use in lines 27-29
        m                       ##Prints the inverse
}
