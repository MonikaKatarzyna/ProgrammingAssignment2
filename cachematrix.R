## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL ## sets matricx m to empty locally
        set <- function(y) {
                x<<- y ## sets the value of matrix x in the environment in which the function is defined (global environemnt in this case) 
                m<<- NULL ## sets matrix m to empty in the environment in which the function is defined (global environemnt in this case)
        }
        
        get <- function() x        
        setinver <- function(solve) m <<- solve ## sets the value of the inverse of the matrix
        getinver <- function() m ## gets the value of the inverse of the matrix
        list(set=set, get=get,
             setinver=setinver,
             getinver=getinver)
}


## the function calculates the inverse of the "special" matrix created with the above function.
## It first checks if the inverse has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinver function.

cacheSolve <- function(x, ...) {
        m <- x$getinver()
        ## checks if the inverse has been calculated, if yes, you get the message "getting cache data"
        if(!is.null(m)) {
                message("getting cache data")
                return m
        }
        ## if the inverse has not been calculated and is not in cache, it is calculated here with solve function
        data <- x$get()
        m <- solve(data,...)
        x$setinver(m)
        m     ## Returns a matrix that is the inverse of 'x'
}
