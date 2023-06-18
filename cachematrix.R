# makeCacheMatrix is a function that returns a list of function which role is to store a matrix and cache the inverse matrix of it.
makeCacheMatrix <- function(x = numeric()) {
        # initializing inv as NULL
        inv <- NULL
        
        # set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # retrieve the stored matrix
        get <- function() {
                x
        }
        # cache the given argument
        setinv <- function(inverse) inv <<- inverse
        
        # get the cached value
        getinv <- function() inv
        
        # defining a list containing the function defined above
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


# cachesolve is a function that calculate the inverse of a matrix passed by makeCacheMatrix
cacheSolve <- function(x, ...) {
        # get the cached matrix
        inv<- x$getinv()
        
        # judging if the cached matrix is null
        if(!is.null(inv)) {
        
        # if cached matrix is not null, 
        # meaning that the makeCacheMatrix has already passed a value of the inverse matrix to inv, 
        # so the function will just retrieve the matrix stored without doing any calculation. 
                message("getting cached data")
                return(inv)
        }
        # otherwise, the function will get the matrix and calculate its inverse matrix.
        data <- x$get()
        inv <- solve(data)
        # then pass the inverse matrix to the inv,
        x$setinv(inv)
        # finally returns the inverse matrix.
        inv
}
