## This function creates a special "matrix" object that can cache its inverse.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## get the value of the matrix
        setInversa <- function(inversa) m <<- inversa ## set the value of the solve     
        getInversa <- function() m ## get the value of the solve
        list(set = set, get = get,
             setInversa = setInversa,
             getInversa = getInversa)        
}


## This function returns a matrix that is the inverse of 'x'

## The function used to obtain the inverse of the matrix is "solve"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInversa()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## Arguments: data = square numeric or complex matrix containing the coefficients of the linear system.
        ## ... = further arguments passed to or from other methods
        x$setInversa(m)
        m                
}
