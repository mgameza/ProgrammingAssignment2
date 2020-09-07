# These functions try to create an special matrix object (functions and variables)
# that be used to store the functions required to calculate y keep in memory 
# the inverse of a matrix, reducing the calculating time.


# Function to create the special "matrix" object that contains not only the functions needed:
# set, get, setsolve and getsolve, but also the variables required:
# x (the result searched) and m (the data, in this case, the matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# Function to calculate the inverse of the matrix 
# Depending on if the inverse was calculated previously, 
# the function obtains the inverse of the data structure in memory (cache).
# Otherwise, it calculate the inverse using the matrix stored in memory. 

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
