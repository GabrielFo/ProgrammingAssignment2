
##This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initially set to NULL
        # Changes when the user sets the value
        inv <- NULL
        
        # set function
        # Sets the matrix itself but not the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get function
        # Gets the matrix itself but not the inverse
        get <- function() x
        
        # Manually set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Get the inverse
        getinverse <- function() inv
        
        # Encapsulate into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Get the current state of the inverse and see if it
        # has been computed yet
        inv <- x$getinverse()
        
        # If it has...
        if(!is.null(inv)) {
                # Simply return the computed inverse		
                message("Getting cached matrix")
                return(inv)
        }
        
        # If it hasn't...
        # Get the matrix itself
        data <- x$get()
        
        # Find the inverse
        inv <- solve(data, ...)
        
        # Cache this result in the object
        x$setinverse(inv)
        
        # Return this new result
        inv    
}