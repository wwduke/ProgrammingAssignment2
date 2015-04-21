## This file contains two functions. The first function creates a set of functions
## useful for managing a matrix and its inverse.  
## The second function, when called uses the first to store the solved matrix. 
## Neither function includes error trapping or handling. 

## makeCacheMatrix returns a set of functions to manage a matrix and its inverse
## The functions will return the original matrix or its inverse. They also make it
## possible to set the value of the identity matrix or to alter the original matrix

makeCacheMatrix <- function(x = matrix()) {
        #Start of with a NULL matrix that will later hold the identity matrix
        myMatrix <- NULL
        
        # Create my set function
        set <- function(y) {
                x <<- y         # put in a new value for the matrix and set identity to null
                myMatrix <<- NULL
        }
        
        # Create my get function -just return original
        get <- function() x
        
        # Create my identity creation function
        setIdentity <- function(ident) myMatrix <<- ident
        
        # Create my identity retriever
        getIdentity <- function() myMatrix
        
        # Finally, return a list of the newly created functions
        list(set = set, get = get, setIdentity = setIdentity, getIdentity = getIdentity)
}


## This function will check to see if the identity has been solved and cached.
## If so, myMatrix is non-Null and the function returns the solved value.
## If myMatrix is null, this functions solves for the identity and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myMatrix <- x$getIdentity()
        if(!is.null(myMatrix)){         # matrix previoulsy solved
                message("getting cached data")
                return(myMatrix)
        }
        
        # matrix has never been solved, let's do it now
        data <- x$get()
        myMatrix <- solve(data)
        x$setIdentity(myMatrix) # and cache the identity
        myMatrix
}
