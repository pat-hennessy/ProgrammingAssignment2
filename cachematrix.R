# Package to store a matrix, calculate its inverse, and
# cache that result for faster access after the first time



# Function to define operations on a matrix
makeCacheMatrix <- function(matx = matrix()) {
    
    # Initialize the inverse matrix to NULL so users will know
    # if it has been set 
    matxInv <- NULL
    
    # Matrix setter function
    setMatrix <- function(maty) {
        
        # Store input matrix to parent environment variable
        # so the getter function can see it
        matx <<- maty
        
        # Reset the inverse in the parent environment to NULL
        # since we have a new matrix
        matxInv <<- NULL
    }
    
    # Matrix getter function - return whatever was previously set
    getMatrix <- function() matx
    
    # Inverse setter function - store to parent for getter visibility
    setInverse <- function(inv) matxInv <<- inv
    
    # Inverse getter function - return whatever was previously set
    getInverse <- function() matxInv
    
    # Expose accessor function pointers
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Solver function to return the inverse of the matrix
# stored within the supplied makeCacheMatrix object
cacheSolve <- function(mcm, ...) {
    
    # Get the stored inverse
    matxInv <- mcm$getInverse()
    
    # If we have a cached value, just return it
    if(!is.null(matxInv)) {
        message("Returning cached matrix")
        return (matxInv)
    }
    
    # If not, get the matrix, calculate its inverse, and cache the result
    data <- mcm$getMatrix()
    matxInv <- solve(data, ...)
    mcm$setInverse(matxInv)
    
    # Return the calculated inverse
    matxInv
}
