## makeCacheMatrix() creates a special "matrix" object that stores as attributes its own value and its inverse.
## Just after creation the special "matrix" object does not stores its inverse value and thus cacheSolve() fucntion must be called to get the inverse.
## cacheSolve() called for the first time for a special "matrix" object computes the inverse of the special "matrix".
## Upon subsequent calls of cacheSolve() for the same special "matrix" object (provided the underlying matrix value has not been modified)
## returns the inverse value from Cache without doing any computations for inverse.  


## This function creates a special "matrix" object that stores as attributes its own value and its inverse.
## The special "matrix" object is of type 'list', which exposes functions to access and mutate its attributes.

makeCacheMatrix <- function(originalMatrix = matrix()) {  # variable 'originalMatrix' stores the actual matrix object with values
    
    inverseOfMatrix <- NULL  # variable 'inverseOfMatrix' stores the inverse of matrix stored by 'originalMatrix' attribute
                             # at initial call of the method its always set to NULL.
    
    setMatrix <- function(newMatrix) {  # allows changing of the matirx stored in attribute 'originalMatrix'
        
        originalMatrix <<- newMatrix  # updates the 'originalMatrix' attribute with new matrix.
        
        inverseOfMatrix <<- NULL  # resets the 'inverseOfMatrix' to null to take care of scenario when there is any change in the 'originalMatrix' attribute.
        
    }
    
    getMatrix <- function() {  # returns the matrix refferenced by 'originalMatrix' attribute.
        originalMatrix
    }
    
    setInverseOfMatrix <- function(calculatedInverseOfMatrix) {  # sets the inverse of matrix
        
        inverseOfMatrix <<- calculatedInverseOfMatrix
    }
    
    getInverseOfMatrix <- function() {  # returns the matrix refferenced by 'inverseOfMatrix'
        
        inverseOfMatrix
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseOfMatrix = setInverseOfMatrix,
         getInverseOfMatrix = getInverseOfMatrix)  # sets refferences to the accessor and mutator functions.

}


## This funtion returns the cached inverse of the special "matrix" object (returned by makeCacheMatrix function) if available.
## In case the special "matrix" object does not have its inverse stored as an attribute,
## this function computes the inverse of the special "matrix" and sets it back to the special "matrix" object.

cacheSolve <- function(specilMatrixObject, ...) {
    
    cachedInverseOfMatrix <- specilMatrixObject$getInverseOfMatrix()  # reads the inverse of the sepcial "matrix" object. 
    
    if( ! is.null(cachedInverseOfMatrix)){  # the 'if' block returns the 'cachedInverseOfMatrix' if its not null in the special "matrix" object
                                                    
        message("returning value from cache")
        return(cachedInverseOfMatrix)  # returns the cached inverse of matrix and halts further processing of the function.
    }
    
    originalMatrix <- specilMatrixObject$getMatrix()  # gets the original underlying matix
     
    calculatedInverseOfMatrix <- solve(originalMatrix)  # computes the inverse of a matrix
    
    specilMatrixObject$setInverseOfMatrix(calculatedInverseOfMatrix)  # sets back the computed inverse of the matrix to the special "matrix" object
    
    calculatedInverseOfMatrix  # returns the calculated inverse of the matrix.
}
