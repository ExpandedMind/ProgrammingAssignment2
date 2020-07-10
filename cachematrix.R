## Put comments here that give an overall description of what your
# Below functions help long matrix inverse computations  by caching result
# of first calculated inverse of a given matrix, returning this cached result
# next time a user asks for the inverse version of a given matrix.

## Creates a custom list object with methods to access a matrix and its inverse
## version

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    getFunction <- function() x
    setFunction <- function(new_matrix) {
        x <<- new_matrix
        inverse_matrix <- NULL
    }
    getInverseMatrix <- function() inverse_matrix
    setInverseMatrix <- function(calculated) inverse_matrix <<- calculated
    list(setMatrix = setFunction,
         getMatrix = getFunction,
         getInverse = getInverseMatrix,
         setInverse = setInverseMatrix)
}


# Returns inverse matrix of provided one as parameter, looking first for a 
# cached calculation. If cache is missing, then inverse matrix calculation is
# performed.
cacheSolve <- function(x, ...) {
    inverse_matrix <- x$getInverse()
    if(!is.null(inverse_matrix)) {
        message("Using cached inverse matrix")
        return(inverse_matrix)
    }
    inverse_matrix <- solve(x$getMatrix(), ...)
    x$setInverse(inverse_matrix)
    return(inverse_matrix)
}
