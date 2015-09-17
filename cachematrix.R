## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function create a special object that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverse) inv <<- inverse
        getInverseMatrix <- function() inv
        list(set = set,
             get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## This function compte the inverse of matrix x.
## If the inverst already been calculated then it will retrive it from the cach.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverseMatrix()
        if (!is.null(inv)) {
                message("getting inverse matrix from cache!")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverseMatrix(inv)
        inv        
}