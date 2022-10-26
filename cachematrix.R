## Both functions used to solve Assignment of week 3 on Coursera

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# The inverse has already been calculated (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Getting Inverse or calculate it
        i <- x$getInverse()
        if(!is.null(i)) {
                message("Getting cached inverse")
                return(i)
        }
        matrix <- x$get()
        
        # Checking for square matrix
        if((dim(matrix)[1] != dim(matrix)[2])){
                message("Matrix not squared, not possible to invert")
        }
        
        i <- solve(matrix)
        x$setInverse(i)
        i
}

## Testing the functions above, with a simple 2x2 matrix
mat = matrix(1:4, 2, 2)
cache_matrix = makeCacheMatrix(mat)
result = cacheSolve(cache_matrix)
print(result)