
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
    
}


## This function creates a special "matrix" object that can cache its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
    
}

#Example
my_matrix <- matrix(1:4, 2, 2)
my_cache_matrix <- makeCacheMatrix(my_matrix)

my_inverse_matrix <- cacheSolve(my_cache_matrix)
r_base_inverse_matrix <- solve(my_matrix)

#Both methods give the same results
identical(my_inverse_matrix, r_base_inverse_matrix)

