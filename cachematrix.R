#1) Matrix inversion can be computationally demanding especially for large sized matrices
#2) So it is good to cash in the solution for the inversion of a matrix instead of calculating it over and over again. 

## Write a short comment describing this function
#makeCacheMatrix creates a list of functions to set and get the value of of the inverse of matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function()x
        setInverse <- function(solve) m <<-solve
        getInverse <- function()m
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse)

}


#CacheSolve takes a the inverse of a matrix as an input, and checks whether the value has been calculated previously. 
#if it has been created, then it returns the value. If the inverse was not calculated then it calculates it. And that is
#how we save computational time and complexity. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data for the inverse of the matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
