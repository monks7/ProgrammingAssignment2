## Functions

## This function stores the value of a square matrix in 
## the current enviroment

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- matrix()   ##inizialize a matrix with no values
        set <- function(y =matrix()) {
                x <<- y    ##assign the value of matrix y to x
                inv <<- matrix()   ##and store it to inv
        }  ##here, we define the methods for this function
        get <- function() { x }
        setinv <- function(x) { inv <<- x }
        getinv <- function() { inv }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)      
        
}



## This function returns a matrix which is the inverse of the original
## given in the makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()                  ##get the value 
        if(sum(!is.na(inv))>0) {           ##if the sum of the na's values is greater than one, then the matrix is empty                
                message("getting cached matrix")  ##retrieve the value of the cache
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)   ##store the inverse 
        ## Return a matrix that is the inverse of 'x'
        inv
}