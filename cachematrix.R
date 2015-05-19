## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #sets the value of inverse to NULL
        set <- function(y) {
                x <<- y #substitutes x as y in the makeCacheMatrix function
                inv <<- NULL #substitutes the val of inv to NULL in the makeCacheMatrix function
        }
        get <- function() x #returns the value of x in the makeCacheMatrix function
        setinv <- function(inverse) inv <<- inverse #stores the value of the inverse into inv
        getinv <- function() inv #returns the value of inv
        list(set=set, get=get, setinv=setinv, getinv=getinv) #store all the functions in the function makeCacheMatrix
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #apply function getinv() : stores any previous calculation of inverse to inv
        if (!is.null(inv)) { #checks whether inv is not null
                message("getting cached matrix") #if inv is not null, this message is printed
                return(inv) #and the value of inv is returned
        }
        mat <- x$get() #apply function get() : if inv is null, then the value of the matrix is obtained and stored in mat
        inv <- solve(mat) #computes inverse of matrix and stores it in inv
        x$setinv(inv) #apply function setinv : stores the inverse of the matrix in inv
        inv #Returns the inverse of x
        ## Return a matrix that is the inverse of 'x'
}
