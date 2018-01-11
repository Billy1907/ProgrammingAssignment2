## The functions makeCacheMatrix and cacheSolve are saving
## and reusing cached inverted matrix. Only square matrices can be inverted.

## Testing steps:

## 1. To create an invertible square matrix, use random numbers. For instance:
## randomMatrix <- matrix(rexp(4^2), 4)

## 2. To create a list with functions, assign as following:
## makeInverted <- makeCacheMatrix(randomMatrix)

## 3. To create cashed inverted matrix, use function cacheSolve:
## invertedMatrix <- cacheSolve(makeInverted)
##
## 4. To test if it was cashed, call the same function again.
## invertedMatrix <- cacheSolve(makeInverted)


## Preparing object for setting and getting a matrix and inverted matrix

makeCacheMatrix <- function(x = matrix()) {

        inv_x <- matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
        set <- function(y) {
                x <<- y
                inv_x <- matrix(data=NA, nrow=1, ncol=1, byrow = FALSE, dimnames = NULL)
        }
        get <- function() x
        setinverse <- function(solve) inv_x <<- solve
        getinverse <- function() inv_x
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Read cached inverted matrix or create new
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

        inv_x <- x$getinverse()
        if( !all(is.na(inv_x))) {
                message("Getting inverted matrix from the cache and printing the 1st row.")
                print(inv_x[1,])
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)
        inv_x
}
