## Functions to calculate the inverse of a matrix. The inverse is stored and can be used at any time 
## without further calculation


## Function that reads a matrix mtrx and returns 3 functions
## 1. getMatrix: returns the input matrix mtrx
## 2. setInverse: stores the value of the invers matrix of mtrx that is provided in an child environment
## 3. getInverse: returns the inverse matrix of mtrx

makeCacheMatrix <- function(mtrx = matrix()) {
    if (!(is.matrix(mtrx))) stop("Input must be a matrix")
    invmtrx <- matrix(NA, nrow = nrow(mtrx), ncol = ncol(mtrx))
    # print(dim(invmtrx))
    #setMatrix <- function()
    getMatrix <- function() mtrx
    setInverse <- function(allreadysolved) invmtrx <<- allreadysolved
    getInverse <- function() invmtrx
    list(getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve returns the inverse of a matrix provided by function makeCacheMatrix. 
## It calculates the inverse and stores it in the environment of function makeCacheMatrix or
## it reads the inverse from the environment of makeCacheMatrix, when it has been allready calculated

cacheSolve <- function(fmtrx, ...) {
    inv <- fmtrx$getInverse()
    # print(dim(inv))
    if (!all(is.na(inv))) {
        print("Inverse already calculated")
        return(inv)
    }
    mx <- fmtrx$getMatrix()
    inv <- solve(mx)
    fmtrx$setInverse(inv)
    inv
}
