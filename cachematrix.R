## This R script contains 2 functions that caches matrix inverse to save time and computational resources


## 1. Function makeCacheMatrix
## input: matrix (assumption - matrix must be invertible)
## output: saves input matrix as orgmat (original matrix)
##         initializes invmat (inverse matrix) as NULL
## lists internal functions: setoriginalmatrix, getoriginalmatrix, setinversematrix, getinversematrix (self explanatory)

## 2. Function cacheSolve
## input: matrix created with above function: makeCacheMatrix
## output: if inverse already cached, return inverse matrix
##         else compute inverse and cache inverse matrix

makeCacheMatrix <- function(orgmat = matrix()) 
{
    invmat <- NULL
    setoriginalmatrix <- function(y) ## set original matrix
    {
        orgmat <<- y
        invmat <<- NULL
    }
    
    getoriginalmatrix <- function() ## get original matrix
    {
        orgmat
    }
   
    setinversematrix <- function(matx) ## set inverse matrix
    {
        invmat <<- matx
    }
    
    getinversematrix <- function() ## get inverse matrix
    {
        invmat
    }
    
    list(setoriginalmatrix = setoriginalmatrix, 
         getoriginalmatrix = getoriginalmatrix,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)    
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinversematrix() ## get then, check if NULL
    if(!is.null(m)) 
    {
        message("getting cached inverse matrix")
        return(m) 
    }
    
    ## else...
    
    data <- x$getoriginalmatrix()
    m <- solve(data, ...) ## assumption - matrix is always invertible (omitted error handling)
    
    x$setinversematrix(m) ## set inverse matrix
    m
    
}
