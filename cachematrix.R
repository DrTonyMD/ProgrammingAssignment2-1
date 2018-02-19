##  Programming assignment - week 3
## 

## This creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set <- function(y) {
          x<<-y
          m<<-NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Computes the inverse of the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m<- x$getinverse()
     if(!is.null(m)){
          message("getting cached inverse matrix")
          return(m)
     }
     data<- x$get()
     m<- solve(data)
     x$setinverse(m)
     m
}

## The problem comes in when one tries to test the functions. 
## If you simply call makeCacheMatrix without assigning it to a matrix object, 
## and then try to call cacheSolve on the same matrix, an error is generated:
##      
##      "Error in x$getinverse : $ operator is invalid for atomic vectors"
## 
## This drove me crazy, and should not have been part of the assignment. 
## The trick is to assign the return of  makeCacheMatrix to another matrix, 
## and then execute cacheSolve on that matrix (leaving your original matrix intact, 
## by the way:). 
## 
## I could still use more explanation surrounding this code, especially 
## for generating the special matrix object.
## 