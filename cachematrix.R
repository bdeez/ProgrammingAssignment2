## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#BDieser Comment: 'makecachematrix' creates a cache object and sets the value.

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        setmatrix <- function(b) { #sets the value of the matrix
                x <<- b #caches the matrix x
                a <<- NULL #default value       
        }
        getmatrix <- function() x #stores matrix x
        setinverse <- function(solve) a <<- solve #sets inverse function
        getinverse <- function() a #stores inverse matrix
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

#BDieser Comment: 'cachesolve' calculates the inverse of the matrix set in 'makecachematrix'. 

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        a <- x$getinverse() #set inverse if already calculated
        if(!is.null(a)) { #if inverse calc null, proceed to cache
                message("getting cached data")
                return(a)
        }
        b <- x$getmatrix() #get value of input matrix
        x$setmatrix(b) #cache matrix
        a <- solve(b, ...) #calc the inverse of the cache matrix
        x$setinverse(a) #cache inverse
        a #return inverse matrix
}
