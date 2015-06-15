## These two functions should work together to get 
## the inverse of a given matrix from cache when it has
## been previously calculated, thus saving time. If not,
## the inverse will be calculated and saved into the cache

## makeCacheMatrix first sets and gets the given matrix, 
## then sets and gets the inverse,
## and finally returns the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
## As in the example first set the inverse of the
## matrix as NULL
  minv <- NULL
  
## Now, define the codes for the setter and getter
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x

## Here, define the codes to set and get the inverse
## of the given matrix
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is a function that 'looks' whether the inverse
## of a matrix has been previously calculated before 
## calculating it. If so, it returns the output from
## the cache

cacheSolve <- function(x = matrix(), ...) {
  minv <- x$getinv()
  
## Return the inverse of the matrix from cache if it 
## is not NULL (it has been previously calculated)
  if(!is.null(minv)) {
    return(minv)
    
## Otherwise, do it
  } else {
    z <- x$get()
    minv <- inv(z, ...)
    x$setinv(minv)
    minv
  }

## Finally, return the inverse of the given matrix
  return(minv)
}
