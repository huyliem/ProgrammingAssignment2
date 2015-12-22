## This function is able to cache potentially time-consuming computations. 
## Taking the inverse of a matrix is typically taking a lot of time. If the 
## matrix are not changing, it may cache the value of the inverse so that when 
## we need it again, it can be looked up in the cache rather than recomputed. 

# Function makeCacheMatrix creates a matrix
# which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Function cacheSolve calculates the inverse of the matrix created with the 
# above function. However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the matrix and sets the 
# value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
