
#Function makeChachMatrix() stores (in an environment separate from current)
#the matrix passed to it as an argument and its inverse.
#The function returns returns a list of functions
#that can be used to access and modify them (in the environment
#they are stored). The returned list looks like this:
#[set(), get(), setinverse(), getinverse()]
#set() is used to modify the matrix matrix, 
#after this function is used, inverse is deleted
#get() is used to read the stored matrix
#setinverse() is used to modify the inverse
#getinverse() is used to read the inverse


makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
    x <<- y
    i <<- NULL
    }
get <- function() x

setinverse <- function(inverse) i <<- inverse
getinverse <- function() i

list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Function cacheSolve() is used to check whether an inverse of a matrix
#that was created with makeCacheMatrix() function is stored in cache mamory.
#If so, it returns the cached data. If not it calculates the inverse, saves
#it in cache memory and returns it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
