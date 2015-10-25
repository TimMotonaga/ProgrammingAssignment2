##makeCacheMatrix() takes a nonsingular matrix as input and produces a list
##as output. If this list is plugged into cacheSolve() then we are given the
##inverse of the original matrix. If the inverse of the matrix was previously
##computed by cacheSolve(), then it reads the inverse from the cache rather 
##than computing it.

##More precisely, when a nonsingular matrix is plugged into makeCacheMatrix(),
##a list of functions is produced: (get,set,getinverse,setinverse). The get() 
##function gets the stored value of the matrix. The set() function sets the 
##value of the matrix.The getinverse() function gets the stored value of the 
##inverse. The setinverse() function sets the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
    get <-function() x
    set <-function(y) {
        x <<- y
        i <<-NULL
    }
    setinverse <- function(inverse) i<<-inverse
    getinverse <- function() i
    list(set=set,get=get,setinverse = setinverse,getinverse=getinverse)
}


##cacheSolve() takes as input a list produced by makeCacheMatrix(). It then
##either:
##
## 1) Gets the value of the inverse of the original matrix from memory and 
##returns it, if the inverse was previously calculated.
##
## 2) Calculates the inverse using Solve() and returns it, if the inverse was
##    not previously calculated.

cacheSolve <- function(x) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
