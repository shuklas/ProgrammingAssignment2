## This function returns a list containing the 
## getter/setter and functions to set/get inverse

makeCacheMatrix <- function(x = matrix()) {
   m<-NULL
   set <- function(y){
           x <<- y
           m<<-NULL
   }
   get <-function() {x}
   setInverse<- function(inverse) m<<-inverse
   getInverse<- function() {m}
   list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function returns the inverse of the matrix.
## x - a list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getInverse()
        if(!is.null(m))
        {
                message("Getting cached data ")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setInverse(m)
        m
}
