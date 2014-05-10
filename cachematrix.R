## Creates a special 'matrix' which is really a list containing a function to
## set the value of the matrix called set
## get the value of the matrix called get
## set the value of the inverse called setInverse
## get the value of the inverse called getInverse

makeCacheMatrix <- function(x = matrix()) {        
        inverse<-NULL                
        set<-function(y) {
                x<<-y
                m<<-NULL
        }                
        get<-function() x        
        setInverse<-function(inv) inverse<<-inv
        getInverse<-function() inverse
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Given that special 'matrix' object created using makeCacheMatrix function,
## computes its inverse and stores it inside that object if its not been computed before
## if its computed before it returns the cached result from that object


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInverse(inv)
        inv
}
