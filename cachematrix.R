## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function_

makeCacheMatrix <- function(x = matrix()) {
        inverse_x<-NULL 
        set <- function(y) {        ## this function is initializing the cache
                x <<- y
                inverse_x <<- NULL
        }              
                get <- function() x ## this one is loading the matrix from the input
                #the next function will load the inv matrix into the cache
                setinverse <- function(inverse_in) inverse_x <<- inverse_in 
                ##the next one is extracting inversed matrix from the cache
                getinverse <- function() inverse_x 
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, corr.check=FALSE, prec=1.e-8,...) {
        ## Return a matrix that is the inverse of 'x'
        ## x- input matrix, iverse_x inverse matrix 
        ## corr.check - do I need to check if the cached matrix is the right one?
        ## prec - tolerance
        inverse_x <- x$getinverse()
        if(!is.null(inverse_x)) {   ##if there is o cached matrix
                z<-TRUE
                if(corr.check){
                ## matrix multiplication and comparison with identity matrix
                data <- x$get()
                z<-(max(abs(inverse_x%*%data-diag(dim(data)[1])))<prec)  
                }
                if(z){
                message("getting cached data")
                return(inverse_x)
                }
        }
        data <- x$get()
        inverse_x <- solve(data, ...)  ## generating an inverse matrix
        x$setinverse(inverse_x)  ##loading into the cache
        inverse_x
}

# TEST_1 z should be very close to 0.
## generating the matrix
x<-matrix(c(1,2,3,4,5,6.5,7.1,8.6,1, 12, 5,7,3.5, 8.9), nrow=4,ncol=4)
##cacheing functions
yy<-makeCacheMatrix(x)
yy$get()
##inversing for th efirst time
x_inv<-cacheSolve(yy)
x_inv
##checking wheather the cache is really used, no correctnes ckeck
cacheSolve(yy)
##external correctness check
z<-max(abs(x_inv%*%yy$get()-diag(dim(x)[1])))
z
##the same witht he correctness ckeck reasonable tolerance
yy_1<-cacheSolve(yy, TRUE, prec= 1.e-9)
yy_1
## ... the tolerance will be now set too low, should recalculate
yy_2<-cacheSolve(yy, TRUE, prec= 1.e-19)
yy_2
##
        


