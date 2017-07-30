## Put comments here that give an overall description of what your
## functions do

## The following function makeCacheMatrix  is generating a set of functions in order to
## 1. populate the matrix
## 2. populate the cache with the inverse matrix, when inversion is done the first time
## 3. check whether the inverse matrix already exists and check if this is the right one
##  (if the original matrix has not been changed) and return it as a results
## 4.  get the data for inverting

## The function cacheSolve checks the inverse for existance and 
## inverts the matrix if the inverse one does not exist or the original has been changed 


makeCacheMatrix <- function(x = matrix()) {
        inverse_x<-NULL 
        set <- function(y) {        ## this function is initializing the cache
                x <<- y
                inverse_x <<- NULL
        }              
                get <- function() x ## this one is loading the matrix from the input
                ##the next function will load the inv matrix into the cache
                ## IMPORTANT!! - the data is taken populated from the CALLING program
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
                } else 
                ## if the inverse and original do not match (the matrix has been changed)
                {print("The matrix has been changed, inverse is recalculated")}
        }
        data <- x$get()
        inverse_x <- solve(data, ...)  ## generating an inverse matrix
        x$setinverse(inverse_x)  ##loading into the cache
        inverse_x
}

# TEST_1 

xx<-matrix(c(1,2,3,4,5,6.5,7.1,8.6,1, 12, 5,7,3.5, 8.9, 3.1, 1.2), nrow=4,ncol=4)
xx  ## generated matrix

yy<-makeCacheMatrix(x) ##caching functions
yy$set(x)   ## setting matrix x
yy$get()
x

x_inv<-cacheSolve(yy) ##inversing for the first time
x_inv

cacheSolve(yy)  ##checking wheather the cache is really used, no correctnes check

z<-max(abs(x_inv%*%yy$get()-diag(dim(x)[1]))) ##external correctness check
z ## z should be very close to 0 (within the tolerance)

yy_1<-cacheSolve(yy, TRUE, prec= 1.e-9) ##the same with the internal correctness check
yy_1 ##should take the cache

yy_2<-cacheSolve(yy, TRUE, prec= 1.e-19) ## the tolerance will be now set low, should recalculate
yy_2

        


