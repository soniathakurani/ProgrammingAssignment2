library(matlib)
makeCacheMatrix <- function(x = numeric())
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#The following function calculates the mean of the special "vector" 
#created with the above function. However, it first checks to see if the mean has already been 
#calculated. If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the 
#setmean function.



cacheSolve <- function(x,...) {
  #x<-as.data.frame(t(x))
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #x<-as.data.frame(t(x))
  data <- x$get()
  m <- mean(data, ...)
  x$setinv(m)
  m
}
mymatrix<-matrix(c(2,3,4,5),ncol = 2)
mymatrix2<-matrix(c(6:9),ncol = 2)

newmatrix <- makeCacheMatrix(mymatrix)
newmatrix$get()    
newmatrix$getinv()           # retrieve the value of m, which should be NULL
newmatrix$set(mymatrix2)          # reset value with a new vector
cacheSolve(newmatrix)          # notice mean calculated is mean of 30:50, not 1:10
newmatrix$getinv()
cacheSolve(newmatrix)


# mean(x)
# solve(x)
g<-matrix(c(2,3,4,5),ncol = 2)

h<-c(3:8)
h
# g
# inv(g)
new<-makeVector1(1:10)
new
cacheinv(h)
