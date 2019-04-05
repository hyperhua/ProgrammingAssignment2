### function makeCacheMatrix was modified from course assignment example "makeVector"
### It is a function including set of function with an inside feeding matrix. It had 
### 1.set() our feeding matrix 
### 2.get() the feeding matrix 
### 3.setInv() the inverse matrix 
### 4.getInv() get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function() m <<- solve(x)
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
} ##set = set, 


## cacheSolve use "solve" function to return the inverse matrix ###
## modified based on the course code 'cachemean'

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cache matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
        
}


#### Test code ###
#> test <- makeCacheMatrix()
#> test$set(matrix(c(1,10,0.1,2),ncol=2))
#> test$get()

#> test$getInv() ### return NULL
### showing the result ###
###### [,1] [,2]
##[1,]    1  0.1
##[2,]   10  2.0
#> test$setInv()
#> test$getInv()

#> cacheSolve(test)
### showing the result ###
##getting cache matrix
##     [,1] [,2]
##[1,]    2 -0.1
##[2,]  -10  1.0

