cachemean <- function(x, ...) {
       m <- x$getmean()
       if(!is.null(m)) {
             message("getting cached data")
           return(m)
         }
      data <- x$get()
       m <- mean(data, ...)
       x$setmean(m)
       m 
   }
makeCasheMatrix <- function(x=matrix()) {
     inv <- NULL
    set <- function(y){
         x<<-y
         inv<<-NULL
       }
     get<-function() {x}
     setInverse <- function(inverse) {inv<<-inverse}
     getInverse <- function() {inv}
     list(set=set,get=get, setInverse=setInverse,getInverse=getInverse)
   }
cacheSolve <- function(x, ...) {
       inv <-x$getInverse()
      if(!is.null(inv)) {
             message("getting catched data")
             return(inv)
         }
       mat <- x$get()
       inv <- solve(mat,...)
     x$setInverse(inv)
   inv
   }
   > matrix1 <- makeCasheMatrix(matrix(3:6, nrow=2, ncol=2))
> matrix1$get()
     [,1] [,2]
[1,]    3    5
[2,]    4    6
> matrix1$getInverse()
NULL
> cacheSolve(matrix1)
     [,1] [,2]
[1,]   -3  2.5
[2,]    2 -1.5
