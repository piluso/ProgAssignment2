#################
#makeCacheMatrix#
#################

##Note that it takes parameter x, which is set simply as an empty matrix
##This function creates a list that will let us set and get both
##the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##Setting the value of the vector via function set, which 
  ##simply assigns y to parameter x and m in a different environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##getting the value of the matrix x
  get <- function() x
  ##We set m as the inverse of the matrix x
  setsolve <- function(solve) m <<- solve
  ##We "summon" the inverse
  getsolve <- function() m
  ##Updating step to pull objects with $
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
############
#cacheSolve#
############

##This function takes the cached matrix made 
##with MakeCacheMatrix as parameter x
##and returns its inverse
cacheSolve <- function(x, ...) {
  
  ## m is obtained by summoning the "get the inverse"
  ##object from the MakeCacheMatrix  
  m <- x$getsolve()
  ##If the inverse exists, we return it  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##data is assigned as the cached matrix
  data <- x$get()
  ##m is the calculation of the inverse
  m <- solve(data, ...)
  ##we make set the inverse in the makeCacheMatrix
  ##function to the inverse we calculated above
  x$setsolve(m)
  m
}

###################################
#Additional: A simple verification#
###################################
##Let's check our work. Create an invertible matrix A

A<-matrix(c(1,3,3,4,7,6,7,8,9), nrow=3, ncol=3, byrow=TRUE)
##Let's make the list for the cached matrix and inverse
##and store it in an object called z

z<-makeCacheMatrix(A)

##Let's get the cached inverse with the cachesolve

cacheSolve(z)

##Note that the value above should be the same as
##if we simply get the inverse of A
solve(A)
##It checks out, so we're done.