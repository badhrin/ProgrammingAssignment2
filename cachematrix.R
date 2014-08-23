# makeCacheMatrix is a constructor function that defines 4 other functions
# cacheSolve is a function that actually does the work required in this assignment i.e. generate 
#the inverted matrix OR retrieve it from cache if it already exists

#********************************************
#This function takes as input a numeric matrix and creates 4 functions that can be used to
# get or set the matrix itself or the inverse of the matrix. 
#********************************************
makeCacheMatrix <- function(mymtrx=matrix()) {
  
# This is just to set this variable to null, everytime this function is run
  invmtrx <- NULL
  
# This function is used to set the matrix to a specific value.
# The invmtrx variable will be reset to null, whenever the matrix itself is changed
  setmtrx <- function(mtrx){
    mymtrx <<- mtrx
    invmtrx <<-NULL
  }

# Simple function to print the matrix
  getmtrx <- function() {
    mymtrx
  }

# Simple function to assign the inverted matrix to the invmtrx variable
  setinvmtrx <- function(mtrx){
    invmtrx <<- mtrx
  }

# Simple function to print the inverted matrix
  getinvmtrx <- function() {
    invmtrx
  }

# This is an important to ensure that methods/functions above can be accessed from outside
  list(getmtrx=getmtrx,setmtrx=setmtrx, setinvmtrx=setinvmtrx, getinvmtrx=getinvmtrx)
}

#********************************************
# This function takes as input an existing object that was instantiated using the makeCacheMatrix
# It then verifies if the inverse matrix for the matrix contained within this object has already 
# been created and is cached. If it is cached, it retrieves this inverse matrix & prints it. If it 
# is not cached, then it generates the inverse matrix, sets this matrix using the set matrix method
# and also prints this inverse matrix
#********************************************
cacheSolve <- function(x,...){

# Gets the value of the cached inverted matrix into a variable.
# If there is nothing in cache, this will become NULL
  currinvmtrx <- x$getinvmtrx()

# Checks if the inverted matrix exists in cache. If it exists, then 
# the cached matrix is returned & a message to this effect is printed. 
# Note that 'return' exits the function, such that other lines are not executed
  if (!is.null(currinvmtrx)) {
    message("getting cached inverse matrix")
    return(currinvmtrx)
  }

# The following lines are executed only if the above 'IF' loop is skipped
# The latest matrix is retrieved from the object & the inverse of it generated using solve()
  currmymtrx <- x$getmtrx()
  currinvmtrx <- solve(currmymtrx)
# The generated inverted matrix is set into the object (i.e. cache) & is also returned
  x$setinvmtrx(currinvmtrx)
  currinvmtrx
}