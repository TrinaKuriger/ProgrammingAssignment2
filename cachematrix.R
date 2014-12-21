## jkuriger
## Intro to R Programming - Coursera
## Assignment 2


## makeCacheMatrix() takes in a matrix and creates an object that stores the value of 
## that matrix and can store an additional value (in our case and inverse matrix) 
## that is associated with that matrix
##

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL     # the inv variable is created and set to null when the object  
                  # is created
  
  get <- function() { x }   # returns the original matrix
  
  set <- function(y) {      # takes a new matrix and assigns it to x
    x <<- y         
    inv <<- NULL            # resets inv to NULL when the matrix value is changed
  }
  
  setinv <- function(inver)  { inv <<- inver }  # sets the value of inv for the 
                                                # object using superassignment   
  
  getinv <- function() { inv } # returns the value of inv for the object
  
  list(get = get, setinv = setinv, getinv = getinv) # defines methods associated 
                                                    # with makeCacheMatrix object
  
}#end makeCacheMatrix function



## cacheSolve() takes in the object created by makeCacheMatrix() and returns its 
## inverse value. It obtains the inverse value by first checking to see if there is
## already one stored in makeCacheMatrix. If there is not one stored it will calculate 
## the value and store it in the makeCacheMatrix object. 

cacheSolve <- function(x, ...) {   # x must be a makeCacheMatrix object
  
  inv <- x$getinv()         # gets the value of inv for the object
  if(!is.null(inv)) {       # this loop runs if the value of inv is already stored
    
    message("getting cached data")  # reports that the value is from cache
    return(inv)                     # returns inv value and exits function 
  }
  
  # this code below is used if the inv value is not cached (i.e. is NULL)
  data <- x$get()       # gets the value of the matrix inside the object
  inv <- solve(data)    # calculates the inverse of the matrix using solve() function
  x$setinv(inv)         # sets the inv value inside the makeCacheMatrix object 
  inv                   # returns the inv value
}# end cacheSolve function
