# Programming in R, Assigment 2.

# makeCacheMatrix
# The makeCacheMatrix is a main function that stores the following list of functions: get, set, getinv and setinv.

makeCacheMatrix <- function(x = matrix()) {
	
# inv <- NULL  assigns to null the value of inv.
	inv <- NULL
	
# - get, returns the matrix x stored in the main function. 	
    get <- function() x
	
# - set, changes the matrix x stored in the main function to a new matrix M given.
#   This function is used only if we want to change the matrix x in the main function.
    set <- function(M){
    # x <<- M, substitutes the matrix x with M.
      x <<- M
    # Due to in the set function we are not interested anymore in matrix x and consequently in its inverse inv,
    # we will not need the value of inv, therefore inv<<-NULL restores to null the value of inv.
    # The new inverse matrix needs to be recalculated by the cacheSolve function.
      inv <<- NULL
    }
	
# setinv, stores the value of the input (solve) in a variable inv into the main function.
    setinv <-function(solve)   inv<<- solve

# getinv, returns the value of inv which was stored in setinv.
    getinv <-function() inv

# We use the function list in order to store the four functions get, set, setinv, getinv in makeCacheMatrix.
# Therefore any object that is assigned to makeCacheMatrix will have all the four functions.
	list(set=set, get= get, setinv= setinv, getinv=getinv)
}

# cacheSolve function
# Return a matrix that is the inverse of the 'x' matrix.
cacheSolve <- function(x, ...) {
# Input of cacheSolve is the object where makeCacheMatrix is stored.
  
#Firstly, the value of the function getinv() stored in makeCacheMatrix is assigned to the variable inv into cacheSolve.
    inv <- x$getinv()
	
# Secondly, cacheSolve verifies that the value inv exists and is not NULL.
# If inv exists in memory, cacheSolve simply returns <<message getting cached data>> and the value of inv,
# that is supposed to be the inverse of x, but not necessarily.
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
# If inv does not exist in memory, the variable data gets the vector stored with makeCacheMatrix by the function get(),
# which return is stored in a variable called data.
    data <- x$get()
# inv calculates the inverse of the matrix data.
    inv  <- solve(data, ...)
# x$setinv stores inv in the object generated assigned with makeCacheMatrix,
# that is by using the setinv() function.
    x$setinv(inv)
# Also inv is printed out.
    inv
}
