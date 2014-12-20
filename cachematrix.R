## Put comments here that give an overall description of what your
## functions do
##
## We define two functions to work on objects of type "cachematrix" here. The type name
## here is informal.
##
## makeCacheMatrix is the constructor of this object type, constructing the object given
## a matrix as its input.
## cacheSolve is a method that returns the cached inverse of the matrix in the object if
## it exists, and calculates, stores, and returns it if it hasn't been cached yet.

## Write a short comment describing this function
##
## makeCacheMatrix takes matrix returns (pseudo)struct (emulated via a list)
##
## Methods
## set: sets the matrix
## get: retrieves the matrix
## setInverse: sets the member inverse
## getInverse: returns the member inverse
##
## Members
## inverse (private)
##
## Description: This function takes a matrix and creates a "cachematrix" object out of it,
## with the property "inverse" that can be used to store the inverse of the given matrix
## and associates with it the necessary functions for storing and retrieving the
## given matrix and its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(tempMatrix) {
    matrix <<- tempMatrix
    inverse <- NULL
  }
  get <- function() matrix
  setInverse <- function(tempInverseMatrix) inverse <<- tempInverseMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##
## Description: This function takes an object of type "cachematrix", checks if the
## inverse of the matrix in it has already been calculated and stored, and returns the
## stored value if it is. Otherwise, it calculates and stores the inverse in the object
## for future use and returns the calculated inverse.
##
cacheSolve <- function(matrix, ...) {
  ## Return a matrix that is the inverse of the input cached matrix
  inverse <- matrix$getInverse()
  if(!is.null(inverse)) {
    message("Cached data found. Retrieving cached data.")
    return(inverse)
  }
  tempMatrix <- matrix$get()
  inverse <- solve(tempMatrix)
  matrix$setInverse(inverse)
  inverse
}
