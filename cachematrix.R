## The functions below have been submitted to Coursera for grading
## as a requirement for the R Programming Course (Data Science Certification)
## Developer: Brian MacConney
## Developed on: 5/15/2016
##
## Put comments here that give an overall description of what your
## functions do
##
## ************NOTE: TEST RESULTS ARE BELOW THE LAST FUNCTION MODULE************
##
## This module contains two functions:  
##
## Requirement: Write a short comment describing this function
##
## Function 1 (makeCacheMatrix):
##    Purpose: Load initial matrix into memory (Global Environment)
## This function caches the existing inverse to the matrix until the matrix changes
## Per the assigment, "This function creates a special "matrix" object that can cache its inverse."
##
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
##
## Requirement: Write a short comment dcaescribing this function
## Function 2 (cacheSolve)
##    Purpose: Check to see if calculation has occurred (in the Global Environment).
##        If yes, then provide a message box
##        If no, then calulate the inverse (solve function)
##           
## This function returns the inverse of matrix x -- for each unique matrix calculated.  
## This function refers to makeCacheMatrix to determine the uniqueness of the function call.
## Per the assignment notation, "This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache."
##
cacheSolve <- function(x, ...) {
  #Return a matrix that is the inverse of x.
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##
## ************NOTE: TEST RESULTS ARE BELOW THE LAST FUNCTION MODULE************
## Step 1: Load makeCacheMatrix and cacheSolve into memory.
##    Expected Result: Functions load without error.
## Step 2: Create a matrix using the following script in the R Console.
##    Matrix Def:  > test_matrix <- makeCacheMatrix(matrix(c(1,3,2,4),2,2))
##    Expected Results: 
##      (1) There are no errors in the console.
##      (2) R will crate a 2x2 matix named test_matrix using the makeCacheMatrix function.
##      Note: This matrix is availabe at http://www.mathwords.com/i/inverse_of_a_matrix.htm
## Step 3: Check the test_matrix object using test_matrix$get().
##    Expected Result: test_matrix$get() will display the loaded values from Step 2.
##    Actual Result:
##                 [,1] [,2]
##            [1,]    1    2
##            [2,]    3    4
## Step 4: Identify the class of the matrix test_matrix using class(test_matrix).
##    Expected Result: List
##    Actual Result: 
##      [1] "list"
## Step 5: Test the contents of test_matrix$getInverse().
##    Expected Result: R should indicate that this value is NULL because no inverse calculation has occurred.
##    Actual Result: NULL
## Step 6: Execute the cacheSolve function using cacheSolve(test_matrix).
##    Expected Result: R will calculate the inverse function to test_matrix.
##    Result: The inverse function is applied to test_matix as indicated below:
##                 [,1] [,2]
##            [1,] -2.0  1.0
##            [2,]  1.5 -0.5
##    Note:  Calculation can be verified at http://www.mathwords.com/i/inverse_of_a_matrix.htm
## Step 7: Now check the test_matrix$get() function.
##    Expected Result: No change because the initial value has been saved.
##    Actual Result:
##                  [,1] [,2]
##            [1,]    1    2
##            [2,]    3    4
## Step 8: Now check the test_matrix$getInverse() function
##    Expected Result: The inverse function is stored (cached).
##    Actual Result:
##                 [,1] [,2]
##            [1,] -2.0  1.0
##            [2,]  1.5 -0.5
## Step 9: What happens if we run cacheSolve(test_matrix) again?
##    Expectation: We will get a message because the inverse has been stored.
##    Result:
##        getting cached data
##                [,1] [,2]
##            [1,] -2.0  1.0
##            [2,]  1.5 -0.5
## Step 10: Change the matrix (> test_matrix <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## Step 11: Now check the test_matrix$get() function.
##    Expected Result: The new matrix will be loaded to the function.
##    Actual Result:
##                [,1] [,2]
##          [1,]    1    3
##          [2,]    2    4
## Step 12: Execute test_matrix$getInverse().
##    Expected Result: Null
##    Result: NULL
## Step 13: Run cacheSolve(test_matrix).
##    Expected Result: R will solve for the inverse of the matrix and store result in cache.
##    Actual Result:
##              [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
## Step 14: Execute test_matrix$getInverse()
##    Result: Matrix should be stored in cache.
##              [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
## Step 15: Run cacheSolve(test_matrix)
##    Expected Result: R will use cache to display current calculation since the matrix (input)
##    has not changed.
##    Result: Message -- "Getting cached data"
##              [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
##
## End of Testing.
