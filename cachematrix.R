#Assumptions:
#Matrix is always assumed to be invertible as mentioned in the question
#Matrix is a square matrix always


## makeCacheMatrix gets and sets a matrix, it also gets and sets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  mat = matrix()
  inverseMat = matrix()
  
  #Returns Matrix
  getMatrix = function(){
      mat
  }
  
  #Sets Matrix
  setMatrix = function(x){
       mat <<- x
  }

  #Returns Inverse of defined matrix 
  getInverseMatrix = function(){
    inverseMat
  }
  
  #Sets Inverse of defined matrix
  setInverseMatrix = function(x){
        print('Setting an inverse Matrix')
        inverseMat <<- solve(x)
  }
  
  #Define the list of functions to be returned
  list('getMatrix' = getMatrix, 'setMatrix' = setMatrix, 'getInverseMatrix' = getInverseMatrix,
       'setInverseMatrix' = setInverseMatrix
       )
  
}


## cacheSolve will compute the inverse of a Matrix if its the first time
#Else it will just return the corresponding cached value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invXMat =  x$getInverseMatrix()
  if (is.na(invXMat)){
    #This matrix has no inverse computed
    invXMat = x$setInverseMatrix(x$getMatrix()) #Be careful to send the matrix
    invXMat
  }
  else{
    print('Getting the cached value of Inverse Matrix')
    invXMat
  }
}



#Run this function to test my code
#For the first attempt, it will set the inverse matrix
#For all subsequent attempts, it will get the cached value of the inverse matrix
testCase = function(){

#Define a matrix
mat1 = matrix( 1:4, nrow = 2, ncol = 2)

#Create inverseCalc as an object of makeCacheMatrix
inverseCalc = makeCacheMatrix(mat1)
inverseCalc$setMatrix(mat1)
  for (i in 1:5){
    cat('\n\n Attempt ',i)
    #Call cacheSolve to test if it returns the cached value
    print(cacheSolve(inverseCalc)) 
  }
}

testCase()

