makeCacheMatrix <- function(x = matrix()) { inv <- NULL ## build my special cahce matrix object
## inv will hold the inverse of the matrix, the inverse hasnt been calculated yet. NULL means that no matrix is stored yet
## cache sarts empty 
  set <- function(y){  ## set function replaces the empty matrix with a new one: take the new matrix "y"
    x <<- y ## "<<- " replaces the x outside the helper function/ changing both local and stored x
    inv <<- NULL ## wipes the existing saved inverse from the first line
  }
## this helps the the inverse and matrix change in reference to each other, helping the object stay relevant 
  get <- function() x ## lets you look at the matrix currently stored
  setInverse <- function(solveMatrix) inv <<- solveMatrix ## after savign inverse its stored in inv, saving time spent needed to recalculate 
  getInverse <- function() inv ## helper function lets cacheSolve() check whether it needs to do any work.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## returns list of all helper functions used
## helps R create my  "special matrix object" 
} 
## stores both matrix and functions used for it
                                
cacheSolve <- function(x, ...) { ## ... helps pass extra arguments if needed
  inv <- x$getInverse()  ## is asking object for cached inverse. Caching begins
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) 
## avoids recacluating the inverse 
  }
  data <- x$get() ## if inverse wasnt cached, actual matrix is needed for computation
  inv <- solve(data, ...) ## computes inverse with built in R function solve()
  x$setInverse(inv) ## sotres newly calcualted inverse inside the object
  inv ## returns the inverse 
}
