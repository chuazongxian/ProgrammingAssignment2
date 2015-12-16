## The two functions work together to store the inversed value for a matrix,
## calling upon this value if there is already a computed value, reducing computing time.

## This function creates a list that of 4 functions that allow the next function to 
## easily gain access to these 4 unique functions within the same environment. 
## It also ensures an anchor for the variable m.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinverse <- function(solved) m<<- solved 
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function looks into the memory in order to find if there has been a previous 
## computed inverse value for this particular matrix. If there is, it will return a value.
## If not, it will calculate the inverse of this matrix and provide the value and store in 
## in the variable m (through the above function) to be reused in the future.
## 'b' as a variable has been used to show 
## 1) it is a variable that is only defined within this environment 
## 2) that is merely used as a value store that will be locked in only through the setinverse 
## function.

cacheSolve <- function(x, ...) {
    b <- x$getinverse()
    if(!is.null(b)){
        message('getting cache...')
        return(b)
    }
    matric <- x$get()
    b <- solve(matric,...)
    x$setinverse(b)
    b
}
