# The two functions below, used in sequence, are designd to calculate the inverse of a matrix x. 
# If there is a previous inverse matrix in cache it is not necessary recalculation.

# (1) The makeCacheMatrix function of x = matrix():

#   A variable 'm' to save inverse matrix in cache;
#   A function get() to obtain "raw" matrix (to find its inverse);
#   A function setImatrix() to assign the inverse matrix (of x) to m;
#   A function getImatrix() to obtain the cached inverse matrix.

makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    get <- function() x
    setImatrix <- function(Imatrix) m <<- Imatrix
    getImatrix <- function() m    
    list(get=get, setImatrix=setImatrix, getImatrix=getImatrix) # return a list of functions as an R object
}

# (2) The cacheSolve function to invert the matrix x

#   It checks if the is a inverse matrix in cache;
#   If yes, returns the result and quits:
#   If not, the inverse of x is calculated, saved to cached, and returned.
#   It is returned a list calling makeCacheMatrix(x).

cacheSolve <- function(x) {
    m <- x$getImatrix()
    if(!is.null(m)) {
        message("Inverse Matrix was found. Done.")
        return(m)
    } else {
        message("No cached data was found. Calcule inverse matrix")
        data <- x$get() # obtains matrix from object x
        m <- solve(data) # finds inverse matrix
        x$setImatrix(m) # assigns resulting inverse matrix to object x
        message("Done.")
        return(m)
    }
}
