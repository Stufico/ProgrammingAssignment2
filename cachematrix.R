##The functions here create a matrix and cache it. Then a second function solves the inverse of the matrix and
##returns the values

## Write a short comment describing this function
## This function creates a matrix and stores it

makeCacheMatrix <- function(x = matrix()) { ##creates a function that assigns x as a matrix
        m <- NULL ## This sets m as Null
        set <-function(y){ 
                x<<- y
                m<<- NULL
        } ##This function assigns y, the matrix, as as x which can come from a different environment is now 
          ##assigned to set
        get <- function()x  ## this gets the value of x when the function is ran
        makematrix <- function(solve)  ## this is creates a function, calls solve and assigns it to makematrix
                m<<- solve ##this assigns solve to m
        returnmatrix <- function()m ##This gets m when the function is run
        list(set = set, get = get, ##creates a list with all of the assigned functions
             makematrix = makematrix,
             returnmatrix = returnmatrix)
}


## This function uses the solve feature to create the inverse of the cached data

cacheSolve <- function(x= matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$returnmatrix() ##calls the returnmatrix function and assigns it to matrix
        if(!is.null(m)){ ##returns a message if M is cached
                message("getting cached data")
                return(m)}
        matrix<-x$get() ##calls the get function and assignes it to matrix
        m<-solve(matrix,...) ##returns the inverse matrix and assigns it to m
        x$makematrix(m) ##returns the makematrix and inserts m
        m ## returns the m value
}

