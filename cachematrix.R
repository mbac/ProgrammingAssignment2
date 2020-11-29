## Put comments here that give an overall description of what your
## functions do

## This function accepts (or creates a null) matrix and returns a list object
## containing the original matrix (m), an initially empty item for the inverse of the
## matrix (inv_m), and functions to initialize the object and invert the matrix. Per request,
## inversion is performed with a call to cacheSolve(), which accepts additional
## arguments to be passed to solve().
makeCacheMatrix <- function(x = matrix(), ...) {
    # start with empty $inv_m on generic function call
    inv_m <- NULL
    m <- x

    # Initialize object with $m element (original matrix) and
    # empty $inv element. This effectively deletes any 'cached' results.
    init <- function() {
        # Load argument to element $m, for reference
        m <<- x
        # Create an element to store inverted matrix
        inv_m <<- NULL
    }

    ## This function inverts the matrix stored at $m (the original object) and
    ## stores it in $inv_m. It also returns that data.
    ## Ellipsis is meant to pass additional arguments to base::solve()
    invert <- function(...) {
        inv_m <<- solve(m, ...)
        return(inv_m)
    }

    # 'getter' function returning the original matrix
    get_m <- function() m

    # getter for the inverted matrix
    get_inv <- function() inv_m

    ## Assemble the object, paying attention not to "publish" any variables.
    ## Only functions are exported to the list.
    list(
        init = init,
        invert = invert,
        get_m = get_m,
        get_inv = get_inv,
        inv_m = inv_m
    )
}


## This function takes a makeCacheMatrix() object and calls its $invert method
## to obtain the inverse of the matrix, which is then returned. If the argument
## already has a $inv_m item, this is returned without any further calculations.

cacheSolve <- function(x, ...) {
    ## Make a copy of the inverted matrix element of the object, using
    ## appropriate 'getter' function
    inv_m <- x$get_inv()

    ## Check if there is an inverted matrix element
    ## (i.e., if it isn't NULL)
    if (!is.null(inv_m)) {
        message("returning cached data")

        # Return the element, which is "cached" at myObject$inv_m
        return(inv_m)
    }

    ## If object$inv_m is NULL, the inv matrix hasn't been calculated, or
    ## it's been deleted using object$init()
    else {
       message("Calculating inversion…")

        ## Use object$invert method to compute the inverted matrix. That
        ## method returns the inverted matrix, so we can make a copy
        ## and return it here. The 'original' is still stored at
        ## object$inv_m.
        ## Ellipsis is for passing additional arguments to base::solve()
        xirtam <- x$invert(...)
       return(xirtam)
    }
}

# Generate example matrix for testing
test <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

