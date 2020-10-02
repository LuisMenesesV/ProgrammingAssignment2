## Put comments here that give an overall description of what your
## functions do


## Esta función crea un objeto "matriz" especial que puede 
## almacenar en caché su inverso.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        
        x <<- y
        i <<- NULL
        
    }
    
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

## Esta función calcula la inversa de la "matriz" especial creada por
##makeCacheMatrix. Si la inversa ya se ha calculado (y la matriz no ha 
##cambiado), entonces debería recuperar la inversa de la caché.



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Devuelve una matriz que es la inversa de 'x'
    
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}