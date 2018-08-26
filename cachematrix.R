## When I make first function to make matrix, it ranges from 2x2 to 10x10.
## and I considered the case of creating un-invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    repeat{
    mat_num = round(2+runif(1)*8,0)
    x = matrix(rep.int(1,times=mat_num^2),mat_num,mat_num)
    
    for (i in 1:mat_num){
        for (j in 1:mat_num){
            x[i,j] <- round(runif(1)*10)
            }
    }
    
    try_err <- class(try(solve(x),silent=TRUE))
    if(try_err!="try-error") break
    }
    x
}


cacheSolve <- function(x, ...) {
        inv_mat <- x
        
        inv_mat <- solve(inv_mat)
        inv_mat
}
