## The underlying functions first create the inverse of a matrix and then cache it

## The function below creates matrix object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
       inv<-NULL
       set<-function(matrix){
              mat<<-matrix
              inv<<-NULL
       }
       get<-function(){
              mat
       }
       setInv<-function(inverse){
              inv<<-inverse
       }
       getInv<-function(){
              inv
       }
       list(set=set, get=get, setInv=setInv, getInv=getInv
}


## The function below computes the inverse of the matrix returned by the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat<-x$getInv()
        if(!is.null(mat)){
                return(mat)        
        }
        info<-x$get()
        mat<-solve(info)%*%info
        x$setInv(mat)
        mat
}
