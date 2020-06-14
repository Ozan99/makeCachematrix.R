makeCacheMatrix <- function(x=matrix()){
        inv<-NULL
        set<-function(y){
          x<<-y
          inv<<-NULL
          
        }
        get<-function() {x}
        sInverse<-function(inverse) {inv<<-inverse}
        gInverse<- function() {inv}
        list(set=set,get=get,sInverse,gInverse)
        
}
 cacheSolve<- function(x,...){
          inv<- x$gInverse()
          if(!is.null(inv)){
         message("cached data")
                   return(inv)
                   
          }
          mat<-x$get()
          inv<- solve(mat,...)
          x$sInverse(inv)
          inv
 }
                      