
makeCacheMatrix <- function(x = matrix()) {
        
        ## initiat the inv vairable
        inv<-NULL
        
        ## set the spicial matrix..
        set<-function(y){ x<<-y }
        
        ## get the spicial matrix..
        get<-function(){ x }
        
        ## assigne the inverse of te spicial matrix to "inv"
        set_invers<-function(inverse){ inv<<-inverse }
        
        ## get the value of "inv"
        get_invers<-function(){inv }
        
        list(set = set, get = get,
             set_invers=set_invers
             ,get_invers=get_invers)

}

cacheSolve <- function(x, ...){
        
        inv <- x$get_invers()
        
        if(!is.null(inv)){
                message("getting the invers")
                return(inv)
                
        }## end if
        
        data<-x$get()
        inv<-solve(data,...)
        x$set_invers(inv)
        inv
}
