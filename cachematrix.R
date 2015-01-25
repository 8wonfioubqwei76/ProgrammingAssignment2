##this function will take any specified matrix and cache it, but also
##it will be able to cache the inverse calculated by the second function

makeCacheMatrix<-function(x=matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function()inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
  
}

##the next function will use the object stored by makeCacheMatrix and return its inverse,
##then this will get stored as 'inv'

cacheSolve<-function(z, ...)  {
  inv<-z$getinv()
  if(!is.null(inv))  {
    message("getting cached data")
    return(inv)
  }
  data<-z$get()
  inv<-solve(data,...)
  z$setinv(inv)
  inv
  
}
