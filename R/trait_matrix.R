### Build traits matrix with columns of 0 and 1 for categorical traits and numerical column for continuous variables ###

#input:
  #ns - number of species
  #nt - number of traits
  #transformation for continuous variable
  #vectors of traits (only for the studied species)

Tmatrix <- function(..., apply.log = TRUE){

  x <- list(...)
  n <- length(x) 
  ns <- n
  nt <- length(x[[1]])
  
  T <- matrix(0,nrow=ns, ncol=nt)

  for(i in 1:n){
    if(is.character(x[[i]])){
      colpos = x[[i]]
      for(j in 1:ns){
        T[j,colpos[j]] = 1 # T matrix does not include the intercept column (tk1=1)
      } 
    } else {
      if(apply.log == TRUE) T[,length(traits)] = log(x[[i]])
      else T[,length(traits)] = x[[i]]
    }
  }
  
  return(T)
  
}
