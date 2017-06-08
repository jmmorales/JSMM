loglik <-function(theta,spdata,covariates){
#loglik <- function(theta, tracks,corridor. = corridor, patch. = patch, di. = di){

  #To be updated: include spdata and covariates as inputs for function
  
  # theta = parameters;
  # ta = tracks;
  # corridor/patch = matrix of habitat type (0/1);
  # di = matrix of distances

  alpha <- exp(theta[1])
  betaC   <- theta[2]
  betaP   <- theta[3]
  li    <- 0
  
  idt <- tracks[,2]
  uid <- unique(idt)
  ta <- tracks[,3]

  for(ss in 1:length(uid)){ #for each one of the tracks
    lta <- ta[idt==uid[ss]] #data for the focal track

    for(i in 1:(length(lta)-1)){ #for each movement step
      x <- lta[i]
      pr <- exp(-di.[x,]/alpha) * exp(betaC * corridor.) * exp(betaP * patch.)
      pr <- pr / sum(pr)
      pr <- pr + 10^(-10) # is this really necesary?
      pr <- pr / sum(pr)
      li <- li + log(pr[lta[i + 1]])
    }
  }
  return(li)
}
