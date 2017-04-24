loglik <- function(theta, ta, corridor, patch, di){

  # theta = parameters;
  # ta = tracks;
  # corridor/patch = matrix of habitat type (0/1);
  # di = matrix of distances

  alpha <- exp(theta[1])
  betaC   <- theta[2]
  betaP   <- theta[3]
  li    <- 0

  uid <- unique(idt) # where is idt?

  for(ss in 1:length(uid)){ #for each one of the tracks
    lta <- ta[idt==uid[ss]] #analyse each track separately;
    # vector with all points of each track ss

    for(i in 1:(length(lta)-1)){ #for each deslocation
      x <- lta[i]
      pr <- exp(-di[x,]/alpha) * exp(betaC * corridor) * exp(betaP * patch)
      pr <- pr / sum(pr)
      pr <- pr + 10^(-10) # is this really necesary?
      pr <- pr / sum(pr)
      li <- li + log(pr[lta[i + 1]])
    }
  }
  return(li)
}
