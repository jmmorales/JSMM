# Priors: 
  # Prior for ZT: Zt~N(muZT,VZT)
  # Prior for SI: SI~IW(PSI,nu)
  # Prior for Rho: Rh~unif(0,1)

# Input
  # np - number of parameters
  # nt - number of traits
  # ns - number of species
  # Trunca function 
  
  # Likelihood function (demanded: theta and TA)
  # CC - species phylogenetic correlation matrix
  # T - traits (1 clumn for each trait; 1 line for each species) - ns x nt
  # tracks
  # parameters
  # n.iter 
  # adapt 
  # posteriors table size

jsmm.mcmc <- function(likelihood = likelihood, data = list(corridor, patch, di),
                      CC = CC, T = T, tracks = tracks, parameters = c("log distance","corridor_aff", "forest_aff"),
                      includeTraits = TRUE, includePhylogeny = TRUE, n.iter = 5000, adapt = 1000){
  
  tpm0 <- proc.time() #initial time
  
  if (missing(CC)) includePhylogeny = FALSE
  if (missing(T)) includeTraits = FALSE
  
  # Check if data is a list ############ to be done
  
  # Check if adapt < n.iter ############ to be done
  
  # Check if adapt & n.iter are wholenumbers ############ to be done
  
  np <- length(parameters)
  
  SPS <- tracks[,1]
  SP <- unique(SPS)
  ns <- length(SP)
  TID <- tracks[,2]
  TA <- tracks[,3] ########## change for z??
  
  # Extract objects from list data
  for(i in 1:length(data)){
    tmpobj <- data[[i]]
    eval(parse(text=paste(names(data)[[i]],"= tmpobj")))
  }
  
  # In the absence of trait information, only the intercept is included, in which case the 
  # expectation is the same for all species k
  if(!includeTraits){
    T = matrix(1,nrow=ns)
    nt = 1 
    traits = "intercept"
  }
  
  if(includeTraits){
    T = T
    nt=ncol(T)
    #if(!is.na(colnames(T))) 
      traits = colnames(T)
  }
  
  # Prior for ZT: ZT~N(muZT,VZT)
  muZT = matrix(0,nrow = np*nt)
  VZT = diag(np*nt)
  
  # Prior for SI: SI~IW(PSI,nu)
  PSI = diag(np)
  nu = np
  
  # Pre-compute inverse and determinant of the D-matrix
  
  if(includePhylogeny){
    
    rhos = seq(0,1,0.01)
    nr = length(rhos)
    priorRho = rep(0.5/(nr - 1),nr)
    priorRho[1] = 0.5
    priorRho = log(priorRho)
    iDDs = array(dim = c(ns,ns,nr))
    ldetDDs = numeric(nr)
    
    for(i in 1:nr){
      DD = rhos[i]*CC + (1 - rhos[i])*diag(ns)
      iDDs[,,i] = solve(DD)
      ldetDDs[i] = log(det(as.matrix(DD)))
    }
    
  }
  
  XT = kronecker(T,diag(np))
  
  #Parameters to be estimated : THE, SI, M, ZT
  THE = matrix(0,nrow = ns,ncol=np)
  SI = diag(np) #identity matrix
  iSI = solve(SI) #inverse of matrix SI
  ZT = matrix(0, nrow=nt, ncol=np)
  M = T%*%ZT #matrix multiplication
  
  if(includePhylogeny){
    RI = round(nr/2)
    RH = rhos[RI]
    iDD = iDDs[,,RI]
    ldetDD = ldetDDs[RI]
  }
  
  if(!includePhylogeny){
    RH = 0
    iDD = solve(diag(ns))
    ldetDD = log(det(solve(ns)))
  }
  
  li1 = numeric(ns)
  for(k in 1:ns){
    
    #specie = which(SPS==SP[k])
    #idt = TID[specie]
    
    li1[k] = likelihood(theta = THE[k,], tracks = tracks[SPS==SP[k],])#, corridor, patch, di)
  }
  
  initliks = data.frame(SP,li1) #initial likelihoods
  
  ac = array(0,c(np,ns,2))  # for acceptance rates #check the mean of {0., 0.}
  kk = array(1,c(np,ns))       # sd for the proposal distributions
  acr = array(NA,c(ns,np))
  
  #POST = list((n.iter-adapt)/10) # columns names = THE, ZT, SI #to store parameters estimates
  POST_THE = array(NA,c(ns,np,(n.iter-adapt)/10))
  POST_ZT = array(NA,c(nt,np,(n.iter-adapt)/10))
  POST_SI = array(NA,c(np,np,(n.iter-adapt)/10))
  POST_RH = numeric((n.iter-adapt)/10)
  
  ##################
  #   MCMC chains  #
  ##################
  
  for(i in 1:n.iter){
    
    #UPDATE THETA
    
    RES = as.numeric(t(M)) - as.numeric(t(THE)) 
    li2 = -(1/2)*RES%*%kronecker(iDD, iSI)%*%RES
    
    for(k in 1:ns){
      
      # specie = which(SPS==SP[k])
      # idt = TID[specie]
      
      for(l in 1:np){
        
        NTHE = THE
        nthe = THE[k,]
        #set.seed(42)
        nthe[l] = nthe[l] + rnorm(1, mean=0, sd = kk[l,k])
        NTHE[k, l] = nthe[l]
        nli1 = likelihood(theta = nthe, tracks = tracks[SPS==SP[k],])#, corridor, patch, di)
        RES = as.numeric(t(M)) - as.numeric(t(NTHE)) #
        nli2 = -(1/2)*RES%*%kronecker(iDD, iSI)%*%RES #
        ac[l,k,1] = ac[l,k,1] + 1    
        
        if(is.finite(nli1) & is.finite(nli2)){
          ##set.seed(42)
          if(runif(1) < exp((nli1 - li1[k] + nli2 - li2))){
            THE[k,] = nthe
            li1[k] = nli1
            li2 = nli2
            ac[l,k,2] = ac[l,k,2] + 1
          }
        }
        
      }
    }
    
    #UPDATE ZETA
    
    XTHE = as.vector(t(THE)) #transform to vector by line
    iXSI = kronecker(iDD,iSI) #iDD - Rho
    Vs = solve(solve(VZT) + t(XT)%*%iXSI%*%XT)
    Vs = (Vs + t(Vs))/2
    mus = Vs%*%(solve(VZT)%*%muZT + t(XT)%*%iXSI%*%XTHE)
    ##set.seed(42)
    ZT = matrix(rmvnorm(1,mean=mus, sigma = Vs), ncol = np, byrow=TRUE)
    M = T%*%ZT
    
    #UPDATE SI
    
    RES = THE - M
    A = t(RES)%*%iDD%*%RES
    PSIA = PSI + A
    PSIA = (PSIA + t(PSIA))/2
    ##set.seed(42)
    SI = riwish((nu+ns), PSIA) #InverseWishartMatrixDistribution;
    SI = (SI + t(SI))/2
    iSI = solve(SI)
    
    #UPDATE RHO
    
    if(includePhylogeny){
      RES = as.numeric(t(M)) - as.numeric(t(THE))
      nldetDDs = ldetDDs
      for(ii in 1:nr){
        nldetDDs[ii] = (-1/2)*(np*ldetDDs[ii] + RES%*%kronecker(iDDs[,,ii], iSI)%*%RES)
      }
      rholi = priorRho + nldetDDs
      pr = exp(rholi)/sum(exp(rholi))
      RI = sample(seq(1:nr),size=1,prob=pr)
      iDD = iDDs[,,RI]
      ldetDD = ldetDDs[RI]
      RH = rhos[RI]
    }
    
    #Storing the results
    if(is.wholenumber((i-1)/10) & i > adapt) {
      POST_THE[,,((i-adapt)+10-1)/10] = THE
      POST_ZT[,,((i-adapt)+10-1)/10] = ZT
      POST_SI[,,((i-adapt)+10-1)/10] = SI
      POST_RH[((i-adapt)+10-1)/10] = RH
    }
    
    #Adapation
    for(h in 1:ns){
      q <- 1 + exp(-i/500)
      w <- 1 - 0.1*exp(-i/500)
      acr[h,] <- ac[,h,2]/ac[,h,1]
      
      if(i <= adapt){
        kk[,h] <- sapply(kk[,h] * q^(acr[h,] - 0.44), trunca)
        ac = ac*w 
      }
    }
    
  }
  
  tpm1 <- proc.time() #final time
  
  # Organizing output
  
  dtpm <- (tpm1[3] - tpm0[3])/60 #time elapsed
  
  npost <- (n.iter-adapt)/10
  
  rnames_summary <- c("Time elapsed (minutes):", "Number of iterations:",
                      "Number of adaptation iterations:","Length of recorded posteriors", 
                      "Number of species", "Number of parameters", "Number of traits")
  results_summary <- matrix(c(as.numeric(dtpm), n.iter, adapt, npost, ns, np, nt), 
                            dimnames = list(rnames_summary,"Model summary")) ########## think about a better title
  
  likelihoods <- cbind(initliks,li1) # table with initial and inal likelihoods for each species
  colnames(likelihoods) <- c("Species", "InitialLikelihood", "FinalLikelihood")
  
  posteriors <- list(POST_THE = POST_THE, POST_ZT = POST_ZT, POST_SI = POST_SI, POST_RH = POST_RH)
  
  return(list(results_summary = results_summary, likelihoods = likelihoods, posteriors = posteriors))
}
