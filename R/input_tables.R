### Delete species from the tracks table not present in the atrib table or with no traits values 
  # or not in the corr matrix ### Delete species from the atributes and phylogenetic tables not 
  # present in the tracks table

# input:
  # matrix of tracks; columns: specie/track ID/position (pixel number); the position should be 
    # in the correct order of movement and the order of the lines represent the order of movement
  # phylogenetic correlation table
  # traits table (first column are species names; other columns are numerical or categorical 
    # traits)
  # requires one continuous variable (mass)
  
set.input <- function (tracks = tracks, traits = traits, phy.cor = cor, body.mass = traits[,11]){
  
  # check if tracks table has 3 columns
  if(!ncol(tracks = 3)) {
    stop("Error: the table with tracks should have 3 columns")
  } else {
    # check if 1 column is cathegorical (species names), and if other columns are numerical 
      # (track ID/position (pixel number))
    if(!is.factor(tracks[,1])|!is.numeric(tracks[,2])|!is.numeric(tracks[,3])){
      stop("Error: the first column of the table with tracks should contain the species 
identification, the second the tracks identification, and the third the position")
    } else {
             
        TA1 = NA #tracks with more than 1 point
        SPS1 = NA #species identification for tracks
        #SP1 = NA
        TID1 = NA #track ID
        traits1 = traits[-c(1:nrow(traits)),]
        #ma1 = NA
        #di1 = NA
        cindex = NA
        SP = NA
        prob1 = prob2 = prob3 = NA #problems found
        
        sps <- unique(tracks[,1])
        ns <- length(sps)
        csp <- as.vector(unlist(cor[1,]))
        mass <- body.mass
        
        for(i in 1:ns){
          po = traits[,1][traits[,1]==sps[i]]
          poc = which(csp==sps[i])
          if(length(po)==0) prob1 = c(prob1,sps[i])
          if(length(poc)==0) prob3 = c(prob3,sps[i])
          if(length(po)!=0 & length(poc)!=0) {
            lma = mass[traits[,1]==sps[i]]
            if(is.numeric(lma)) {
              TA1 = c(TA1,tracks[,3][tracks[,1]==sps[i]])
              cindex = c(cindex, poc[1])
              SPS1 = c(SPS1,tracks[,1][tracks[,1]==sps[i]])
              TID1 = c(TID1, tracks[,2][tracks[,1]==sps[i]])
              #SP1 = c(SP1,sps[i])
              traits1 = rbind.data.frame(traits1,traits[mass[mass==lma],])
              SP = c(SP,sps[i])
            }
            
            else prob2 = c(prob2,sps[i])
          }
        }
                
        rnames_probs <- c("species with no traits:", "species with no trait values:",
                      "species with no phylogeny:")
        results_probs <- matrix(c(prob1, prob2, prob3), 
                            dimnames = list(rnames_probs,"Species with missing data"))
            
        TA = TA1[!is.na(TA1)] #só tracks com pelo menos 2 pontos
        #SP = SP1[!is.na(SP1)] #espécies observadas
        TID = TID1[!is.na(TID1)]
        SPS = SPS1[!is.na(SPS1)]
        SP = SP[!is.na(SP)]
        cindex = cindex[!is.na(cindex)]
        CC= cor[cindex,cindex] #same order for species in the correlation table
        
        return(list(results_probs,SP, tracks = data.frame(SPS,TID,TA), traits = traits1, cor = CC))
      }
    }
  }
