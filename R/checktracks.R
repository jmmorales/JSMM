### Delete species from the tracks table not present in the atrib table or with no traits values 
  #or not in the corr matrix ###

#input:
  # matrix of tracks; columns: specie/track ID/sequence of the point/position (pixel number)
  # correlation table
  # traits table (first column are species names; other columns are numerical ou cathegorical traits)

TA1 = NA #tracks with more than 1 point
SPS1 = NA #species identification for tracks
#SP1 = NA
TID1 = NA #track ID
ma1 = NA
di1 = NA
cindex = NA
SP = NA
prob1 = prob2 = prob3 = NA #problems found

for(i in 1:ns){
  po = tsp[tsp==sps[i]]
  poc = which(csp==sps[i])
  if(length(po)==0) prob1 = c(prob1,sps[i])
  if(length(poc)==0) prob3 = c(prob3,sps[i])
  if(length(po)!=0 & length(poc)!=0) {
    lma = mass[tsp==sps[i]]
    if(is.numeric(lma)) {
      TA1 = c(TA1,TA[SPS==sps[i]])
      cindex = c(cindex, poc[1])
      SPS1 = c(SPS1,SPS[SPS==sps[i]])
      TID1 = c(TID1, TID[SPS==sps[i]])
      #SP1 = c(SP1,sps[i])
      ma1 = c(ma1,lma)
      di1 = c(di1,diet[tsp==sps[i]])
      SP = c(SP,sps[i])
    }
    
    else prob2 = c(prob2,sps[i])
  }
}
print(paste("species with no traits: ", prob1))
print(paste("species with no trait values: ", prob2))
print(paste("species with no phylogeny: ", prob3))

TA = TA1[!is.na(TA1)] #só tracks com pelo menos 2 pontos
#SP = SP1[!is.na(SP1)] #espécies observadas
TID = TID1[!is.na(TID1)]
SPS = SPS1[!is.na(SPS1)]
ma1 = ma1[!is.na(ma1)] #vetor de pesos por espécie (length=ns)
di1 = di1[!is.na(di1)]
SP = SP[!is.na(SP)]
cindex = cindex[!is.na(cindex)]
CC= cor[cindex,cindex] #same order for species in the correlation table

####end#####