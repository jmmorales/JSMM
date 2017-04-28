### Build tracks table only with tracks of at least 2 positions, inside the landscape ###

#input: 
  # matrix of tracks; columns: specie/track ID/sequence of the point/position (pixel number)
  # minimum length of the track (default:2 positions)

sp = tracks$sp
sps = unique(tracks$sp) #list of observed species #não está em ordem alfabética
ind = tracks$ID
inds = unique(ind) 

TA = NA #tracks with more than 1 point
SPS = NA #species identification for tracks
#SP = NA
TID = NA #track ID

#remove points out of plot and tracks with less than 2 points

for (i in 1:length(inds)){
  take = ind[ind==inds[i]]
  
  if(length(take)>1){
    tmp = tracks[tracks$ID==inds[i],5] #column z
    tr = numeric()
    for(j in 1:length(tmp)){
      if(tmp[j]<3601) tr = c(tr,tmp[j])
    }
    if(length(tr)>1){
      TA = c(TA,tr)
      #SP[length(SP)+1] = as.vector(unique(sp[tracks$ID==inds[i]]))
      SPS= c(SPS,as.vector(sp[tracks$ID==inds[i] & tracks$z<3601]))
      TID = c(TID,ind[ind==inds[i] & tracks$z<3601])
      if(length(unique(sp[tracks$ID==inds[i]])) > 1) print("problem with species names") #arrumar para colocar o nome da sp?
    }
  }
}
TA = TA[!is.na(TA)] #só tracks com pelo menos 2 pontos
#SP = SP[!is.na(SP)] #espécies por track
TID = TID[!is.na(TID)] #identificação do track
SPS = SPS[!is.na(SPS)] #vetor de espécies por ponto