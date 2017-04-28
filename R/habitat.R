### Build n vectors of 0 and 1 for cathegorical environmental variables ###

#input: 
  #environmental matrix (numerical)
  #number of classes n
  #values for each class

h = habitat
h[h==3|h==4] = 2 #corridors,isolated trees and groups of trees are corridors (or stepping-stones)
h[h==5|h==6] = 3 #forest patches are forest
hh=h

corridor = h #matrix; each pixel has a value =1 for corridor pixels or =0 for patch or pasture pixels
corridor[corridor==1|corridor==3]=0
corridor[corridor==2]=1
corridor <- unlist(corridor)

patch = h #matrix; each pixel has a value =1 for patch pixels or =0 for corridor or pasture pixels
patch[patch==1|patch==2]=0
patch[patch==3]=1
patch <- unlist(patch)