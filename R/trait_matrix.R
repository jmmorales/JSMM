### Build traits matrix with columns of 0 and 1 for cathegorical traits and numerical column for continuous variables ###

#input:
  #ns - number of species
  #nt - number of traits
  #transformation for continuous variable

T = matrix(0,nrow=ns, ncol=nt)

colpos = di1
for(i in 1:ns){
  T[i,colpos[i]] =1 
} # T matrix does not include the intercept column (tk1=1)

T[,length(traits)] = log(ma1)