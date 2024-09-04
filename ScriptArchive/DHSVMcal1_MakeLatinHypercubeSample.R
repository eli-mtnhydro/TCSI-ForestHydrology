### MOBOhydro CALIBRATOR FOR DHSVM ###
# Session 1: Make Latin Hypercube Sample of initial designs

################################################################################
# Universal adjustable settings
################################################################################

# Note: this session is only run once, to create the design grid for generation 1

ParamNames = c("SOILD","SLHC","POROSITY","EXPDEC","MINRES","SNOWTEMP","ALBMELTRATE")

ResponseOrder = 3 # Highest order polynomial response surface that can be constrained by the results

# See The Design and Analysis of Computer Experiments (Santner, Williams, & Notz 2014) page 111
nParams = length(ParamNames) # Number of parameters in the calibration
nModels = choose((ResponseOrder + nParams),ResponseOrder) # Number of designs per generation
print(nModels)

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/Calibrate_MOBOhydro/"
setwd(dir)

################################################################################
# Create matrix of initial random space-filling designs
################################################################################

library(DiceDesign)

# Create random Latin hypercube samples in [0,1]^nParams space
RandomLHS = lhsDesign(nModels, nParams, seed = 42)

# Enhanced Stochastic Evolutionary algorithm to optimize space-filling design
# Maximizes the minimum distance between design points in parameter space
MaxMinLHS = maximinESE_LHS(RandomLHS$design)

design.grid = MaxMinLHS$design
colnames(design.grid) = ParamNames

# Plot histograms
par(mfrow=c(2,4))
BreakLocs = seq(0,1,0.1)
for (i in 1:nParams){
  hist(design.grid[,i], main=paste0(ParamNames[i],"\nHistogram of Initial Designs"),xlim=c(0,1),breaks=BreakLocs,axes=FALSE,xlab="",ylab="")
  axis(side=1, at=BreakLocs)
  axis(side=2)
}

# Add labels for param sets
design.grid = cbind(ParamSet=1:nModels, design.grid)
print(head(design.grid))

# Write design grid to file
write.csv(design.grid,"Generation1_DesignGrid.csv")
