### MOBOhydro CALIBRATOR FOR DHSVM ###
# Session 3: Create auxiliary shell scripts to run on Pronghorn

################################################################################
# Universal adjustable settings

ModelGen = 6

# Pronghorn settings for parallel DHSVM runs
MaxRunTime = "3-00:00" # (D-HH:MM) Est. 1.6 days for Yuba
MemoryPerModel = 6 # Gb, 1 CPU hour includes 7 Gb of RAM for free

# For myconvert.c conversion of ascii<-->binary rasters
nColumns = 2247
nRows = 1436

InputMapNames = c("soild","soil_ksat","soil_porosity")
InputMapNlayers = c(1,1,3)
InputMapTypes = c("float","float","float")

OutputMapNames = c("Map.Snow.MaxSwe","Map.Snow.MaxSweDate","Map.Snow.MeltOutDate")
OutpuMapNlayers = c(6,6,6) # Number of snow calibration years
OutputMapTypes = c("float","int","int")
#nOutputMaps = length(OutputMapNames)
nOutputMaps = 2 # Not downloading MeltOutDate

# For naming output folders
BasinNames = c("Truckee","Yuba")
SleepTimes = c(30,30)

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/Calibrate_MOBOhydro/"
setwd(dir)

ScriptDir = "Pronghorn/"

################################################################################
# Get matrix of new designs (just for numbering of parameter sets)
################################################################################

if (ModelGen > 1){
  # Read both current and last generation designs so we only make config files/maps for the new parameter sets
  design.grid.lastgen = read.csv(paste0("Generation",ModelGen-1,"_DesignGrid.csv"))[,-1]
  design.grid.thisgen = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
  
  nDesignsLastGen = dim(design.grid.lastgen)[1]
  nDesignsThisGen = dim(design.grid.thisgen)[1]
  
  # Subset novel designs
  design.grid = design.grid.thisgen[(nDesignsLastGen+1):(nDesignsThisGen),]
} else {
  # Read the first generation design grid
  design.grid = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
}

nNewDesigns = length(design.grid[,1])
print(nNewDesigns)

################################################################################
# Make Cal3a Setup and Convert Inputs
################################################################################

# Bash header with options for Pronghorn
ScriptLines = c("#!/bin/bash")
ScriptLines = c(ScriptLines,"#SBATCH -n 1 # Number of tasks created for this job")
ScriptLines = c(ScriptLines,"#SBATCH -t 0-02:00 # Runtime in D-HH:MM")
ScriptLines = c(ScriptLines,paste0("#SBATCH --mem=",1000*MemoryPerModel," # Memory in Mb for each CPU"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -o Cal3a_Gen",ModelGen,"_%j.out # File to which STDOUT will be written"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -e Cal3a_Gen",ModelGen,"_%j.err # File to which STDERR will be written"))

# Lines to convert input maps from ascii to binary and make output folders for each new parameter set using a C-style for-loop
ScriptLines = c(ScriptLines,"\n# Convert input maps from ascii to binary and make output directories\n")

ScriptLines = c(ScriptLines,paste0("MIN=",design.grid$ParamSet[1]),paste0("MAX=",tail(design.grid$ParamSet,1),"\n"))

ScriptLines = c(ScriptLines,"for ((i=MIN; i<=MAX; i++)); do")
ScriptLines = c(ScriptLines,paste0("    mkdir ../Params\"$i\"_",BasinNames))
ScriptLines = c(ScriptLines,paste0("    ../program/myconvert asc ",InputMapTypes," ../input/Params\"$i\"_",InputMapNames,".txt ../input/Params\"$i\"_",InputMapNames,".bin ",nRows*InputMapNlayers," ",nColumns))
ScriptLines = c(ScriptLines,"done\n") # End of for-loop

# Write Cal3a bash script for the current generation in Unix format
Cal3aScript = file(paste0(ScriptDir,"Cal3a_SetupAndConvertInputs_Gen",ModelGen,".slurm"), "wb")
writeLines(ScriptLines, Cal3aScript)
close(Cal3aScript)

################################################################################
# Make Cal3b Run DHSVM in Parallel (this is the big one...)
################################################################################

for (bsn in 1:length(BasinNames)){
  # Bash header with options for Pronghorn
  ScriptLines = c("#!/bin/bash")
  #ScriptLines = c(ScriptLines,"#SBATCH --nodes=1")
  ScriptLines = c(ScriptLines,paste0("#SBATCH --ntasks=",length(design.grid$ParamSet)))
  ScriptLines = c(ScriptLines,"#SBATCH --cpus-per-task=2")
  ScriptLines = c(ScriptLines,"#SBATCH --cpu-freq=performance")
  ScriptLines = c(ScriptLines,paste0("#SBATCH -t ",MaxRunTime," # Runtime in D-HH:MM"))
  ScriptLines = c(ScriptLines,paste0("#SBATCH --mem-per-cpu=",1000*MemoryPerModel," # Memory in Mb for each CPU"))
  ScriptLines = c(ScriptLines,paste0("#SBATCH -o Cal3b_Gen",ModelGen,"_%j.out # File to which STDOUT will be written"))
  ScriptLines = c(ScriptLines,paste0("#SBATCH -e Cal3b_Gen",ModelGen,"_%j.err # File to which STDERR will be written"))
  
  # Lines to launch a background task running each parameter set for each basin using a C-style for-loop
  ScriptLines = c(ScriptLines,"\n# Run DHSVM Calibration Models in Parallel\n")
  for (j in 1:length(design.grid$ParamSet)){
    ScriptLines = c(ScriptLines,paste0("srun --ntasks=1 --nodes=1 --cpus-per-task=2 --mem-per-cpu=",1000*MemoryPerModel," --exclusive bash  -c \"(../sourcecode/DHSVM3.2 ../config/Config_Params",design.grid$ParamSet[j],"_",BasinNames[bsn],".txt > ../Params",design.grid$ParamSet[j],"_",BasinNames[bsn],"/out.txt) >& ../Params",design.grid$ParamSet[j],"_",BasinNames[bsn],"/err.txt\" &"))
    ScriptLines = c(ScriptLines,paste0("sleep ",SleepTimes[bsn])) # Wait for the dust to settle after initial file i/o
  }
  ScriptLines = c(ScriptLines,"\nwait") # Wait for background processes to finish
  
  # Write Cal3b bash script for the current generation in Unix format
  Cal3bScript = file(paste0(ScriptDir,"Cal3b",BasinNames[bsn],"_RunDHSVMinParallel_Gen",ModelGen,".slurm"), "wb")
  writeLines(ScriptLines, Cal3bScript)
  close(Cal3bScript)
}

################################################################################
# Make Cal3c Convert Outputs
################################################################################

# Bash header with options for Pronghorn
ScriptLines = c("#!/bin/bash")
ScriptLines = c(ScriptLines,"#SBATCH -n 1 # Number of tasks created for this job")
ScriptLines = c(ScriptLines,"#SBATCH -t 0-02:00 # Runtime in D-HH:MM")
ScriptLines = c(ScriptLines,paste0("#SBATCH --mem=",1000*MemoryPerModel," # Memory in Mb for each CPU"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -o Cal3a_Gen",ModelGen,"_%j.out # File to which STDOUT will be written"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -e Cal3a_Gen",ModelGen,"_%j.err # File to which STDERR will be written"))

# # Lines to convert output maps from binary to ascii for each new parameter set using a C-style for-loop
ScriptLines = c(ScriptLines,"\n# Convert output maps from binary to ascii\n")
ScriptLines = c(ScriptLines,paste0("MIN=",design.grid$ParamSet[1]),paste0("MAX=",tail(design.grid$ParamSet,1),"\n"))
ScriptLines = c(ScriptLines,"for ((i=MIN; i<=MAX; i++)); do")
ScriptLines = c(ScriptLines,paste0("    mkdir ../results/Params\"$i\"_",BasinNames))
ScriptLines = c(ScriptLines,paste0("    cp ../Params\"$i\"_",BasinNames,"/Streamflow.Only ../results/Params\"$i\"_",BasinNames,"/Streamflow.Only"))
for (k in 1:nOutputMaps){
  ScriptLines = c(ScriptLines,paste0("    ../program/myconvert ",OutputMapTypes[k]," asc ../Params\"$i\"_",BasinNames,"/",OutputMapNames[k],".bin ../results/Params\"$i\"_",BasinNames,"/",OutputMapNames[k],".txt ",nRows*OutpuMapNlayers[k]," ",nColumns))
}
ScriptLines = c(ScriptLines,"done")

# Write Cal3c bash script for the current generation in Unix format
Cal3cScript = file(paste0(ScriptDir,"Cal3c_ConvertOutputs_Gen",ModelGen,".slurm"), "wb")
writeLines(ScriptLines, Cal3cScript)
close(Cal3cScript)
