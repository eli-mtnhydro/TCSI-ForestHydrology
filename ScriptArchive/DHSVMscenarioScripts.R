
SelectedModel = 276

MapYears = seq(0,80,10) + 2020 - 5 - 1 # Shift back halfway, then shift to water year (10/1/2014 is WY 2015)

MapScenarios = 7
Climate = c("cnrm")
ClimateName = c("CNRM-CM5_RCP8.5")
LANDISrun = 1
# Climate = c("miroc")
# ClimateName = c("MIROC5_RCP8.5")
# LANDISrun = 4

MapSets = apply(expand.grid("S", MapScenarios, Climate, "R", LANDISrun),
                1, paste0, collapse="")

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/config/"
setwd(dir)

# Pronghorn settings for parallel DHSVM runs
MaxRunTime = "8-00:00" # (D-HH:MM) Est. 5.4 days for Truckee chunks
MemoryPerModel = 6 # Gb, 1 CPU hour includes 7 Gb of RAM for free
SleepTime = 60 # Seconds between starting model runs

# For myconvert.c conversion of ascii<-->binary rasters
nColumns = 2247
nRows = 1436

InputMapNames = c("Type","FC","Height","LAI")
InputMapNlayers = c(1,1,2,12)
InputMapTypes = c("char","float","float","float")

configdir = paste0("DynaVegConfig_Params",SelectedModel,"/")
ConfigFiles = list.files(configdir)
ConfigFiles = ConfigFiles[grepl(Climate, ConfigFiles)]
#ConfigFiles = ConfigFiles[!(grepl("Chunk1", ConfigFiles) | grepl("American_Chunk3", ConfigFiles) | grepl("Yuba_Chunk2", ConfigFiles))]
MainDirs = sub("_.*", "", sub("Config_", "", ConfigFiles))
SubDirs = sub("\\.txt", "", sub(paste0(".*",Climate,"R",LANDISrun,"_"), "", ConfigFiles))

################################################################################
# Setup and Convert Inputs
################################################################################

# Bash header with options for Pronghorn
ScriptLines = c("#!/bin/bash")
ScriptLines = c(ScriptLines,"#SBATCH -n 1 # Number of tasks created for this job")
ScriptLines = c(ScriptLines,"#SBATCH -t 0-02:00 # Runtime in D-HH:MM")
ScriptLines = c(ScriptLines,paste0("#SBATCH --mem=",1000*MemoryPerModel," # Memory in Mb for each CPU"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -o SetupScenarios_%j.out # File to which STDOUT will be written"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -e SetupScenarios_%j.err # File to which STDERR will be written"))

ScriptLines = c(ScriptLines,"\n# Make output directories\n")
ScriptLines = c(ScriptLines,paste0("cp -r ../ScenarioResults/DirectoryTemplates ../ScenarioResults/","P",SelectedModel,MapSets))

ScriptLines = c(ScriptLines,"\n# Convert input maps from ascii to binary\n")
for (MapSet in MapSets){
  for (Year in MapYears){
    ScriptLines = c(ScriptLines,paste0("../program/myconvert asc ",InputMapTypes,
                                       " ../dynaveg/",MapSet,"/Vegetation.",InputMapNames,
                                       ".10.01.",Year,".00.00.00.txt ../dynaveg/",
                                       MapSet,"/Vegetation.",InputMapNames,
                                       ".10.01.",Year,".00.00.00.bin ",nRows*InputMapNlayers," ",nColumns))
  }
}

# Write script in Unix format
SetupScript = file(paste0("Setup_ScenarioRuns_P",SelectedModel,"S",min(MapScenarios),"-",max(MapScenarios),Climate,"R",LANDISrun,".slurm"), "wb")
writeLines(ScriptLines, SetupScript)
close(SetupScript)

################################################################################
# Run DHSVM in Parallel (this is the big one...)
################################################################################

# Bash header with options for Pronghorn
ScriptLines = c("#!/bin/bash")
ScriptLines = c(ScriptLines,paste0("#SBATCH --ntasks=",length(ConfigFiles)))
ScriptLines = c(ScriptLines,"#SBATCH --cpus-per-task=2")
ScriptLines = c(ScriptLines,"#SBATCH --cpu-freq=performance")
ScriptLines = c(ScriptLines,paste0("#SBATCH -t ",MaxRunTime," # Runtime in D-HH:MM"))
ScriptLines = c(ScriptLines,paste0("#SBATCH --mem-per-cpu=",1000*MemoryPerModel," # Memory in Mb for each CPU"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -o RunScenarios_%j.out # File to which STDOUT will be written"))
ScriptLines = c(ScriptLines,paste0("#SBATCH -e RunScenarios_%j.err # File to which STDERR will be written"))

# Lines to launch a background task running each parameter set for each basin using a C-style for-loop
ScriptLines = c(ScriptLines,"\n# Run DHSVM Scenarios in Parallel\n")

for (i in 1:length(ConfigFiles)){
  ScriptLines = c(ScriptLines,paste0("srun --ntasks=1 --nodes=1 --cpus-per-task=2 --mem-per-cpu=",1000*MemoryPerModel,
                                     " --exclusive bash  -c \"(../sourcecode/DHSVM3.2 ../config/",ConfigFiles[i],
                                     " > ../ScenarioResults/",MainDirs[i],"/",SubDirs[i],"/out.txt) >& ../ScenarioResults/",
                                     MainDirs[i],"/",SubDirs[i],"/err.txt\" &"))
  ScriptLines = c(ScriptLines,paste0("sleep ",SleepTime)) # Wait for the dust to settle after initial file i/o
}

ScriptLines = c(ScriptLines,"\nwait") # Wait for background processes to finish

# Write script in Unix format
RunScript = file(paste0("Run_ScenarioRuns_P",SelectedModel,"S",min(MapScenarios),"-",max(MapScenarios),Climate,"R",LANDISrun,".slurm"), "wb")
writeLines(ScriptLines, RunScript)
close(RunScript)


