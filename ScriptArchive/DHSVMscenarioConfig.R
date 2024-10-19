
SelectedModel = 276

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

templatedir = paste0("ScenarioTemplates/Params",SelectedModel,"/")
ConfigTemplates = list.files(templatedir)

# Directory for final config files
finisheddir = paste0("DynaVegConfig_Params",SelectedModel,"/")

# Set up each config file for each map set
for (MapSet in MapSets){
  for (ConfigTemplate in ConfigTemplates){
    
    # Read template config file
    ConfigMapSet = readLines(paste0(templatedir, ConfigTemplate))
    
    # Fill placeholders with climate and map set info
    ConfigMapSet = gsub(pattern="CLIMATE", replace=ClimateName, x=ConfigMapSet)
    ConfigMapSet = gsub(pattern="MAPSET", replace=MapSet, x=ConfigMapSet)
    
    # Write new config file for this template and map set
    writeLines(ConfigMapSet, con=paste0(finisheddir, "Config_", "P", SelectedModel, MapSet, sub(".*Temp", "", ConfigTemplate)))
  }
}

