# Subset the meteorology files to a time period of interest so they're not so huge
# MUST MANUALLY SET WHICH LINES TO SUBSET!

StartLine = 75241
EndLine = 121992

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/gridforcing/"
setwd(dir)

StationFiles = list.files(dir)

for (i in 1:length(StationFiles)){
  OriginalData = read.table(StationFiles[i])
  
  OutFile = file(paste0("TrimmedGridForcing_2005-2020/",StationFiles[i]), "wb")
  
  write.table(OriginalData[StartLine:EndLine,], file=OutFile, col.names=FALSE, row.names=FALSE, quote=FALSE)
  
  close(OutFile)
  
  print(i/length(StationFiles))
}
