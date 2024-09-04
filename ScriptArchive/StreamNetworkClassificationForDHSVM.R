# Using attribute table output from ArcMap streamfile

# Set up basin name for later use
BasinName = "Bear"

# Read data from streamfile attribute table
dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamNetworks/",BasinName,"/")
setwd(dir)

streamfile = read.csv(paste0(BasinName,"StreamfileAttributes.txt"))
head(streamfile)

plot(streamfile$chanclass,streamfile$meanmsq)
plot(streamfile$chanclass,streamfile$slope)

classes = sort(unique(streamfile$chanclass))
length(classes)

width = 0*classes
depth = 0*classes

# Determine bankfull width and depth for each class
for (i in 1:length(classes)) {
  # Calculate mean contributing area for this class
  DAavg = mean(streamfile$meanmsq[streamfile$chanclass==classes[i]])
  DAavg = DAavg/(1000*1000) # convert to km^2
  print(classes[i])
  print(DAavg)
  
  # https://doi.org/10.1111/jawr.12282
  width[i] = 2.76*DAavg^0.399
  depth[i] = 0.23*DAavg^0.294
}

width = round(width,2)
depth = round(depth,2)

print(width)
print(depth)

# Use DHSVM default values
manningsn = 0*classes + 0.05
maxinfiltration = format(0*classes + 0.0001, scientific=F)

classfile = data.frame(classes,width,depth,manningsn,maxinfiltration)
classfile

# Need to write in Unix (LF not CRLF) format
OutFile = file("adjust.classfile", "wb")
write.table(classfile, file = OutFile, row.names = FALSE, col.names = FALSE, quote = FALSE)
close(OutFile)

