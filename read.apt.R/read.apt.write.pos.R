library(tools) #needed to get filename without extension
# Clear all variables (no direct equivalent, but clearing the environment can be done)
rm(list = ls())
# Set working directory to current file location
setwd(dirname(parent.frame(2)$ofile))

# Specify the location of the *.apt input file
# Note that .pos file will be saved in this location
fn <- c("C:/Users/gc9307/OneDrive - UK Atomic Energy Authority/Documents/Code/read.apt.R/Test.apt")

# Sourcing necessary scripts
source("ScriptsForReadAPT/APTBranchesDict.R")
source("ScriptsForReadAPT/APTFileHeader2.R")
source("ScriptsForReadAPT/APTSectionHeaderAuto.R")
source("ScriptsForReadAPT/PARAPROBE_Transcoder2.R")

# Save information into list called apt
apt <- PARAPROBE_Transcoder2(fn)

# Save as .pos file
#### function to save .pos file ####
writeposR <- function(posData,posFileName){
  # size of floats = 4 bytes
  sizeOfFloat = 4
  to.write = file(posFileName, "wb")
  reshaped <- matrix(t(posData),ncol=1)
  writeBin(object=as.double(reshaped),
           con=to.write,
           size=sizeOfFloat,
           endian="big",
           useBytes = FALSE)
  close(to.write)
}

#### Saving x, y, z, mass to pos ####
writeposR(
  data.frame(
    x = c(apt$Position[1,]),
    y = c(apt$Position[2,]),
    z = c(apt$Position[3,]),
    m = c(apt$Mass[1,])
  ),
  paste0(file_path_sans_ext(fn),".pos")
)

# Clear space in global env
rm(list = ls())
