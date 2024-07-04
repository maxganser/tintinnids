## CALCULATE CELL VOLUME BASED ON OUTLINES
## AUTHOR: Maximilian Ganser
###############################################################################################################
## Required libraries
#install.packages("devtools")
#devtools::install_github("MomX/Momocs")
library(Momocs)
library(ggplot2)

## Volume function | Discrete sum of "cylindrical slices" using x-y coordinates
calculate_volume <- function(xy_matrix) {
  # Assign x and y coordinates into separate vectors
  x <- xy_matrix[,1]
  y <- xy_matrix[,2]
  vol=0
  for (i in 1:length(x)) {
    vol <- vol + pi * (y[i]^2)
  } 
  return(vol)
}

# Conversion factor: pixel volume to µm³
# Based on µm/pixel of image (depends on microscope/camera/magnification)
# Isometric pixel | Side length of pixel = x µm | Conversion factor = x³ 
cfactor <- 0.000035937

# Get list of JPG files in the specified folder
jpg_files <- list.files(path = "cell_volumes", pattern = ".jpg", full.names = TRUE)
  
# Loop through each JPG file
for (jpg_file in jpg_files) {
  # Import image
  cell <- import_jpg1(jpg.path = jpg_file, auto.notcentered = TRUE, fun.notcentered = NULL, threshold = 0.5)
    
  # Remove unwanted lines
  cell_outline <- cell[cell[, 2] > 1, ]
  cell_outline <- cell_outline[cell_outline[, 1] < max(cell[, 1]), ]
  cell_outline <- as.data.frame(cell_outline - 2)
    
  # Average identical x coordinates
  cell_outline <- aggregate(cell_outline$V2, list(cell_outline$V1), FUN = mean) 
  colnames(cell_outline) <- c("x","y")
    
  # Plot cell outline (optional)
  plot(cell_outline[,1], cell_outline[,2], type = "l", col = "red", asp = 1)
    
  # Calculate cell volume
  outer <- calculate_volume(cell_outline)
  cell_volume <- outer * cfactor
    
  # Print filename and volume
  cat("Filename:", jpg_file, "Volume:", cell_volume, "\n")
}
