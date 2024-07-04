## CALCULATE LORICA VOLUME BASED ON OUTLINES
## AUTHOR: Maximilian Ganser
###############################################################################################################
## Required libraries
#install.packages("devtools")
#devtools::install_github("MomX/Momocs")
library(Momocs)
require(ggplot2)

## Volume function | Discrete sum of "cylindrical slices" using x-y coordinates
calculate_volume <- function(xy_matrix) {
  # Assign x and y coordinates into separate vectors
  x <- xy_matrix[,1]
  y <- xy_matrix[,2]
  vol = 0
  for (i in 1:length(x)) {
    vol <- vol + pi * (y[i]^2)
  } 
  return(vol)
}

# Conversion factor: pixel volume to µm³
# Based on µm/pixel of image (depends on microscope/camera/magnification)
# Isometric pixel | Side length of pixel = x µm | Conversion factor = x³ 
cfactor <- 0.0001346113

# Get list of JPG files in the specified folder
jpg_files <- list.files(path = "lorica_outlines", pattern = "\\.jpg$", full.names = TRUE)

# Separate files with _a and _b endings
files_a <- jpg_files[grep("_a\\.jpg$", jpg_files)]
files_b <- jpg_files[grep("_b\\.jpg$", jpg_files)]

# Ensure that each pair has a corresponding file with the same base name
base_names_a <- gsub("_a\\.jpg$", "", basename(files_a))
base_names_b <- gsub("_b\\.jpg$", "", basename(files_b))

# Create a named list to hold the pairs
file_pairs <- list()
for (base_name in base_names_a) {
  file_a <- files_a[grep(paste0(base_name, "_a\\.jpg$"), files_a)]
  file_b <- files_b[grep(paste0(base_name, "_b\\.jpg$"), files_b)]
  
  if (length(file_a) == 1 && length(file_b) == 1) {
    file_pairs[[base_name]] <- list(a = file_a, b = file_b)
  }
}

# Loop through each pair of JPG files
for (base_name in names(file_pairs)) {
  # Import images
  lorica_a <- import_jpg1(jpg.path = file_pairs[[base_name]]$a, auto.notcentered = TRUE, fun.notcentered = NULL, threshold = 0.5)
  lorica_b <- import_jpg1(jpg.path = file_pairs[[base_name]]$b, auto.notcentered = TRUE, fun.notcentered = NULL, threshold = 0.5)
  
  # Remove unwanted lines and average identical x coordinates
  lorica_a_outline <- lorica_a[lorica_a[, 2] > 1, ]
  lorica_a_outline <- lorica_a_outline[lorica_a_outline[, 1] < max(lorica_a[, 1]), ]
  lorica_a_outline <- as.data.frame(lorica_a_outline - 2)
  lorica_a_outline <- aggregate(lorica_a_outline$V2, list(lorica_a_outline$V1), FUN=mean)
  colnames(lorica_a_outline) <- c("x", "y")
  
  lorica_b_outline <- lorica_b[lorica_b[, 2] > 1, ]
  lorica_b_outline <- lorica_b_outline[lorica_b_outline[, 1] < max(lorica_b[, 1]), ]
  lorica_b_outline <- as.data.frame(lorica_b_outline - 2)
  lorica_b_outline <- aggregate(lorica_b_outline$V2, list(lorica_b_outline$V1), FUN=mean)
  colnames(lorica_b_outline) <- c("x", "y")
  
  # Plot outer and inner lorica outlines (optional)
  shift_x <- max(lorica_a_outline$x) - max(lorica_b_outline$x)
  plot(lorica_a_outline$x, lorica_a_outline$y, type = "l", col = "red", asp = 1)
  lines(lorica_b_outline$x + shift_x, lorica_b_outline$y, type = "l", col = "blue")
  
  ## Calculate volume
  outer <- calculate_volume(lorica_a_outline)
  inner <- calculate_volume(lorica_b_outline)
  
  # Calculate lorica volume outside of the function
  lorica_volume <- round((outer - inner) * cfactor)
  
  # Print volume with identifier
  print(paste("Lorica", base_name, "Volume:", lorica_volume, "µm³"))
}
