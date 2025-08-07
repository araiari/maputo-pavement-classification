# Maputo Road Surface Analysis - Processing Images of Road Segments to cloud of points
# To analyze the subdivision of Maputo roads, we first need to extract the cloud of
# point of each road segment. This script applies road surface classification 
# to road segments using DBSCAN clustering.

{
  rm(list = ls())       # Clear workspace
  graphics.off()        # Close graphics devices
  
  library(sp)
  library(terra)        # Replaces rgdal and raster packages
  library(sf)           # For spatial data handling
  library(rgl)          # For 3D graphics
  library(DepthProc)    # For depth-based statistics
  library(fpc)          # For DBSCAN clustering
}


# Extract Road Names --------------------------------------------------
# Save names of unknown road images in surf_division_workspace_ALL.RData

  # Load shapefile -> road segments
  data_poly <- st_read("preprocessed-data/Road_cleaned.shp")
  colnames(data_poly)
  
  data_poly = na.omit(data_poly)
  data_poly = cbind(data_poly, osm_surf_pred = "unk")
  
  # Extract names of road segments with unknown surface type
  unk_names <- as.vector(data_poly[which(data_poly$osm_surf == "unk"), ]$image)
  pav_names <- c(
    as.vector(data_poly[which(data_poly$osm_surf == "paved"), ]$image),
    as.vector(data_poly[which(data_poly$osm_surf == "asphalt"), ]$image)
  )
  unp_names <- as.vector(data_poly[which(data_poly$osm_surf == "unpaved"), ]$image)
  
  # Function to standardize image names for file system compatibility
  change_name = function(name) {
    name2 <- name
    name <- strsplit(name, "_")
    
    n <- length(name)
    for (i in 1:n) {
      temp_name <- paste(name[[i]][1], "_", sep = "")
      temp_name <- paste(temp_name, "_", sep = "")
      temp_name <- paste(temp_name, name[[i]][2], sep = "")
      temp_name <- paste(temp_name, ".tif", sep = "")
      name2[i] <- temp_name
    }
    return(name2)
  }
  
  unk_names = change_name(unk_names)
  pav_names = change_name(pav_names)
  unp_names = change_name(unp_names)
  
  # Clean up temporary variables
  rm(change_name, data_poly)
  
  # Uncomment to save workspace:
  save(unk_names, unk_names, unk_names, 
      file="processed-data/surf_type_workspace.RData", )


# Calculate Road Surface Clouds -- Known roads ---------------------------------

# Load workspace with road surface classification data
load("processed-data/surf_type_workspace.RData")
known_names = c(pav_names, unp_names)

# Load custom analysis functions
source("scripts/utils/funzione_crea_valMed_valTot.R")

# Remove blacklisted (invalid) images from processing list
for (black in black_list) {
  if (sum(known_names == black) > 0) {
    i = which(known_names == black)
    known_names = known_names[-i]
  }
}

# Batch processing loop for selective method
prefix = "data/raster"
val_tot = NULL                  # Initialize tot cloud results
val_med = NULL                  # Initialize median results

approach = "permissive"             # Choose between strict and permissive
lim_threshold = 90

for (name in known_names) {
  
  # Apply selective DBSCAN method and get both total pixels and median
  lista = crea_valMed_valTot(name, prefix, lim = lim_threshold, 
                              method = approach, return = "all")
  
  # Check if processing was successful (no errors)
  if (dim(lista[[1]])[1] != 1) {
    # Store road pixel data with metadata
    val_tot = rbind(val_tot, cbind(lista[[1]], img_name = name))
    # Store median values with metadata
    val_med = rbind(val_med, cbind(lista[[2]], img_name = name))
  }
}

# Convert to data frame and ensure numeric columns

for (j in 1:3) {
  val_tot[, j] = as.numeric(val_tot[, j])
}

# Save batch results to separate files
final_file_batch = paste(paste(final_file, i, sep = ""), "RData", sep = ".")
save(val_tot, file = final_file_batch)

# Process and save final median results
val_tot = data.frame(val_tot)
val_med = data.frame(val_med)
for (i in 1:3) {
  val_med[, i] = as.numeric(val_med[, i])
  val_tot[, i] = as.numeric(val_tot[, i])
}
save(val_tot, file = "val_tot_known.RData")
save(val_med, file = "val_med_known.RData")



