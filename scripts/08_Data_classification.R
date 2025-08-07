# ============================================================================
# ROAD SURFACE CLASSIFICATION PIPELINE FOR MAPUTO
# ============================================================================
#
# METHODOLOGY:
# 1. DBSCAN Clustering: Applied to satellite image pixel values to identify road pixels
# 2. Feature Extraction: Extract statistical properties from road pixel clusters
# 3. k-NN Classification: Use energy distance to classify unknown roads based on 
#    similarity to known road segments
# 4. Spatial Analysis: Generate maps showing predicted road surface types
#
# INPUT DATA:
# - Satellite/aerial imagery of road segments (GeoTIFF format)
# - Shapefile with known road surface classifications (OSM data)
# - Training data from previously classified road segments
#
# OUTPUT:
# - Classified road surface types for unknown segments
# - Probability/confidence scores for predictions  
# - Spatial visualization of results
#
# CLASSIFICATION CATEGORIES:
# - "paved": Asphalt/concrete roads
# - "unpaved": Dirt/gravel roads  
# - "uncertain": Ambiguous cases requiring further analysis
#
# TECHNICAL APPROACH:
# - Energy distance: Robust statistical distance measure between point clouds
# - Sampling: 150 representative pixels per road segment for computational efficiency
# - Cross-validation: k=5 nearest neighbors with frequency-based decision rules
#
# PERFORMANCE CONSIDERATIONS:
# - Batch processing for memory efficiency with large datasets
# - Approximately 8 seconds per image classification
# - Total processing time: ~13 hours for complete dataset
# ============================================================================

rm(list = ls())
graphics.off()

# Load required libraries
library(terra)        # Modern spatial data processing
library(sf)           # Spatial features handling
library(energy)       # Energy statistics and distances
library(ggplot2)      # Advanced plotting
library(RColorBrewer) # Color palettes


# PHASE 1: EXTRACT ROAD PIXELS FROM UNKNOWN SEGMENTS ========================

# Load workspace with road surface classification data
load("processed-data/surf_type_workspace.RData")

# Load custom analysis functions for DBSCAN road detection
source("scripts/utils/funzione_crea_valMed_valTot.R")

# Clean dataset: Remove blacklisted (invalid/corrupted) images from processing list
for (black in black_list) {
  if (sum(unk_names == black) > 0) {
    i = which(unk_names == black)
    unk_names = unk_names[-i]
  }
}

# Configuration parameters for road pixel extraction
prefix = "data/raster"                                    # Path to image directory
final_file = "preprocessed-data/val_tot_unknown_"         # Base filename for output
approach = "strict"                                       # DBSCAN method: "strict" or "permissive"
lim_threshold = 90                                        # Vegetation removal threshold

# Batch processing: Process images in batches to manage memory usage
N_batch = 10000  # Images per batch
for (batch_num in (1:5 * N_batch)) {
  
  val_tot = NULL  # Initialize batch results
  unk_names_cut = unk_names[batch_num:(batch_num + N_batch - 1)]  # Current batch
  
  # Process each image in the current batch
  for (name in unk_names_cut) {
    
    # Apply DBSCAN clustering to extract road pixels
    val_tot_tmp = crea_valMed_valTot(name, prefix, lim = lim_threshold, 
                                     method = approach, return = "valTot")
    
    # Check if processing was successful (valid road pixels extracted)
    if (dim(val_tot_tmp)[1] != 1) {
      # Store road pixel data with metadata
      val_tot = rbind(val_tot, cbind(val_tot_tmp, img_name = name, surf_type = "unk"))
    }
  }
  
  # Convert to data frame and ensure RGB columns are numeric
  val_tot = data.frame(val_tot)
  for (j in 1:3) {
    val_tot[, j] = as.numeric(val_tot[, j])
  }
  
  # Save batch results to separate files for memory management
  final_file_batch = paste(paste(final_file, batch_num, sep = ""), "RData", sep = ".")
  save(val_tot, file = final_file_batch)
}

 
# PHASE 2: SAMPLE REPRESENTATIVE PIXELS ======================================
# Sample n=150 points per road segment to create manageable feature vectors
# This reduces computational complexity while maintaining representative samples

prefix = "preprocessed-data/val_tot_unknown_"
suffix = ".RData"

names_test = character()      # Image names for test set
test = matrix(nrow = 0, ncol = 3)  # Sampled pixel matrix (RGB values)

set.seed(4321)  # Ensure reproducible sampling
start_time = Sys.time()

# Process all 14 batch files created in Phase 1
for (dataset in 1:14) {
  
  filename = paste(paste(prefix, dataset, sep = ""), suffix, sep = "")
  load(filename)
  
  print(paste("Working with dataset ", filename, sep = ""))
  
  names_test_local = as.character(unique(val_tot$img_name))
  
  N_sample = 150  # Number of pixels to sample per image
  n_images = length(names_test_local)  
  
  # Sample pixels from each road segment
  for (i in 1:n_images){ 
    index = which(val_tot$img_name == names_test_local[i])
    
    # Sample with or without replacement based on available pixels
    if (length(index) >= N_sample) {
      index = sample(index, N_sample)  # Sample without replacement
    } else {
      index = sample(index, N_sample, replace = TRUE)  # Sample with replacement
    }
    
    test = rbind(test, val_tot[index, 1:3])  # Add RGB values to test matrix
  }
  
  names_test = c(names_test, names_test_local)  # Accumulate image names
}

end_time = Sys.time()
processing_time = end_time - start_time
print(paste("Sampling completed in:", processing_time))  # ~13 hours for full dataset

# Save the sampled test data
save(test, names_test, file = "val_n150_unknown.RData")
rm(val_tot, prefix, suffix, filename, names_test_local, i, dataset, index)


# PHASE 3: LOAD TRAINING DATA ================================================
# Load pre-processed training data from road segments with known surface types

load("preprocessed-data/val_n150_known.RData")
train = val              # Training pixel data (RGB values)
names_train = img_name   # Training image names
class_train = osm_surf   # Ground truth surface classifications


# PHASE 4: k-NN CLASSIFICATION WITH ENERGY DISTANCE =========================
# Classify unknown road segments using k-nearest neighbors with energy distance

# Clear workspace and reload test data
rm(list = ls())
load("val_n150_unknown.RData")

# Initialize classification parameters
n_unk_images = length(names_test)    # Number of unknown images to classify
n_images = length(names_train)       # Number of training images

# Classification output variables
class_unk = character(n_unk_images)  # Predicted surface classes
frequency = numeric(n_unk_images)    # Confidence scores (frequency of "paved" in k-NN)

# Algorithm parameters
N_pixels = 150    # Pixels per image (consistent with sampling)
f1 = 0.2         # Lower threshold for "unpaved" classification
f2 = 0.6         # Upper threshold for "paved" classification
k_neig = 5       # Number of nearest neighbors to consider

# Create index arrays for efficient data access
sizes = rep(N_pixels, n_unk_images)
cumsizes = cumsum(c(1, sizes))

sizes_known = rep(N_pixels, n_images)
cumsizes_known = cumsum(c(1, sizes_known))
rm(sizes, sizes_known)

# Main classification loop: Process each unknown image
print("Starting k-NN classification...")
start_time = Sys.time()

for (i in 1:n_unk_images) {
  
  # Extract pixel cloud for current unknown image
  index = cumsizes[i]:(cumsizes[i + 1] - 1)
  nube_unk = test[index, ]  # "nube" = cloud (pixel cloud)
  
  # Initialize arrays for k nearest neighbors
  min_dists = numeric(k_neig)  # Minimum distances found so far
  mins = 1:k_neig             # Indices of nearest neighbors
  
  # Compare with each training image
  for (j in 1:n_images) {
    index_known = cumsizes_known[j]:(cumsizes_known[j + 1] - 1)
    nube_known = train[index_known, ]
    
    # Calculate energy distance between pixel clouds
    temp = energy::edist(rbind(nube_unk, nube_known), sizes = c(N_pixels, N_pixels), distance = FALSE)
    
    # Initialize with first k_neig distances
    if (j <= k_neig) {
      min_dists[j] = temp
    }
    # Update nearest neighbors if closer match found
    else if (temp < min_dists[k_neig]) {
      min_dists[k_neig] = temp
      mins[k_neig] = j
      
      # Resort to maintain ordered list
      perm = order(min_dists)
      min_dists = min_dists[perm]
      mins = mins[perm]
    }
  }
  
  # Make classification decision based on k nearest neighbors
  frequency[i] = sum(class_train[mins] == "paved") / k_neig
  
  # Apply frequency-based decision rules
  if (frequency[i] >= f2) {
    class_unk[i] = "paved"      # High confidence paved
  } else if (frequency[i] <= f1) {
    class_unk[i] = "unpaved"    # High confidence unpaved  
  } else {
    class_unk[i] = "uncertain"  # Ambiguous classification
  }
  
  # Progress indicator
  if (i %% 100 == 0) {
    print(paste("Processed", i, "of", n_unk_images, "images"))
  }
} 

classification_time = Sys.time() - start_time
print(paste("Classification completed in:", classification_time))

# Clean up temporary variables and save results
rm(temp, mins, min_dists, i, j, nube_known, nube_unk, index, index_known, cumsizes, cumsizes_known)
save.image(file = "output/k-nn_only_images/classifyMaputo_e_workspace.RDATA")


# PHASE 5: RESULTS ANALYSIS AND VISUALIZATION ===============================

# Summary statistics of classification results
print("Classification Results Summary:")
print(summary(as.factor(class_unk)))

# Visualize confidence distribution
hist(frequency, main = "Distribution of Classification Confidence", 
     xlab = "Frequency of 'Paved' in k-NN", ylab = "Count")

# Create spatial visualization of results
library(sf)
library(ggplot2)
library(RColorBrewer)

# Load original road network shapefile
data_poly = st_read("data/shapefile/Road_cleaned.shp")
unk_index = which(data_poly$osm_surf == "unk")

# Create comprehensive results dataset
data_poly4 = cbind(data_poly[unk_index, ], 
                   pred = 1, 
                   surf_pred = class_unk, 
                   prob_pred = frequency)

# Add known road segments for context
data_poly4 = rbind(data_poly4, 
                   cbind(data_poly[-unk_index, ],
                         pred = 0, 
                         surf_pred = paste(data_poly$osm_surf[-unk_index], "known", sep = "_"),
                         prob_pred = NA))

# Export results as shapefile for GIS analysis
st_write(data_poly4, "output/k-nn_only_images/Road_KNN_energy.shp", 
         delete_dsn = TRUE)  # Overwrite if exists

# Prepare data for visualization
data_poly4$surf_pred[which(data_poly4$surf_pred == "paved")] = "3.paved"
data_poly4$surf_pred[which(data_poly4$surf_pred == "unpaved")] = "1.unpaved"
data_poly4$surf_pred[which(data_poly4$surf_pred == "paved_known")] = "4.paved_known"
data_poly4$surf_pred[which(data_poly4$surf_pred == "unpaved_known")] = "2.unpaved_known"

# Create map showing predicted surface types
cols_surface = c("#FFCB6A", "red3", "gray50", "gray20")  # Color palette for surface types
which_pred = which(data_poly4$pred == 1)  # Only show predicted segments
cols_pred = cols_surface[c(1, 3)]  # Colors for predicted classes

# Plot 1: Surface type predictions
dev.new()
map_surface = ggplot() + 
  geom_sf(data = data_poly4[which_pred, ], 
          aes(color = surf_pred[which_pred])) +
  scale_color_manual(values = cols_pred) +
  labs(color = "Predicted type") +
  ggtitle("Maputo Road Network - Predicted Surface Types") + 
  coord_sf() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FAFAFA"))

print(map_surface)

# Plot 2: Classification confidence levels
cols_confidence = brewer.pal(n = 11, name = "Spectral")
dev.new()
map_confidence = ggplot() + 
  geom_sf(data = data_poly4[which_pred, ], 
          aes(color = as.factor(prob_pred[which_pred]))) +
  scale_color_manual(values = cols_confidence) +
  labs(color = "Paved frequency") +
  ggtitle("Maputo Road Network - Classification Confidence") + 
  coord_sf() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FAFAFA"))

print(map_confidence)

# Print final summary
print("=== CLASSIFICATION PIPELINE COMPLETED ===")
print(paste("Total unknown segments processed:", n_unk_images))
print(paste("Classification accuracy will depend on training data quality"))
print(paste("Results saved to: output/k-nn_only_images/"))
print("Maps generated for visual inspection of results")
  