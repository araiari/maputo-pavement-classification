# Road Pixel Analysis Function: crea_valMed_valTot
# This function performs automated road detection and statistical analysis on satellite/aerial images
#
# Input parameters:
# - name: image filename
# - prefix: path to the image directory
# - lim: threshold for removing trees/vegetation using image_cleaner (default: 90)
# - img: optional pre-cleaned image object
# - method: DBSCAN parameter selection method ("permissive" or "strict")
# - return: what to return ("valMed", "valTot", or "all")
#
# Returns:
# - valMed: matrix with Tukey median coordinates of road pixels
# - valTot: matrix with road pixel coordinates + seed/non-seed classification
# - all: both valMed and valTot as a list

# Load required packages:
{
  graphics.off()  # Close any open graphics devices
  
  library(sp)
  library(terra)     # Replaces rgdal and raster packages
  library(rgl)       # For 3D graphics
  library(DepthProc) # For depth-based statistical methods
  library(fpc)       # For DBSCAN clustering
}

# Define utility functions:
{
  # Calculate volume of a sphere given radius
  area = function(x) {return(4/3 * pi * x^3)}
  
  # Calculate Euclidean (L2) norm of a vector
  norm2 = function(x) {sqrt(sum(x^2))}
  
  # Calculate cylindrical volume for density estimation
  # l = cylinder height = diagonal of color space
  area_all = function(x, l = 255 * sqrt(3)) {l * x^2 * pi}
}

# Load custom functions:
{
  source("scripts/utils/funzione_DBSCANimages.R")
  source("scripts/utils/funzione_imageCleaner.R")
  source("scripts/utils/funzione_quantilesPCA.R")
}

crea_valMed_valTot = function(name, prefix, lim = 90, img = NULL,
                              method = c("permissive", "strict"),
                              return = c("valMed", "valTot", "all"))
{
  # Load and clean the image if not provided
  if (is.null(img)) {
    fullname = paste(prefix, name, sep = "/")
    img = rast(fullname)
    img = image_cleaner(img, lim)  
  }
  
  # Handle edge case: if image is completely black or too small
  if (ncol(img) <= 1 && nrow(img) <= 1) {
    print("DUMMY IMAGE: returning 0 (image too small or completely black)")
    
    if (return != "all") return(0)
    else return(list(0, 0))
  }
  
  # Process valid images
  val = na.omit(values(img, na.rm = FALSE))  # Extract pixel values, remove NA
  DIM = dim(val)[1]  # Number of valid pixels
  
  # Calculate epsilon parameter using 75th percentile from PCA analysis
  eps = as.numeric(quantiles_PCA(val, quantiles = 0.75))
  eps = round(eps, 2)
  
  # Calculate MinPts parameter based on selected method
  if (method == "permissive") {
    # Permissive method: lower density threshold, more inclusive clustering
    l = 255 * sqrt(3) - lim  # Effective color space height
    dens_strada = DIM * 0.75 / area_all(eps, l)  # Estimated road density
    costante = 1 / 0.75  # Scaling constant
    MP = round(dens_strada * area(eps) * costante)
  }
  
  else if (method == "strict") {
    # Selective method: higher density threshold, more restrictive clustering
    l = max((255 * sqrt(3) - lim) / 2, diff(range(apply(val, 1, norm2))))
    dens_strada = DIM * 0.75 / area_all(eps, l)  # Estimated road density
    costante = 1.75  # Higher scaling constant for selectivity
    MP = round(dens_strada * area(eps) * costante)
  }
  
  else {
    print("ERROR: select a method between 'permissive' and 'strict'")
    
    if (return != "all") return(1)
    else return(list(1, 1))
  }
  
  # Apply DBSCAN clustering to identify road pixels
  set.seed(1234)  # Ensure reproducibility
  valTot = dbscan_images(name, img = img, lim = lim, eps = eps, MP = MP, 
                        plot = FALSE, tables = FALSE, strada_values = TRUE)

  # Return requested results
  if (return == "valTot") {
    # Return all road pixel coordinates with seed classification
    return(valTot)
  }
  
  else if (return == "valMed") {
    # Calculate and return Tukey depth median of road pixels
    med = DepthProc::depthMedian(valTot[, 1:3], depth_params = list(method = "Tukey"))
    valMed = rbind(med)  # Ensure matrix format
    return(valMed)
  }
  
  else if (return == "all") {
    # Return both total road pixels and their median
    med = DepthProc::depthMedian(valTot[, 1:3], depth_params = list(method = "Tukey"))
    valMed = rbind(med)  # Ensure matrix format
    return(list(valTot, valMed))
  }
  
  else {
    print("ERROR: select a return type among 'valMed', 'valTot', and 'all'")
    return(2)
  }
}