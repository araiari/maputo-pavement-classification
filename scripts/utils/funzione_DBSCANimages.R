
# DBSCAN Image Analysis for Road Detection
# This code implements automated DBSCAN clustering for road detection in satellite/aerial images
# Main function: dbscan_images - applies DBSCAN clustering to image pixel values

# Given an image name and DBSCAN parameters,
# applies DBSCAN clustering to the image pixel values to identify road pixels.

# Returns:
# - Image plots (full image | road-only image) if plot=TRUE
# - Road pixel values if strada_values=TRUE
# - DBSCAN clustering results otherwise

# Load required packages:
library(sp)
library(terra)
library(fpc) 

# Source the image cleaning function
source("scripts/utils/funzione_imageCleaner.R")

dbscan_images = function(img_name, img=NULL, lim=0, eps, MP, plot=FALSE, strada_values=FALSE, tables=FALSE) 
{
  # Create descriptive title for the analysis
  title_text = paste(img_name, paste(paste(" DBSCAN with eps=", eps), paste("and MP=", MP), sep=" "), sep=": ")
  print(title_text)
  
  # Load and clean the image if not provided
  if (is.null(img)){
    img = rast(img_name)  # Use terra::rast instead of raster::brick
    img = image_cleaner(img, norm_limit=lim)
  }
  
  # Extract pixel values, excluding NA values:
  val = values(img, na.rm=FALSE)  # terra syntax
  val_rows = which(apply(is.na(val), 1, sum) == 0)  # Identify complete rows
  val = na.omit(val)  # Remove rows with NA values
  
  # Apply DBSCAN clustering:
  set.seed(1234)  # Set seed for reproducibility
  c1 = dbscan(val, eps=eps, MinPts=MP)
  
  # Identify seed points and determine which cluster represents roads
  seeds <- c1$isseed
  seeds[seeds] <- "seed"
  seeds[seeds != "seed"] <- "no"
  
  # Find cluster with most seed points (likely to be roads)
  c1_strada <- which.max(summary(as.factor(c1$cluster[seeds == "seed"])))
  
  # Extract road pixel indices and values
  strada_index <- which(c1$cluster == c1_strada)
  val_s = cbind(values(img, na.rm=FALSE)[val_rows[strada_index], ], isseed=seeds[strada_index])
  
  # Print clustering summary tables if requested
  if (tables == TRUE) {
    print(table(seeds, cluster=c1$cluster))
    print(paste("Cluster with most seeds =", as.numeric(c1_strada), sep=" "))
  }
  
  # Generate plots showing: roads, non-roads, and dark pixels (if threshold applied)
  if (plot == TRUE) {
    
    # Set up plotting window
    x11(width=8, height=8, bg="white")
    
    # Determine subplot layout based on threshold parameter
    if (lim == 0){ 
      par(mfrow=c(1,2))
    } else {
      par(mfrow=c(1,3))
    }
    
    # Set plot aesthetics
    par(col.axis="white", col.lab="white", tck=0)
    
    # Plot 1: Roads only (set non-road pixels to NA)
    new_black <- which(c1$cluster != c1_strada)
    img_copy <- img
    values(img_copy)[val_rows[new_black], ] <- NA
    
    plotRGB(img_copy, axes=TRUE, main="Roads")
    box(col="#23373b")
    
    # Plot 2: Non-roads only (set road pixels to NA)
    img <- image_cleaner(rast(img_name), lim)  # Reload original image
    new_black <- which(c1$cluster == c1_strada)
    values(img)[val_rows[new_black], ] <- NA
    
    plotRGB(img, axes=TRUE, main="Non-roads")
    box(col="#23373b")
    
    # Plot 3: Dark pixels (only if threshold is applied)
    if (lim != 0) {
      # Define L2 norm function for pixel intensity
      norm2 = function(x) {sqrt(sum(x^2))}
      
      img <- image_cleaner(rast(img_name))  # Reload without threshold
      new_black <- which(apply(values(img, na.rm=FALSE), 1, norm2) > lim)
      values(img)[new_black, ] <- NA
      
      plotRGB(img, axes=TRUE, main="Dark pixels")
      box(col="#23373b")
    }
    
    # Reset plot layout and add overall title
    par(mfrow=c(1,1))
    title(title_text)
  }
  
  # Return results based on function parameters
  if (strada_values == TRUE) {
    # Return road pixel values as data frame
    val_s = data.frame(val_s)
    for (i in 1:3) {
      val_s[, i] = as.numeric(val_s[, i])
    }
    return(val_s)
  } else {
    # Return DBSCAN clustering results
    return(c1)
  }
}