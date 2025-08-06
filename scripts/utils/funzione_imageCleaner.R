# FUNCTION IMAGE_CLEANER -------------------------------------------------------

# - input: img=RasterBrick object and norm_limit=scalar in [0,255]
# - output: RasterBrick object
# - keep only the three RGB bands, discard the others, rename as redBand, greenBand, blueBand
# - remove black pixel (since in this framework black pixel = outside street borders)
#   - if the image is fully black, return a RasterBrick object with dimensions ncol=1, nrow=1
# - remove all pixels whose L2 norm is not greater of norm_limit

image_cleaner <- function(img, norm_limit=0) {
  
  # remove the fourth band
  values(img) <- values(img)[,1:3]
  
  # visualize max and min
  img <- setMinMax(img)
  
  # change name to colour bands
  names(img) <- c("redBand", "greenBand", "blueBand")
  
  # black pixel check: remove them, return dummy brick
  img[(values(sum(img))==0)] <- NA
  
  if (sum(!is.na(values(img)))==0)
  {
    print("image is totally black! dummy brick is returned")
    return (brick(nrow=1, ncol=1))
  }
  
  # dark pixel check: remove pixels if darker than norm_limit
  if (norm_limit > 0) {
    L2_norm <- function(x) {sqrt(sum(x^2))}
    val_norm <- apply(values(img),1, L2_norm)
    val_rows <- which(val_norm < norm_limit)
    values(img)[val_rows,] <- NA
  }
  
  # trim the image if needed
  img <- trim(img)
  
  return(img)
}

