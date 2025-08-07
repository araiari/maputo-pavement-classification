# DATA ANALYSIS: COLOUR SPACES ---------------------------------------------

# GOAL: find the best colour space for street pixels in our application
# Three colour spaces will be considered: RGB, R'G'B' (or sRGB) and HSV
# Data will be transformed in each colour space and k-NN will be performed
# Furthermore, HSV data will be equalized (histogram equalization)


## DISTANCE COMPUTATION --------------------------------------------------------

### R'G'B' space ---------------------------------------------------------------

# Load and prepare data:
rm(list=ls())

transform_sRGB = function (x, gamma=2.5) {
    # Normalization of the vector x in [0,1]:
  x = x/255
    # Transformation in sRGB:
  x = x^(1/gamma)
    # Back to [0,255]:
  x = x*255
  return(x)
}

load("preprocessed-data/val_n150_known.RData")
n = 150
N = length(img_name) 
val = transform_sRGB(val)

# Compute Energy distance between all distributions:
cumsizes = cumsum(c(1,rep(n,N)))
e_dist = matrix(0, ncol=N, nrow=N)

pb = txtProgressBar(min=1, max=(N+1)*N/2, style=3)
i_pb = 0

for (i in 1:N){
  nube1 = val[cumsizes[i]:(cumsizes[i+1]-1),]
  for (j in i:N){
    nube2 = val[cumsizes[j]:(cumsizes[j+1]-1),]
    
    d_temp = as.matrix(dist(rbind(nube1, nube2), method="euclidean"))
      
    e_temp = energy::edist(d_temp, sizes=c(n,n), distance=T)
    e_dist[i,j] = e_dist[j,i] = as.matrix(e_temp)[1,2]

    i_pb = i_pb+1
    setTxtProgressBar(pb, i_pb)
  }
}
close(pb)

lista = list(e_dist = e_dist)
save(lista, file="preprocessed-data/distances_n150_sRGB.RData")

# ora nel file "difference_distances_sRGB_AU.RData" oppure "difference_distances_sRGB2_AU.RData"



### HSV space ------------------------------------------------------------------

# Load and prepare data:
rm(list=ls())

transform_hsv = function(x) {
  # x = matrix or dataframe
  # Transformation through R function rgb2hsv:
  x = t(rgb2hsv(r=x[,1], g=x[,2], b=x[,3]))
  colnames(x) = c("hue", "saturation", "brightness")
  # Hue angle in [-pi, pi]:
  x[,1] = x[,1]*2*pi
  x[which(x[,1]>pi),1] = x[which(x[,1]>pi),1]-(2*pi)
  return (x)
}

load("preprocessed-data/val_n150_known.RData")
val = transform_hsv(val)
n = 150
N = length(img_name)  


# Since the computation is too expensive, it will be performed in C++
# Save all the data in a txt file:
{
  # Save image names:
  for (i in 1:N){
    x = img_name[i]
    write.table(x, file="preprocessed-data/val_n150_names.txt", append=T, sep = " ",
                row.names=F, col.names=F, eol="\n", quote=F)
  }
  
  # Save HSV data:
  val = data.frame(val)
  for (i in 1:N){
    x = val[((i-1)*n+1):(i*n), ]
    write.table(x, file="preprocessed-data/cpp-evaluation/val_n150_dataHSV.txt", append=T, sep = " ", dec = ".",
                row.names=F, col.names=F, eol="\n")
  }
}

# After computation in C++:
{
  e_dist_hsv <- read.csv("preprocessed-data/cpp-elaboration/energy_distance_HSV.txt", header=FALSE, row.names=NULL)
  
  for (i in 1:N)
    for (j in i:N){
      e_dist_hsv[j,i] = e_dist_hsv[i,j]
    }
  diag(e_dist_hsv) = 0
  e_dist_hsv = e_dist_hsv[,-(N+1)]
  lista_dist_HSV = list(e_dist = e_dist_hsv)
  
  save(lista_dist_HSV, file="preprocessed-data/distances_n150_HSV.RData")
  # ora nel file "difference_distances_HSV_AU.RData"
}




### HSV with Histogram Equalization -----------------------------------------

# Load and prepare data:
rm(list=ls())

transform_hsv = function(x) {
  # x = matrix or dataframe
  # Transformation through R function rgb2hsv:
  x = t(rgb2hsv(r=x[,1], g=x[,2], b=x[,3]))
  colnames(x) = c("hue", "saturation", "brightness")
  # Hue angle in [-pi, pi]:
  x[,1] = x[,1]*2*pi
  x[which(x[,1]>pi),1] = x[which(x[,1]>pi),1]-(2*pi)
  return (x)
}

load("preprocessed-data/val_n150_known.RData")
val = transform_hsv(val)
n = 150
N = length(img_name)  

# Histogram equalization of V (value of brightness):
val_eq = numeric(n*N)
for (i in 1:N){
  index = ((i-1)*n+1):(i*n)
  cdf_nube = ecdf (val[index, 3])
  val_eq[index] = cdf_nube(val[index, 3])
}
val = cbind(val[,1:2], val_eq)
val = data.frame(val)
colnames(val) = c("hue", "saturation", "brightness")


# Since the computation is too expensive, it will be performed in C++
# Save all the HSV data with equalization in a txt file:
for (i in 1:N){
  x = val[((i-1)*n+1):(i*n), ]
  write.table(x, file="preprocessed-data/cpp-evaluation/val_n150_dataHSVeq.txt", append=T, sep = " ", dec = ".",
              row.names=F, col.names=F, eol="\n")
}


# After computation in C++:
{
  e_dist_hsv <- read.csv("preprocessed-data/cpp-evaluation/energy_distance_HSVeq.txt", header=FALSE, row.names=NULL)
  
  for (i in 1:N)
    for (j in i:N){
      e_dist_hsv[j,i] = e_dist_hsv[i,j]
    }
  diag(e_dist_hsv) = 0
  e_dist_hsv = e_dist_hsv[,-(N+1)]
  lista_dist_HSVeq = list(e_dist = e_dist_hsv)
  
  save(lista_dist_HSVeq, file="preprocessed-data/distances_n150_HSVeq.RData")
  # ora nel file "difference_distances_HSVeq_AU.RData"
}


## MER COMPARISON -------------------------------------------------------------
# Evaluation of the performances of k-NN when using different colour spaces 
# and energy distance between distributions. 
# Distances are contained in "distances_n150_****.RData", where **** is the 
# name of the colour space 


# Load packages and functions:
rm(list=ls())
graphics.off()

library(rgl) ; close3d()

load("preprocessed-data/distances_n150_sRGB.RData")
source('scripts/utils/funzione_knn_distanza.R')
source('scripts/utils/funzione_knn_k_choice.R')
source('scripts/utils/funzione_knn_compute_MER.R')


# Loading and preparing data:
load("preprocessed-data/val_n150_known.RData"); rm(val)

# Training and test sets:
N = length(img_name)
N_train = round(N*70/100)
N_test = N-N_train

set.seed(123) #123
perm = sample(N, replace=F)
img_name = img_name[perm]
osm_surf = osm_surf[perm]
osm_type = osm_type[perm]


### R'G'B' space: -------------------------------------------------------------
load("preprocessed-data/distances_n150_sRGB.RData")
d = lista$e_dist[perm,perm]; rm(lista)

# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER 
k_neig = 1:50
k_fold = 10
MER = knn_k_choice(d[1:N_train, 1:N_train], k_neig=k_neig, 
                   k_fold=k_fold, true_class_train=osm_surf[1:N_train])

{  
  windows()
  plot(1:length(k_neig), MER, type="l", xlab="k values")
  points(1:length(k_neig), MER, pch=20)
  grid(nx = 107, ny=6)
  title("MER for different k values in the R'G'B' space")
  
  k_neig_chosen = which.min(MER)
  points(k_neig_chosen, MER[k_neig_chosen], pch=20, col="red")
}
dev.off()

# Choice: k in {5,...,8} --> k=5
k_neig_chosen = 5

# K-NN with k=5: 
# Evaluation of the model performance on the test set
knn_result_sRGB = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                               train_class_labels=osm_surf[1:N_train],
                               test_class_labels=osm_surf[-c(1:N_train)])

pred_class_test_sRGB = knn_result_sRGB$pred_class
prob_sRGB = knn_result_sRGB$frequency

MER_test_sRGB = knn_result_sRGB$MER
print(MER_test_sRGB)

{
  windows()
  plot(1:length(k_neig), MER, type="l", xlab="k values")
  points(1:length(k_neig), MER, pch=20)
  grid(nx = 107, ny=6)
  title("MER for different k values in the R'G'B' space")
  
  points(k_neig_chosen, MER_test_sRGB, pch=20, col="red")
  abline(v=k_neig_chosen, col="red", lwd=0.1, lty=3)
}
dev.off()

# ROC curve: 
{
  sub = paste0("kNN with k=",k_neig_chosen,", R'G'B' space, e_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob_sRGB)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm_sRGB = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test_sRGB)
cm_sRGB
MER_test_sRGB


### HSV space: -------------------------------------------------------------
load("preprocessed-data/distances_n150_HSV.RData")
d = lista$e_dist[perm,perm]; rm(lista)

# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER 
k_neig = 1:50
k_fold = 10
MER = knn_k_choice(d[1:N_train, 1:N_train], k_neig=k_neig, 
                   k_fold=k_fold, true_class_train=osm_surf[1:N_train])

{  
  windows()
  plot(1:length(k_neig), MER, type="l", xlab="k values")
  points(1:length(k_neig), MER, pch=20)
  grid(nx = 107, ny=6)
  title("MER for different k values in the HSV space")
  
  k_neig_chosen = which.min(MER)
  points(k_neig_chosen, MER[k_neig_chosen], pch=20, col="red")
}
dev.off()

# Choice: k in {5,6,7} --> k=5
k_neig_chosen = 5

# K-NN with k=5: 
# Evaluation of the model performance on the test set
knn_result_HSV = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                               train_class_labels=osm_surf[1:N_train],
                               test_class_labels=osm_surf[-c(1:N_train)])

pred_class_test_HSV = knn_result_HSV$pred_class
prob_HSV = knn_result_HSV$frequency

MER_test_HSV = knn_result_HSV$MER
print(MER_test_HSV)

{
  windows()
  plot(1:length(k_neig), MER, type="l", xlab="k values", 
       ylim=c(min(MER, MER_test_HSV), max(MER, MER_test_HSV)))
  points(1:length(k_neig), MER, pch=20)
  grid(nx = 107, ny=6)
  title("MER for different k values in the HSV space")
  
  points(k_neig_chosen, MER_test_HSV, pch=20, col="red")
  abline(v=k_neig_chosen, col="red", lwd=0.1, lty=3)
}
dev.off()

# ROC curve: 
{
  sub = paste0("kNN with k=",k_neig_chosen,", HSV space, e_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob_HSV)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm_HSV = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test_HSV)
cm_HSV
MER_test_HSV



### HSV with Histogram Equalization: -------------------------------------------
load("preprocessed-data/distances_n150_HSVeq.RData")
d = lista$e_dist[perm,perm]; rm(lista)

# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER 
k_neig = 1:50
k_fold = 10
MER = knn_k_choice(d[1:N_train, 1:N_train], k_neig=k_neig, 
                   k_fold=k_fold, true_class_train=osm_surf[1:N_train])

{  
  windows()
  plot(1:length(k_neig), MER, type="l", xlab="k values")
  points(1:length(k_neig), MER, pch=20)
  grid(nx = 107, ny=6)
  title("MER for different k values in the HSV space (equalized V)")
  
  k_neig_chosen = which.min(MER)
  points(k_neig_chosen, MER[k_neig_chosen], pch=20, col="red")
}
dev.off()

# Choice: k in {5,6,7} --> k=5
k_neig_chosen = 5

# K-NN with k=5: 
# Evaluation of the model performance on the test set
knn_result_HSVeq = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                              train_class_labels=osm_surf[1:N_train],
                              test_class_labels=osm_surf[-c(1:N_train)])

pred_class_test_HSVeq = knn_result_HSVeq$pred_class
prob_HSVeq = knn_result_HSVeq$frequency

MER_test_HSVeq = knn_result_HSVeq$MER
print(MER_test_HSVeq)

{
  windows()
  plot(1:length(k_neig), MER, type="l", xlab="k values")
  points(1:length(k_neig), MER, pch=20)
  grid(nx = 107, ny=6)
  title("MER for different k values values in the HSV space (equalized V)")
  
  points(k_neig_chosen, MER_test_HSVeq, pch=20, col="red")
  abline(v=k_neig_chosen, col="red", lwd=0.1, lty=3)
}
dev.off()

# ROC curve: 
{
  sub = paste0("kNN with k=",k_neig_chosen,", HSVeq space, e_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob_HSVeq)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm_HSVeq = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test_HSVeq)
cm_HSVeq
MER_test_HSVeq


## CONCLUSION: -----------------------------------------------------------------
# Computation of distances in HSV space is really expensive in R, and C++ was used
# MER is comparable for RGB and HSV, but the former was preferredd


