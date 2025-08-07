# DATA ANALYSIS: DISTANCE CHOICE -----------------------------------------------


## DISTANCE COMPUTATION --------------------------------------------------------
# Computation of distances among all the street pixel distributions of known
# pavement surface
# Distances to be studied: Euclidean distance between Tukey medians,
# Hausdorff distance, Energy distance, Wasserstein distance
#    NB: Euclidean distance was always used as groud distance

rm(list = ls())
graphics.off()

library(terra); library(rgl) 

# Load and prepare data:
load("preprocessed-data/val_n150_known.RData")
n = 150
N = length(img_name)

cumsizes = cumsum(c(1,rep(n, N)))
h_dist = w_dist = e_dist = m_dist = matrix(0, ncol=N, nrow=N)
haus_dist2 = function(dist) { return( max (max(apply(dist,1,min)),max(apply(dist,2,min))) ) }

# Compute hausdorff, energy and wasserstein distances:
for (i in 1:N){
  for (j in i:N){
    print(paste0("iteration i=", i, "; j=",j))
    nube1 = val[cumsizes[i]:(cumsizes[i+1]-1),1:3]
    nube2 = val[cumsizes[j]:(cumsizes[j+1]-1),1:3]
    
    d_temp = as.matrix(dist(rbind(nube1, nube2), method="euclidean"))
    
    e_temp = energy::edist(d_temp, sizes=rep(n, 2), distance=T)
    e_dist[i,j] = e_dist[j,i] = as.matrix(e_temp)[1,2]
    
    d_temp = d_temp[1:n, (n+1):(2*n)]
    h_dist[i,j] = h_dist[j,i] = haus_dist2(d_temp)
    
    w_dist[i,j] = w_dist[j,i] = transport::wasserstein(transport::pp(nube1),
                                                       transport::pp(nube2))
  }
} 
rm (nube1,nube2,i,j,d_temp)  # rimosso 'perm' che non esisteva

# Compute distance between Tukey medians:
load("preprocessed-data/val_med_n150.RData")
m_dist = as.matrix(dist(val_med_red[,1:3], method="euclidean"))

# Save all distances for later:
lista = list(h_dist=h_dist, 
             e_dist=e_dist,
             median_dist = m_dist,
             w_dist=w_dist)
save(lista, file="preprocessed-data/distances_n150_RGB.RData")


## MER COMPARISON --------------------------------------------------------------
# Evaluation of the performances of k-NN when using different distances between
# distributions. Distances are contained in "distances_n150_2.RData"

rm(list = ls())
graphics.off()

library(terra); library(rgl) 

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


### Tukey median distance: ------------------------------------------------------

# Loading the (already computed) distance between Tukey medians of distributions:
load(file="preprocessed-data/distances_n150_RGB.RData")
d = lista$median_dist[perm,perm] ; rm(lista)

# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER
k_neig = 1:70
k_fold = 10
MER = knn_k_choice(d[1:N_train, 1:N_train], k_neig=k_neig, 
                   k_fold=k_fold, true_class_train=osm_surf[1:N_train])

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values")
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using median_distance")
}
dev.off()

k_neig_chosen = 10 # Choice: k in {10,...,20} -> 10 neighbours


# K-NN with k=10: 
# Evaluation of the model performance on the test set
knn_result_m = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                            train_class_labels=osm_surf[1:N_train],
                            test_class_labels=osm_surf[-c(1:N_train)])

pred_class_test_m = knn_result_m$pred_class
prob_m = knn_result_m$frequency

MER_test_m = knn_result_m$MER
print(MER_test_m)

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values",
       ylim=c(min(c(MER_test_m,MER)), max(c(MER_test_m,MER))))
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using median_distance")
  abline(v=k_neig_chosen, lty=3, col="red")
  points(k_neig_chosen, MER_test_m, pch=20, col="red")
}
graphics.off()


# ROC curve:
{
  sub = paste0("kNN with k=",k_neig_chosen,", rectangular kernel, m_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob_m)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm_m = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test_m)
cm_m
MER_test_m



### Hausdorff distance: --------------------------------------------------------

# Loading the (already computed) Hausdorff distance between distributions:
load(file="preprocessed-data/distances_n150_RGB.RData")
d = lista$h_dist[perm,perm] ; rm(lista)

# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER
k_neig = 1:70
k_fold = 10
MER = knn_k_choice(d[1:N_train, 1:N_train], k_neig=k_neig, 
                   k_fold=k_fold, true_class_train=osm_surf[1:N_train])
  
{
  x11()
  plot(k_neig, MER, type="l", xlab="k values")
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using h_distance")
}
dev.off()

k_neig_chosen = 5 # Choice: k=5 neighbours


# K-NN with k=5: 
# Evaluation of the model performance on the test set
knn_result_h = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                              train_class_labels=osm_surf[1:N_train],
                              test_class_labels=osm_surf[-c(1:N_train)])
  
pred_class_test_h = knn_result_h$pred_class
prob_h = knn_result_h$frequency
  
MER_test_h = knn_result_h$MER
print(MER_test_h)
  
{
  x11()
  plot(k_neig, MER, type="l", xlab="k values",
       ylim=c(min(c(MER_test_h,MER)), max(c(MER_test_h,MER))))
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using h_distance")
  abline(v=k_neig_chosen, lty=3, col="red")
  points(k_neig_chosen, MER_test_h, pch=20, col="red")
}
graphics.off()


# ROC curve:
{
  sub = paste0("kNN with k=",k_neig_chosen,", rectangular kernel, h_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob_h)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm_h = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test_h)
cm_h
MER_test_h


### Energy distance: ---------------------------------------------------------

# Loading the (already computed) Energy distance between distributions:
load(file="preprocessed-data/distances_n150_RGB.RData")
d = lista$e_dist[perm,perm] ; rm(lista)

# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER
k_neig = 1:50
k_fold = 10
MER = knn_k_choice(d[1:N_train, 1:N_train], k_neig=k_neig, 
                   k_fold=k_fold, true_class_train=osm_surf[1:N_train])

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values")
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using e_distance")
}
dev.off()

k_neig_chosen = 5 # Choice: k=5 neighbours


# K-NN with k=5: 
# Evaluation of the model performance on the test set
knn_result_e = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                            train_class_labels=osm_surf[1:N_train],
                            test_class_labels=osm_surf[-c(1:N_train)])

pred_class_test_e = knn_result_e$pred_class
prob_e = knn_result_e$frequency

MER_test_e = knn_result_e$MER
print(MER_test_e)

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values",
       ylim=c(min(c(MER_test_e,MER)), max(c(MER_test_e,MER))))
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using e_distance")
  abline(v=k_neig_chosen, lty=3, col="red")
  points(k_neig_chosen, MER_test_e, pch=20, col="red")
}
graphics.off()


# ROC curve:
{
  sub = paste0("kNN with k=",k_neig_chosen,", rectangular kernel, e_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob_e)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm_e = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test_e)
cm_e
MER_test_e





### Wasserstein distance:------------------------------------------------------

# Loading the (already computed) Energy distance between distributions:
load(file="preprocessed-data/distances_n150_RGB.RData")
d = lista$w_dist[perm,perm] ; rm(lista)

# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER
k_neig = 1:70
k_fold = 10
MER = knn_k_choice(d[1:N_train, 1:N_train], k_neig=k_neig, 
                   k_fold=k_fold, true_class_train=osm_surf[1:N_train])

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values")
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using w_distance")
}
dev.off()

k_neig_chosen = 7 # Choice: k in {5,6,7} -> 7 neighbours


# K-NN with k=7: 
# Evaluation of the model performance on the test set
knn_result_w = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                            train_class_labels=osm_surf[1:N_train],
                            test_class_labels=osm_surf[-c(1:N_train)])

pred_class_test_w = knn_result_w$pred_class
prob_w = knn_result_w$frequency

MER_test_w = knn_result_w$MER
print(MER_test_w)

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values",
       ylim=c(min(c(MER_test_w,MER)), max(c(MER_test_w,MER))))
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using w_distance")
  abline(v=k_neig_chosen, lty=3, col="red")
  points(k_neig_chosen, MER_test_w, pch=20, col="red")
}
graphics.off()


# ROC curve:
{
  sub = paste0("kNN with k=",k_neig_chosen,", rectangular kernel, w_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob_w)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm_w = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test_w)
cm_w
MER_test_w


## COMPARISON OF COMPUTATIONAL TIME --------------------------------------------

rm(list = ls())
graphics.off()

library(terra); library(rgl) 

# Load and prepare data:
load("preprocessed-data/val_n150_known.RData")
n = 150
N = length(img_name)

data_fake = val[1:n,1:3]
cumsizes = 1+(n*0:N)

n_trials = 10


### Tukey median distance:  ---------------------------------------------------
# NB: computational time does not include the time to compute Tukey medians
# of data from the training set
m_dist = numeric(n)
m_diff_time = NULL

load("preprocessed-data/val_med_n150.RData")
val_med_red = val_med_red[,1:3]

for (trial in 1:n_trials) {  # cambiato nome variabile da 'i' per evitare conflitto
  start = Sys.time()
  m0 = DepthProc::depthMedian(data_fake, depth_params=list(method="Tukey"))
  for (i in 1:n){
    m1 = val_med_red[i,]
    m_dist[i] = sqrt(sum((m1-m0)^2))
  } 
  end = Sys.time()
  m_diff_time = c(m_diff_time, difftime(end, start, units="secs"))
}
mean(m_diff_time) # 2.0621 con 3, 1.6559 con 10


### Hausdorff distance: --------------------------------------------------------
h_dist = numeric(n)
h_diff_time = NULL
haus_dist2 = function(dist) { return( max (max(apply(dist,1,min)),max(apply(dist,2,min))) ) }

for (trial in 1:n_trials) {  # cambiato nome variabile da 'i' per evitare conflitto
  
  start = Sys.time()
  for (i in 1:N){
    nube1 = val[cumsizes[i]:(cumsizes[i+1]-1),]
    
    d_temp = as.matrix(dist(rbind(nube1, data_fake), method="euclidean"))[1:n, (n+1):(2*n)]
    h_dist[i] = haus_dist2(d_temp)
  } 
  end = Sys.time()
  h_diff_time = c(h_diff_time, difftime(end, start, units="secs"))
}
mean(h_diff_time) #8.3213 con 3, 8.1066 con 10


### Energy distance:-----------------------------------------------------------
e_dist = numeric(n)
e_diff_time = NULL

for (trial in 1:n_trials) {  # cambiato nome variabile da 'i' per evitare conflitto
  
  start = Sys.time()
  for (i in 1:N){
    nube1 = val[cumsizes[i]:(cumsizes[i+1]-1),]
    
    d_temp = as.matrix(dist(rbind(nube1, data_fake), method="euclidean"))
    
    e_temp = energy::edist(d_temp, sizes=c(dim(nube1)[1], dim(data_fake)[1]), distance=T)
    e_dist[i] = as.matrix(e_temp)[1,2]
  } 
  end = Sys.time()
  e_diff_time = c(e_diff_time, difftime(end, start, units="secs"))
}
mean(e_diff_time) # 6.030568 secs con 3, 5.6692 con 10



### Wasserstein distance: ------------------------------------------------------
w_dist = numeric(n)
w_diff_time = NULL

for (trial in 1:n_trials) {  # cambiato nome variabile da 'i' per evitare conflitto
  start = Sys.time()
  for (i in 1:N){
    nube1 = val[cumsizes[i]:(cumsizes[i+1]-1),]
    
    w_dist[i] = transport::wasserstein(transport::pp(nube1),
                                       transport::pp(data_fake))
  } 
  end = Sys.time()
  w_diff_time = c(w_diff_time, difftime(end, start, units="secs"))
}
mean(w_diff_time) #23.5147 con 3, 21.3988 con 10



## CONCLUSION: ----------------------------------------------------------------
# The Energy distance provides us with a lower Misclassification Error Rate and
# a lower computational time, hence will be used fot further analysis