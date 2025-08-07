
# DATA ANALYSIS: --------------------------------------------------------------
# OBJECT-ORIENTED K-NN WITH ENERGY DISTANCE BETWEEN 
# STREET PIXEL DISTRIBUTIONS IN THE RGB SPACE

# The best objext-oriented k-NN classifier works with Energy distance between
# street pixel distributions (see "Data analysis 2 - distances.R")
# in the RGB space (see "Data analysis 3 - colour spaces.R")
# Here previous results are collected:

rm(list = ls())
graphics.off()


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


# Loading the (already computed) Energy distance between distributions:
load(file="preprocessed-data/distances_n150_RGB.RData")
d = lista$e_dist[perm,perm] ; rm(lista)

# Choice: k=5 neighbours
k_neig_chosen = 5

# Evaluation of the k-NN model performance on the test set
knn_result = knn_distanza(d[(N_train+1):N,1:N_train], k_neig=k_neig_chosen,
                            train_class_labels=osm_surf[1:N_train],
                            test_class_labels=osm_surf[-c(1:N_train)])

pred_class_test = knn_result$pred_class
prob = knn_result$frequency

MER_test = knn_result$MER

# ROC curve:
{
  sub = paste0("kNN with k=",k_neig_chosen,", RGB space, e_dist")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm = table(true=osm_surf[-c(1:N_train)], predicted=pred_class_test)
cm
MER_test

rm (d)
