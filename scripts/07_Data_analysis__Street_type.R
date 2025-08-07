
# DATA ANALYSIS: STREET TYPE INFORMATION ---------------------------------------

rm(list=ls())
graphics.off()


## MODEL 1: STREET TYPE --------------------------------------------------------
# Classification which makes use ONLY of street type information to predict surfaces
# Classification rule: assign the pavement type which is the most frequent in the 
# street type class.
# GOAL: understand the importance of the street type attribute for surface classification

# Training and test sets:
load("preprocessed-data/val_n150_known.RData") ; rm(val)
N = length(img_name)
N_train = round(N*70/100)
N_test = N-N_train

set.seed(123) #123
perm = sample(N, replace=F)
img_name = img_name[perm]
osm_surf = osm_surf[perm]
osm_type = osm_type[perm]

  # Training set:
osm_type1 = as.factor(osm_type[1:N_train])
osm_surf1 = as.factor(osm_surf[1:N_train])


# Create the classification rule by majority voting:
t = table(osm_type1, osm_surf1); print(t)
rule = apply(t,1,which.max)
rule[which(rule==1)] = "paved"
rule[which(rule==2)] = "unpaved"

rule = cbind(type=names(rule), surf=rule)
rownames(rule) = NULL
rule
n_type = dim(rule)[1]


  # Test set:
osm_type2 = osm_type[-c(1:N_train)]
osm_surf2 = osm_surf[-c(1:N_train)]

pred_surf2 = character(N_test)
for (i in 1:n_type) 
  pred_surf2[which(rule[i,1]==osm_type2)] = rule[i,2]


cm = table(true=osm_surf2, predicted=pred_surf2); print(cm)
MER = 1-sum(diag(cm))/sum(cm); print(MER)



## MODEL 2: STREET TYPE AND PIXEL COLOURS --------------------------------------
# The model includes both street type information and pixel colour distributions.
# The surface is predicted by looking at the labels of the k closest pixel 
# distributions. Such distributions are found among the same street type.

rm (list=ls())
source('scripts/utils/funzione_knn_k_choice.R')


### OBJECT-ORIENTED K-NN ------------------------------------------------------

# Training and test sets:
load("preprocessed-data/val_n150_known.RData"); rm(val)
type_name = unique(osm_type)

N = length(img_name)
N_train = round(N*70/100)
N_test = N-N_train

set.seed(123) #123
perm = sample(N, replace=F)
img_name = img_name[perm]
osm_surf = osm_surf[perm]
osm_type = osm_type[perm]

# Loading the (already computed) Energy distance between distributions:
load("preprocessed-data/distances_n150_RGB.RData")
d = lista$e_dist[perm, perm] ; rm(lista)


# Choice of k:
# Compute MER of k_fold Cross Validation, with different k values
# Choose k that minimizes MER
k_neig = 1:50
k_fold = 10
MER = rep(0,length(k_neig))

for (type in type_name) {
  index = which(osm_type[1:N_train]==type)
  MER_type = knn_k_choice(d[index, index], k_neig=k_neig, 
                     k_fold=k_fold, true_class_train=osm_surf[index])
  MER = MER + MER_type*length(index)
}
MER = MER/N_test
rm(MER_type, type, index)

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values")
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using street type and pixel colours")
}
dev.off()

# Choice: k in {5,6,7} neighbours --> k=5
k_neig_chosen = 5 


# K-NN with k=5: 
# Evaluation of the model performance on the test set
k_neig_chosen = 5#oppre 7

pred_class = character(N_test)
prob = numeric(N_test)
MER_test = 0

for (type in type_name){
  index_train = which(osm_type[1:N_train]==type)
  index_test = which(osm_type[-c(1:N_train)]==type)+N_train
  
  knn_result = knn_distanza(d[index_test, index_train],
                            k_neig=k_neig_chosen,
                            train_class_labels=osm_surf[index_train],
                            test_class_labels=osm_surf[index_test])
  pred_class[index_test-N_train] = knn_result$pred_class
  prob[index_test-N_train] = knn_result$frequency
  
  MER_test = MER_test + knn_result$MER*length(index_test) #0.05867
  
}
MER_test = MER_test/N_test
print(MER_test)

rm(knn_result, index_test, index_train, type)

# ROC curve:
{
  source('scripts/utils/funzione_knn_compute_MER.R')
  sub = paste0("kNN with k=",k_neig_chosen,", RGB space pixels, e_dist and street type")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm = table(true=osm_surf[-c(1:N_train)], predicted=pred_class)
cm
MER_test


### THRESHOLD CHOICE ----------------------------------------------------------

# With single threshold f_{up}, no uncertainty allowed:
kk = k_neig_chosen
error_pu = error_up = NULL
doubt = rep(0, kk+1)

p_range = 0:kk/kk
for (p in p_range)
{
  n_pp = length(which(osm_surf[-c(1:N_train)]=="paved"    & prob>=p))
  n_uu = length(which(osm_surf[-c(1:N_train)]=="unpaved"  & prob<p))
  n_pu = length(which(osm_surf[-c(1:N_train)]=="paved"    & prob<p))
  n_up = length(which(osm_surf[-c(1:N_train)]=="unpaved"  & prob>=p))
  
  error_pu = c(error_pu, n_pu)
  error_up = c(error_up, n_up)
}

names(error_pu) = names(error_up) = names(doubt) = paste("i",round(p_range, digits=2),sep="=")

lista_single = list(error_pu=error_pu, error_up=error_up, doubt=doubt)


# With pair of thresholds [f_u, f_p]:
error_pu = error_up = doubt = matrix(NA, kk+1, kk+1)

for (i in (0:kk/kk)) {
  for (j in ((kk*i):kk/kk)){
    print(paste0(i," ",j))
    class_pred = rep("uncertain", N_test)
    which_p = which(prob>j); which_u = which(prob<i)
    class_pred[which_p] = "paved"; class_pred[which_u] = "unpaved"
    
    doubt[kk*i+1, kk*j+1] = length(which(class_pred=="uncertain"))
    error_pu[kk*i+1, kk*j+1] =  length(which(osm_surf[-c(1:N_train)]=="paved" & class_pred=="unpaved"))
    error_up[kk*i+1, kk*j+1] =  length(which(osm_surf[-c(1:N_train)]=="unpaved" & class_pred=="paved"))
  }
}

error_pu[1,kk+1] = error_up[1,kk+1] = 0

rownames(doubt) = rownames(error_up) = rownames(error_pu) = paste("i",round(0:kk/kk, digits=2),sep="=")
colnames(doubt) = colnames(error_up) = colnames(error_pu) = paste("j",round(0:kk/kk, digits=2),sep="=")

lista_pair = list(error_pu=error_pu, error_up=error_up, doubt=doubt)


# Cost computation: 
unit_cost = c(c_pu=2.5, c_up=2, c_d=1)
# unit_cost = c(c_pu=10, c_up=5, c_d=5)

cost_single = unit_cost[1]*lista_single$error_pu + unit_cost[2]*lista_single$error_up +
  unit_cost[3]*lista_single$doubt
print(cost_single)

cost_pair = unit_cost[1]*lista_pair$error_pu + unit_cost[2]*lista_pair$error_up + 
  unit_cost[3]*lista_pair$doubt
print(cost_pair)


# Plot:
library("plot.matrix")
cost_plot = cbind("single"=rep(NA,kk+1), rep(NA,kk+1), rep(NA,kk+1), cost_pair)
diag(cost_plot) = c(cost_single)

{
  col = colorRampPalette(c("snow2", "red3"), 10)
  x11(width=8, height=8)
  plot(cost_plot, main="", cex=1, digits=0, axis.col=list(side=3),
       na.print=F, col=col, xlab="", ylab="", key=NULL)
  mtext("total costs", line=2.5, cex=1.3, font=2)
  rm(col)
}

# Costs are minimized with strict threshold f_{up} = 0.6
# (equivalent to f_{up}=0.5, used for previous results)


f_up = 0.6
class_pred = character(N_test)
which_p = which(prob>=f_up); which_u = which(prob<f_up)
class_pred[which_p] = "paved"; class_pred[which_u] = "unpaved"


t = table(true = osm_surf[-c(1:N_train)], predicted = class_pred); t

# Accuracy:
(t[1,1]+t[2,2])/N_test

# MER:
(t[2,1]+t[1,2])/N_test

# Uncertainty: 0



## 6 INDEPENDENT CLASSIFIERS --------------------------------------------------
# More specific model:
# In the previous model, k is chosen to minimize the total MER on the training set.
# Now, it will be chosen to minimize the MER on the training set independently for each type.
# By doing so, 6 fully independent classifiers are built.
# GOAL: check if such a specialization is needed, 
# or the choice of a common k is good enough.

rm (list=ls())
source('scripts/utils/funzione_knn_k_choice.R')

# Training and test sets:
load("preprocessed-data/val_n150_known.RData"); rm(val)
type_name = unique(osm_type)

N = length(img_name)
N_train = round(N*70/100)
N_test = N-N_train

set.seed(123) #123
perm = sample(N, replace=F)
img_name = img_name[perm]
osm_surf = osm_surf[perm]
osm_type = osm_type[perm]


# Loading the (already computed) Energy distance between distributions:
load("preprocessed-data/distances_n150_RGB.RData")
d = lista$e_dist[perm, perm] ; rm(lista)


# Choice of k: choose k for each street type to minimize MER
# Compute MER of k_fold Cross Validation, with different k values
k_neig = 1:50
k_fold = 10
MER = matrix(ncol=0,nrow=length(k_neig))

for (type in type_name) {
  index = which(osm_type[1:N_train]==type)
  MER_type = knn_k_choice(d[index, index], k_neig=k_neig, 
                          k_fold=k_fold, true_class_train=osm_surf[index])
  MER = cbind(MER, MER_type)
}
colnames(MER) = type_name
rm(MER_type, type, index)

head(MER)
for (i in 1:length(type_name)){
  x11()
  plot(k_neig, MER[,i], type="l", xlab="k values")
  points(k_neig, MER[,i], pch=20)
  grid()
  title("MER for different k values using street type and pixel colours")
  mtext(paste0("street type: ", colnames(MER)[i]), line=0)
}
graphics.off()



# K-NN with ks chosen: evaluation of the model performance on the test set
ks_neig_chosen = c(7, 5, 5, 5, 7, 5)
names(ks_neig_chosen) = type_name

pred_class = character(N_test)
prob = numeric(N_test)
MER_test = 0

for (i in 1:length(type_name)){
  type = type_name[i]
  index_train = which(osm_type[1:N_train]==type)
  index_test = which(osm_type[-c(1:N_train)]==type)+N_train
  
  knn_result = knn_distanza(d[index_test, index_train],
                            k_neig=ks_neig_chosen[i],
                            train_class_labels=osm_surf[index_train],
                            test_class_labels=osm_surf[index_test])
  pred_class[index_test-N_train] = knn_result$pred_class
  prob[index_test-N_train] = knn_result$frequency
  
  MER_test = MER_test + knn_result$MER*length(index_test) #0.05867
  
}
MER_test = MER_test/N_test
print(MER_test)

rm(knn_result, index_test, index_train, type)

# Confusion table:
cm = table(true=osm_surf[-c(1:N_train)], predicted=pred_class)
cm
MER_test


# Threshold choice: different threshold for different street type
i =  1 #  let it vary in 1:length(type_name)

  # Counting errors and doubts:
type = type_name[i]; print(type)
index_test = which(osm_type[-c(1:N_train)]==type)+N_train
kk = ks_neig_chosen[i]
  
  # With single threshold f_{up}, no uncertainty allowed:
error_pu = error_up = NULL
doubt = rep(0, kk+1)

p_range = 0:kk/kk
for (p in p_range)
{
  n_pp = length(which(osm_surf[index_test]=="paved"    & prob[index_test-N_train]>=p))
  n_uu = length(which(osm_surf[index_test]=="unpaved"  & prob[index_test-N_train]<p))
  n_pu = length(which(osm_surf[index_test]=="paved"    & prob[index_test-N_train]<p))
  n_up = length(which(osm_surf[index_test]=="unpaved"  & prob[index_test-N_train]>=p))
  
  error_pu = c(error_pu, n_pu)
  error_up = c(error_up, n_up)
}

names(error_pu) = names(error_up) = names(doubt) = paste("i",round(p_range, digits=2),sep="=")

lista_single = list(error_pu=error_pu, error_up=error_up, doubt=doubt)


  # With pair of thresholds [f_u, f_p]:
error_pu = error_up = doubt = matrix(NA, kk+1, kk+1)

for (i in (0:kk/kk)) {
  for (j in ((kk*i):kk/kk)){
    print(paste0(i," ",j))
    class_pred = rep("uncertain", length(index_test))
    which_p = which(prob[index_test-N_train]>j); which_u = which(prob[index_test-N_train]<i)
    class_pred[which_p] = "paved"; class_pred[which_u] = "unpaved"
    
    doubt[kk*i+1, kk*j+1] = length(which(class_pred=="uncertain"))
    error_pu[kk*i+1, kk*j+1] =  length(which(osm_surf[index_test]=="paved" & class_pred=="unpaved"))
    error_up[kk*i+1, kk*j+1] =  length(which(osm_surf[index_test]=="unpaved" & class_pred=="paved"))
  }
}

error_pu[1,kk+1] = error_up[1,kk+1] = 0

rownames(doubt) = rownames(error_up) = rownames(error_pu) = paste("i",round(0:kk/kk, digits=2),sep="=")
colnames(doubt) = colnames(error_up) = colnames(error_pu) = paste("j",round(0:kk/kk, digits=2),sep="=")

lista_pair = list(error_pu=error_pu, error_up=error_up, doubt=doubt)


  # Cost computation:
unit_cost = c(c_pu=2.5, c_up=2, c_d=1)
# unit_cost = c(c_pu=10, c_up=5, c_d=5)
  
cost_single = unit_cost[1]*lista_single$error_pu + unit_cost[2]*lista_single$error_up +
  unit_cost[3]*lista_single$doubt
print(cost_single)

cost_pair = unit_cost[1]*lista_pair$error_pu + unit_cost[2]*lista_pair$error_up + 
  unit_cost[3]*lista_pair$doubt
print(cost_pair)
  
  
  # Plot cost:
library("plot.matrix")
cost_plot = cbind("single"=rep(NA,kk+1), rep(NA,kk+1), rep(NA,kk+1), cost_pair)
diag(cost_plot) = c(cost_single)
{
  col = colorRampPalette(c("snow2", "red3"), 10)
  x11(width=8, height=8)
  plot(cost_plot, main="", cex=1, digits=0, axis.col=list(side=3),
       na.print=F, col=col, xlab="", ylab="", key=NULL)
  mtext("total costs", line=2.5, cex=1.3, font=2)
  rm(col)
}

# Results are summarized in the paper



## UNIFY TYPE CLASSES ---------------------------------------------------------
# More generic model:
# Merge streets of (intuitively) similar street type.
# primary + secondary; tertiary+unclassified; residential+footways
# GOAL: verify that (in our application) street type can be summarized with no
# significant changes in the model

rm (list=ls())
source('scripts/utils/funzione_knn_k_choice.R')

# Training and test sets:
load("preprocessed-data/val_n150_known.RData"); rm(val)

N = length(img_name)
N_train = round(N*70/100)
N_test = N-N_train

set.seed(123) #123
perm = sample(N, replace=F)
img_name = img_name[perm]
osm_surf = osm_surf[perm]
osm_type = osm_type[perm]

# Unify street labels (intuitively): 
osm_type2 = osm_type
osm_type2[which(osm_type2=="primary" | osm_type2=="secondary")] = "1"
osm_type2[which(osm_type2=="tertiary" | osm_type2=="unclassified")] = "2"
osm_type2[which(osm_type2=="residential" | osm_type2=="footway")] = "3"
type_name = unique(osm_type2)


# Loading the (already computed) Energy distance between distributions:
load("preprocessed-data/distances_n150_RGB.RData")
d = lista$e_dist[perm, perm] ; rm(lista)


# Choice of k: choose k that minimizes MER
# Compute MER of k_fold Cross Validation, with different k values
k_neig = 1:50
k_fold = 10
MER = rep(0,length(k_neig))

for (type in type_name) {
  index = which(osm_type2[1:N_train]==type)
  MER_type = knn_k_choice(d[index, index], k_neig=k_neig, 
                          k_fold=k_fold, true_class_train=osm_surf[index])
  MER = MER + MER_type*length(index)
}
MER = MER/N_test
rm(MER_type, type, index)

{
  x11()
  plot(k_neig, MER, type="l", xlab="k values")
  points(k_neig, MER, pch=20)
  grid()
  title("MER for different k values using street type and pixel colours")
}
dev.off()

# Choice: k in {5,...,10} neighbours --> k=7
k_neig_chosen = 7


# K-NN with k=7: evaluation of the model performance on the test set
pred_class = character(N_test)
prob = numeric(N_test)
MER_test = 0

for (type in type_name){
  index_train = which(osm_type2[1:N_train]==type)
  index_test = which(osm_type2[-c(1:N_train)]==type)+N_train
  
  knn_result = knn_distanza(d[index_test, index_train],
                            k_neig=k_neig_chosen,
                            train_class_labels=osm_surf[index_train],
                            test_class_labels=osm_surf[index_test])
  pred_class[index_test-N_train] = knn_result$pred_class
  prob[index_test-N_train] = knn_result$frequency
  
  MER_test = MER_test + knn_result$MER*length(index_test) #0.05867
  
}
MER_test = MER_test/N_test
print(MER_test)

rm(knn_result, index_test, index_train, type)

# ROC curve:
{
  source('scripts/utils/funzione_knn_compute_MER.R')
  sub = paste0("kNN with k=",k_neig_chosen,", RGB space pixels, e_dist and street type")
  lista = knn_ROC_curve(k_neig_chosen, osm_surf[-c(1:N_train)], pred_prob=prob)
  knn_ROC_curve_plot(lista, k_neig_chosen, subtitle=sub)
  
  MER_sharp = rev(lista$MER[-1])
  TPR_sharp = rev(lista$TPR[-1]);FPR_sharp = rev(lista$FPR[-1])
  rm(lista, sub)
  # 0.5 is a reasonable strict threshold
}
dev.off()

# Confusion table:
cm = table(true=osm_surf[-c(1:N_train)], predicted=pred_class)
cm
MER_test
