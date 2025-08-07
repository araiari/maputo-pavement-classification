######################## KNN function ##################################

# It takes as input a distance matrix between the test points (rows) and the points
# of training (columns): d is an N_test*N_train matrix
# 
# For each test item, the k_nig closest points of the training are found.
# 
# The most frequent class is assigned to it (in the standard version, alpha=0.5).
# A classification threshold can be chosen via the alpha parameter.
#
# The function also works for multiple classes: just input the class factors
# and the alpha threshold vector.
# 
# The train_class_labels vector must have been passed in order to predict the class.
# If you want the MER to be calculated, you can also pass the test_class_labels.


knn_distanza = function(d, k_neig, alpha=NULL, train_class_labels, test_class_labels=NULL,
                        class_cat=NULL ){
  
  d = as.matrix(d)
  
      # Initialize the indices
  N_train = length(train_class_labels)
  N_test = dim(d)[1]
  N = N_train+N_test
  
      # Check dimensionality errors
  if (N_train != dim(d)[2]){
    print("Error: distance matrix dimension do not correspond!")
    return (1)
  }
  
      # if not indicated, initialize class_category and alpha threshold vector
  if (is.null(class_cat)) class_cat = levels(as.factor(train_class_labels))
  n_class = length(class_cat)
  if (is.null(alpha))     alpha = rep(1/n_class, n_class)
  
      # Initialize the aforementioned counters, frequencies and classes
  f_counter = numeric(n_class)
  
  freq = matrix(0, ncol=n_class, nrow=N_test)
  pred_class_labels = character(N_test)
  
      # for loop on all test set items
  for (v in 1:N_test){
    
    which_neig = order(d[v,])[1:k_neig]   # 
    
    for (fact in 1:n_class)
      f_counter[fact] = sum(train_class_labels[which_neig] == class_cat[fact])
    
    freq[v,] = f_counter/k_neig
    
    exit = found = FALSE
    while (!exit & !found) {
      max_i = which.max(f_counter)
      if (freq[v, max_i] >= alpha[max_i]){
        pred_class_labels[v] = class_cat[max_i]
        found = TRUE
      }    
      else {
        f_counter[max_i] = 0
        exit = (sum(f_counter)==0)
      }
    }
    #if (!found) # se non trova quello che soddisfa, allora predice quello massimo
      #pred_class_labels[v] = class_cat[which.max(freq[v,])]
  }
    
  if (n_class==2)  freq = as.numeric(freq[,1])
  
  if(!is.null(test_class_labels) & N_test==length(test_class_labels))
    MER = sum((pred_class_labels != test_class_labels) )/N_test #& (pred_class_labels != "")
  
  else
    MER = NULL
  
  knn_result = list(pred_class=pred_class_labels, frequency=freq, MER=MER)
  return(knn_result)

}