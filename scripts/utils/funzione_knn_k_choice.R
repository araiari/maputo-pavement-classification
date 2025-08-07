# The function calculates the MER of the knn with threshold 0.5 using different k values.
# These values are specified in the k_neig parameter.
# The MER is calculated as the average of the MERs evaluated for different test sets with 
# k_fold cross validation (the number of folds is in the k_fold parameter)
# the function takes as input the distance matrix between all points (d)

# If there are more classes than one, the function to use
# knn_distance but knn_distance_multiclass.
# Assign class to maximum frequency.

source('scripts/utils/funzione_knn_distanza.R')

knn_k_choice = function(dd, k_neig=1:50, alpha=NULL, k_fold=10, plot=F, true_class_train) 
{
  class_cat = levels(as.factor(true_class_train))
  start_time = Sys.time()
  
  # I initialize the indexes of the validation sets:
  n_train = dim(dd)[1]
    n_valid = rep(floor(n_train/k_fold), k_fold)
  if (n_train%%k_fold!=0)
    n_valid[1:(n_train%%k_fold)] = n_valid[1:(n_train%%k_fold)] + 1;
  n_valid_index = cumsum(c(1, n_valid))
  
  # I calculate the MER with a for loop for each value in k_neig:
  MER = rep(0, length(k_neig))
  
  for (j in 1:length(k_neig)) {
    
    for (i in 1:k_fold){
      
      valid = n_valid_index[i]:(n_valid_index[i+1]-1)
      #if (i == k_fold) valid = n_valid_index[i]:n_train
      
      knn_result = knn_distanza(dd[valid,-valid], k_neig=k_neig[j], alpha=alpha,
                                train_class_labels=true_class_train[-valid],
                                test_class_labels=true_class_train[valid],
                                class_cat=class_cat)
      
      MER[j] = MER[j] + knn_result$MER
    }
    
    MER[j] = MER[j]/k_fold
  }
  
  end_time = Sys.time()
  print(end_time-start_time)
  
  rm(dd, j, i, valid, knn_result, n_valid, n_valid_index, n_train, end_time, start_time)
  
  if (plot) 
    knn_k_choice_plot(k_neig, MER)
  
  return (MER)
}


knn_k_choice_plot = function(k_neig, MER, title="MER for different k values",
                             k_chosen=NULL, MER_test=NULL) 
{
  x11(bg="#FAFAFA")
  plot(k_neig, MER, type="l", xlab="k values", ylim=c(min(c(MER_test,MER)), max(c(MER_test,MER))))
  points(k_neig, MER, pch=20)
  grid()
  title(title)
  
  if (!is.null(k_chosen)) {
    abline(v=k_chosen, lty=3, col="red")
    if (!is.null(MER_test))
      points(k_chosen, MER_test, pch=20, col="red")
  }
}




