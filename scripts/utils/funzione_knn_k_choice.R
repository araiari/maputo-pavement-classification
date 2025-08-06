# La funzione calcola il MER del knn con soglia 0.5 utilizzando diversi valori di k.
# Tali valori sono specificati nel parametro k_neig.
# Il MER viene calcolato come media dei MER valutati per diversi test sets con 
# k_fold cross validation (il numero di folds � nel parametro k_fold)
# la funzione prende in ingresso la matrice distanza tra tutti i punti (d)

# Nel caso in cui le classi siano pi� di una, la funzione da utilizzare non � 
# knn_distanza ma � knn_distanza_multiclass.
# Assegna la classe a frequenza massima.

source('scripts/utils/funzione_knn_distanza.R')

knn_k_choice = function(dd, k_neig=1:50, alpha=NULL, k_fold=10, plot=F, true_class_train) 
{
  class_cat = levels(as.factor(true_class_train))
  start_time = Sys.time()
  
  # inizializzo gli indici dei validation sets:
  n_train = dim(dd)[1]
    n_valid = rep(floor(n_train/k_fold), k_fold)
  if (n_train%%k_fold!=0)
    n_valid[1:(n_train%%k_fold)] = n_valid[1:(n_train%%k_fold)] + 1;
  n_valid_index = cumsum(c(1, n_valid))
  
  # calcolo il MER con un ciclo for per ogni valore in k_neig:
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
  windows(bg="#FAFAFA")
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




