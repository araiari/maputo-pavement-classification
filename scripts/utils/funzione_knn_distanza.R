######################## KNN function ##################################

# Prende in ingresso una matrice distanza tra i punti del test (righe) e i punti
# del training (colonne): d è una matrice N_test*N_train
# 
# Per ogni elemento del test, vengono trovati i k_neig punti più vicini del training.
# 
# Viene assegnata ad esso la classe più frequente (nella versione standard, alfa=0.5).
# Può essere scelta una soglia di classificazione tramite il parametro alpha.
#
# La funzione funziona anche per classi multiple: basta dare in input i fattori della classe
# e il vettore di soglie alfa.
# 
# Deve essere passato il vettore train_class_labels per poter predire la classe.
# Se si vuole che venga calcolato il MER, si può passare anche la test_class_labels.

knn_distanza = function(d, k_neig, alpha=NULL, train_class_labels, test_class_labels=NULL,
                        class_cat=NULL ){
  
  d = as.matrix(d)
  
      # inizializzo indici massimi
  N_train = length(train_class_labels)
  N_test = dim(d)[1]
  N = N_train+N_test
  
      # controllo errore dimensione
  if (N_train != dim(d)[2]){
    print("Error: distance matrix dimension do not correspond!")
    return (1)
  }
  
      # se non indicati, inizializzo class_cathegory e vettore delle soglie alpha
  if (is.null(class_cat)) class_cat = levels(as.factor(train_class_labels))
  n_class = length(class_cat)
  if (is.null(alpha))     alpha = rep(1/n_class, n_class)
  
      # inizializzo contatori, frequenze e classi predette
  f_counter = numeric(n_class)
  
  freq = matrix(0, ncol=n_class, nrow=N_test)
  pred_class_labels = character(N_test)
  
      # ciclo for su tutti gli elementi del test set
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