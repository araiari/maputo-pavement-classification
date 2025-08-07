# Here are all the functions to calculate the MER
# The ROC_curve functions calculate the MER for narrow threshold varying from 0 to 1
# and they plot it.
# The MER_uncertain_interval function allows a frequency range [i,j] in which the
# prediction is doubtful. Returns the MER and % uncertainty (both matrices)


# ROC curve --------------------------------------------------------------------

knn_ROC_curve = function (k_neig_chosen, true_class_test, pred_prob, 
                          plot=F, subtitle=NULL, class_cat=c("paved", "unpaved"))
{
  n_test = length(true_class_test)
  
  MER = round(length(which(true_class_test==class_cat[1]))/n_test,digits=3)
  TPR = FPR = 0
  
  p_range = k_neig_chosen:0 /k_neig_chosen
  for (p in p_range)
  {
    print(p)
    
    class_pred_ROC = character(n_test)
    class_pred_ROC[pred_prob>=p] = class_cat[1]; class_pred_ROC[pred_prob<p] = class_cat[2]
    
    true_class_test2 = true_class_test #if multiple classes, we must fix...
    true_class_test2[which(true_class_test2!=class_cat[1])] = class_cat[2]
    
    true_class_test2 = as.factor(true_class_test2); levels(true_class_test2) = class_cat
    class_pred_ROC = as.factor(class_pred_ROC); levels(class_pred_ROC) = class_cat
    
    cm_ROC = table(true=true_class_test2, predicted=class_pred_ROC)
    MER = c(MER, round(1-sum(diag(cm_ROC))/sum(cm_ROC), digits=3))

    print(cm_ROC)
    print(paste('MER=', round(1-sum(diag(cm_ROC))/sum(cm_ROC), digits=3)))
    
    TPR = c(TPR, cm_ROC[1,1]/sum(cm_ROC[1,])) # TP/(TP+FN)=TP/(TP+paved that are misclassified as unpaved)
    FPR = c(FPR, cm_ROC[2,1]/sum(cm_ROC[2,])) # FP/(FP+TN)
  }
  rm(class_pred_ROC, cm_ROC, p, p_range)
  
  lista = list(TPR=TPR, FPR=FPR, MER=MER)
  if (plot==T)
    knn_ROC_curve_plot(lista, k_neig_chosen, subtitle)
  
  return(lista)
}


# ROC curve plot ---------------------------------------------------------------

knn_ROC_curve_plot = function(lista, k_neig_chosen, subtitle=NULL)
{
  if (names(lista)[1] != "TPR"| names(lista)[2] != "FPR" | names(lista)[3] != "MER"){
    print("ERROR: list does not contain the needed information: missing TPR, FPR or MER")
    return()
  }
  
  TPR = lista$TPR
  FPR = lista$FPR
  MER = lista$MER
  
  p_range = k_neig_chosen:0 /k_neig_chosen
  
  x11(bg="white")
  plot(FPR, TPR, type="l", xlim=c(-0.01,1.01), ylim=c(-0.01,1.01), col="olivedrab", lwd=1)
  points(FPR, TPR, pch=16, col="olivedrab")
  text(FPR, TPR+0.03, labels=c("", as.character(round(p_range, digits=2))),col="olivedrab")
  text(FPR, TPR-0.01, labels=paste('MER=', MER, sep=''), col=1, cex=0.6)
  grid()
  
  abline(a=0, b=1, lty=3, col=1)
  abline(h=0); abline(v=0); abline(h=1, lty=3)
  
  #title("ROC with a=positive", line=2)
  if (!is.null(subtitle))
    mtext(subtitle, line=0.7)
  
  rm(p_range)
}


# MER uncertain interval -------------------------------------------------------

knn_MER_uncertain_interval = function(k, true_test_class, pred_prob) 
{
  n_test = length(true_test_class)
  n = k+1
  MER = n_dubbie = TPR = FPR = n_ua = n_au = matrix(NA, n, n)
  true_test_class = as.factor(true_test_class)
  
  for (i in (0:k/k)) {
    for (j in ((k*i):k/k)){
      
      print(paste0("i=",i,"; j=",j))
      
      class_pred = rep("d", n_test)
      which_a = which(pred_prob>j); which_u = which(pred_prob<i)
      class_pred[which_a] = "a"; class_pred[which_u] = "u"
      
      cm = table(true=true_test_class, predicted=class_pred)
      print(cm)
      
      n_dubbie[k*i+1, k*j+1] = length(which(class_pred=="d"))
      n_au[k*i+1, k*j+1] = length(which(class_pred=="u" & true_test_class=="a"))
      n_ua[k*i+1, k*j+1] = length(which(class_pred=="a" & true_test_class=="u"))
      
      n_aa = length(which(class_pred=="a" & true_test_class=="a"))
      n_uu = length(which(class_pred=="u" & true_test_class=="u"))
      
      MER[k*i+1, k*j+1] =  (n_au[k*i+1, k*j+1]+n_ua[k*i+1, k*j+1])/n_test
      TPR[k*i+1, k*j+1] = n_aa/(n_aa+n_au[k*i+1, k*j+1]) # TP/(TP+FN)=TP/(TP+paved that are misclassified as unpaved)
      FPR[k*i+1, k*j+1] = 1 - n_uu/(n_uu+n_ua[k*i+1, k*j+1]) # FP/(FP+TN)
      
    }
  }
  rm(i, j, cm, class_pred, which_a, which_u)
  
  MER[1,n] = 0
  
  rownames(n_dubbie) = paste("i",round(0:k/k, digits=2),sep="=")
  colnames(n_dubbie) = paste("j",round(0:k/k, digits=2),sep="=")
  #print(round(n_dubbie, digits=3))
  
  rownames(MER) = paste("i",round(0:k/k, digits=2),sep="=")
  colnames(MER) = paste("j",round(0:k/k, digits=2),sep="=")
  #print(round(MER, digits=3))
  
  rownames(TPR) = paste("i",round(0:k/k, digits=2),sep="=")
  colnames(TPR) = paste("j",round(0:k/k, digits=2),sep="=")
  #print(round(TPR, digits=3))
  
  rownames(FPR) = paste("i",round(0:k/k, digits=2),sep="=")
  colnames(FPR) = paste("j",round(0:k/k, digits=2),sep="=")
  #print(round(FPR, digits=3))
  
  rownames(n_au) = paste("i",round(0:k/k, digits=2),sep="=")
  colnames(n_au) = paste("j",round(0:k/k, digits=2),sep="=")
  
  rownames(n_ua) = paste("i",round(0:k/k, digits=2),sep="=")
  colnames(n_ua) = paste("j",round(0:k/k, digits=2),sep="=")
  
  lista = list(MER=MER, n_dubbie=n_dubbie, TPR=TPR, FPR=FPR, n_ua=n_ua, n_au=n_au)
  return(lista)
}

