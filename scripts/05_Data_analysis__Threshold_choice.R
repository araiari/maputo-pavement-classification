
# DATA ANALYSIS: THRESHOLD CHOICE ----------------------------------------------


rm(list=ls())
graphics.off()
source("scripts/04_Data_analysis__k-NN_model.R")


## COMPUTATION OF ERRORS AND UNCERTAINTY ---------------------------------------

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


## COST COMPUTATION -----------------------------------------------------------

unit_cost = c(c_pu=2.5, c_up=2, c_d=1)

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
dev.off()


## CONCLUSION: ----------------------------------------------------------------
# Optimal threshold to minimize the total cost: f_u = f_p = 0.4

f_u = f_p = 0.4
class_pred = rep("uncertain", N_test)
which_p = which(prob>f_p); which_u = which(prob<f_u)
class_pred[which_p] = "paved"; class_pred[which_u] = "unpaved"


t = table(true = osm_surf[-c(1:N_train)], predicted = class_pred); t


# Accuracy:
(t[1,1]+t[2,3])/N_test

# MER:
(t[2,1]+t[1,3])/N_test

# Uncertainty:
sum(t[,2])/N_test

