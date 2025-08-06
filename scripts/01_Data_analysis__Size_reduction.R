
# DATA ANALYSIS: SIZE REDUCTION ---------------------------------------------

# The computation of distances between distribution is highly time consuming
# By reducing the number of pixels of each distribution, the computational
# effort might be reduced


library(rgl); library(energy)
rm(list=ls())

# How many pixels per image:
load("preprocessed-data/val_tot_known.RData")
img_name = unique(val_tot$img_name)
N = length(img_name)
DIM = integer(N)

for (i in 1:N){
  i_ok = which(img_name[i]==val_tot$img_name)
  DIM[i] = length(i_ok)
}

s = summary(DIM); s
{
  x11()
  hist(DIM, xlab='', main="Dimension of street pixel clouds", col="gray95")
  abline(v=s[4], col="red4"); abline(v=s[3], col="blue3")
  text(x=s[4], y=730, "mean", col="red4");text(x=s[3], y=730, "q0.5", col="blue4");
  abline(v=s[c(2,5)], col="blue4", lty=3)
  text(x=s[2], y=730, "q0.25", col="blue4");text(x=s[5], y=730, "q0.75", col="blue4");
}
graphics.off()
rm(s, i_ok, i)


## CLASSIFICATION TIME OF NEW OBSERVATIONS -------------------------------------
# Suppose a new dummy observation with n=mean=2000 pixels.
# Compute the time to evaluate the euclidean distance between its points and points
# of other distributions (and energy distances) in the training set

n = 2000
N = 2558
data_fake = val_tot[1:n,1:3]
n_trials = 10

cumsizes = c(1, cumsum(DIM)) 
diff_times = NULL

for (j in 1:n_trials){
  print(paste0("Trial n. ", j))
  e_dist = numeric(n)
  
  start = Sys.time()
  for (i in 1:N){
    nube1 = val_tot[cumsizes[i]:(cumsizes[i+1]-1),1:3]
    
    d_temp = as.matrix(dist(rbind(nube1, data_fake), method="euclidean"))
    
    e_temp = energy::edist(d_temp, sizes=c(dim(nube1)[1], dim(data_fake)[1]), distance=T)
    e_dist[i] = as.matrix(e_temp)[1,2]
    print(i)
  }
  end = Sys.time()
  diff_times = c(diff_times, difftime(end, start, units="mins"))
}
# Time difference of (37.72944+38.74699+36.97791)/3 mins when n=2000 -> t=34.48011 mins
diff_times
mean(diff_times)   


# In case the dimension of each cloud is reduced to n=200, 150, 100, 50:
img_name = unique(val_tot$img_name)
n_all = c(200,150,100,50)
N = 2558

for (n in n_all){
  val_red = val_tot[1,]
  set.seed(1234)
  for (i in 1:N){
    val_red = rbind(val_red, val_tot[sample(which(val_tot$img_name == img_name[i]), n, replace=T),])
  }
  val_red = val_red[-1,]

  data_fake = val_red[1:n,1:3]
  n_trials = 3
  
  cumsizes = c(1, rep(n, N)) 
  diff_times = NULL
  
  for (j in 1:n_trials){
    print(paste0("Trial n. ", j))
    e_dist = numeric(n)
    
    start = Sys.time()
    for (i in 1:N){
      nube1 = val_red[cumsizes[i]:(cumsizes[i+1]-1),1:3]
      
      d_temp = as.matrix(dist(rbind(nube1, data_fake), method="euclidean"))
      
      e_temp = energy::edist(d_temp, sizes=c(dim(nube1)[1], dim(data_fake)[1]), distance=T)
      e_dist[i] = as.matrix(e_temp)[1,2]
    }
    end = Sys.time()
    diff_times = c(diff_times, difftime(end, start, units="secs"))
  
  }
  print(diff_times)
  print(mean(diff_times) )
  # Time difference of (1.093422+1.045325+1.040263)/3 secs when n=50 -> t=1.05967 secs
  # Time difference of ( 1.385295+1.387264+1.357369)/3 secs when n=100 -> t=1.376643 secs
  # Time difference of (2.379848+1.960755+1.913413)/3 secs when n=150 -> t=2.084672 secs
  # Time difference of (2.871018+2.876249+2.812477)/3 secs when n=200 -> t=2.853248 secs
}

# Acceptable times for n <= 200

rm(data_fake, end, d_temp, start, diff_times, e_dist, i, j, e_temp, nube1, val_red, n, cumsizes, n_trials)



## VISUALIZE PIXEL DISTRIBUTIONS -----------------------------------------------
# Pixel distributions in the RGB space, and approximated pixel distributions 
# in the RGB space, when n=50, 100, 150, 200

img_name_red = c("img__11.tif", 'img__7.tif', 'img__43095.tif',
              'img__961.tif', 'img__2662.tif', 'img__239.tif')
N = length(img_name_red)

val_red = val_tot[1,]
DIM_red = integer(N)
for (i in 1:N){
  val_red = rbind(val_red, val_tot[which(val_tot$img_name==img_name_red[i]),])
  DIM_red[i] = DIM[which(img_name==img_name_red[i])]
}
val_red = val_red[-1,]

cumsizes = cumsum(c(1, DIM_red))
range = c(20,255)
colors = c("navy", "#4682B4","darkgreen",  "orange", "red3",  "olivedrab1")

{
  open3d()
  plot3d(val_red[cumsizes[1]:(cumsizes[2]-1),1:3], 
         col=colors[1], size=1.5, xlim=range, ylim=range, zlim=range)
  for (i in 2:N)
    points3d(val_red[cumsizes[i]:(cumsizes[i+1]-1),1:3], col=colors[i], size=1.5)
  # legend3d("right", legend = img_name_red, pch = 16, col = colors, cex=1, inset=c(0.02))
}

for (n in n_all) {
  cumsizes = cumsum(c(1, DIM_red))
  open3d()
  val_red2 = val_red[sample(cumsizes[1]:(cumsizes[2]-1), n, replace=T),1:3]
  plot3d(val_red2, col=colors[1], size=3, xlim=range, ylim=range, zlim=range)
  for (i in 2:N) {
    val_red2 = val_red[sample(cumsizes[i]:(cumsizes[i+1]-1), n, replace=T),1:3]
    points3d(val_red2, col=colors[i], size=3)
  }
  
  x = readline("Press any for the next n:")
  # legend3d("right", legend = img_name_red, pch = 16, col = colors, cex=1, inset=c(0.02))
}

close3d(); close3d(); close3d(); close3d()
rm(range,i, cumsizes, colors, val_red2, x, n)
rm(val_red, img_name_red, DIM_red)

# With n >= 100, distributions seem to be well approximated



## DISTANCES AND APPROXIMATIONS ------------------------------------------------
 
### COMPARISON OF TUKEY MEDIANS ------------------------------------------------
# Compute the Tukey median of distributions with original and reduced pixel size
# Evaluate their L2 distance in the RGB space

library(DepthProc)
L2_dist = function(x,y) {sqrt(sum((x-y)^2))}

# Tukey median with the original pixel size:
N = length(img_name)
val_med = NULL
for (name in img_name) {
  index = which(val_tot$img_name == name)
  med = depthMedian (val_tot[index,1:3], depth_params=list(method="Tukey"))
  val_med = rbind(val_med, cbind(rbind(med), name))
  print(name)
}
save(val_med, file="preprocessed-data/val_med_known.RData")
rm(val_med, index, name, med)


# Tukey median with reduced pixel size (n):
n_all = c(200,150,100)
for (n in n_all) {
  val_med_red = NULL
  set.seed(1234)
  for (name in img_name) {
    index = which(val_tot$img_name == name)
    index = sample(index, n)
    med = depthMedian (val_tot[index,1:3], depth_params=list(method="Tukey"))
    val_med_red = rbind(val_med_red, cbind(rbind(med), name))
    print(name)
  }
  save(val_med_red, file=paste0("preprocessed-data/val_med_red_n",n,".RData"))
}
rm(n, val_med_red, index, med, name)


# L2 distances between Tukey medians:
load("preprocessed-data/val_med_known.RData")
for (n in n_all){
  load(paste0("preprocessed-data/val_med_red_n",n,".RData"))
  dist = numeric(N)
  for (i in 1:N)
    dist[i] = L2_dist(val_med[i,1:3], val_med_red[i,1:3])
  
  s = summary(dist); print(s)
  x11()
  hist(dist, xlab='', col="gray95")
  title("Distance between true and approximated Tukey medians")
  subtitle(paste0("n = ", n ))
  abline(v=s[4], col="red4"); abline(v=s[3], col="blue3") ##### CAMBIA y=730
  text(x=s[4], y=730, "mean", col="red4");text(x=s[3], y=730, "q0.5", col="blue4");
  abline(v=s[c(2,5)], col="blue4", lty=3)
  text(x=s[2], y=730, "q0.25", col="blue4");text(x=s[5], y=730, "q0.75", col="blue4");
  
}

graphics.off()
rm(s, val_med_red, val_med, dist, n, i)



### COMPARISON OF DISTANCES ----------------------------------------------------
# Select N = 100 random images/distributions and compute the distances
# both with all the pixels and with a reduced number of pixels: n=100, 150, 200

N = 100
set.seed(234); img_name_red = sample(img_name, size=N)
DIM_red = integer(N)
val_red = val_tot[1,]
for (i in 1:N){
  val_red = rbind(val_red, val_tot[which(val_tot$img_name==img_name_red[i]),])
  DIM_red[i] = DIM[which(img_name==img_name_red[i])]
}
val_red = val_red[-1,]

# Computing distances with the original pixel size:
cumsizes = cumsum(c(1,DIM_red))

h_dist = w_dist = e_dist = matrix(0, ncol=N, nrow=N)
haus_dist2 = function(dist) { return( max (max(apply(dist,1,min)),max(apply(dist,2,min))) ) }

{
  for (i in 1:N){
    for (j in i:N){
      print(paste0("i=",i, "; j=",j))
      nube1 = val_red[cumsizes[i]:(cumsizes[i+1]-1),1:3]
      nube2 = val_red[cumsizes[j]:(cumsizes[j+1]-1),1:3]
      
      d_temp = as.matrix(dist(rbind(nube1, nube2), method="euclidean"))
      
      e_temp = energy::edist(d_temp, sizes=DIM_red[c(i,j)], distance=T)
      e_dist[i,j] = e_dist[j,i] = as.matrix(e_temp)[1,2]
      
      d_temp = d_temp[1:DIM_red[i], (DIM_red[i]+1):(DIM_red[i]+DIM_red[j])]
      h_dist[i,j] = h_dist[j,i] = haus_dist2(d_temp)
      
      perm = sample(size=min(DIM_red[c(i,j)]),
                    1:max(DIM_red[c(i,j)]))
      if (DIM_red[i] < DIM_red[j])
        nube2 = nube2[perm,]
      else
        nube1 = nube1[perm,]
      w_dist[i,j] = w_dist[j,i] = transport::wasserstein(transport::pp(nube1),
                                                         transport::pp(nube2))
      
    }
  } 
  rm (nube1,nube2,i,j,d_temp,perm)
  
  lista = list(h_dist=h_dist, 
               e_dist=e_dist,
               w_dist=w_dist)
  save(lista, file='preprocessed-data/distances_N100_true.RData')
}


# Computing distances with the reduced pixel size (n):
n_all = c(200,150,100)
for (n in n_all) {
  
  val_red2 = val_red[1,]
  set.seed(1234)
  for (i in 1:N){
    val_red2 = rbind(val_red2, val_red[sample(which(val_red$img_name==img_name_red[i]), n, replace=T),])
  }
  val_red = val_red[-1,]
  
  cumsizes = cumsum(c(1,rep(n, N)))

  h_dist = w_dist = e_dist = matrix(0, ncol=N, nrow=N)

  for (i in 1:N){
    for (j in i:N){
      print(paste0("n=", n, " - iteration i=", i, ", j=",j))
      nube1 = val_red2[cumsizes[i]:(cumsizes[i+1]-1),1:3]
      nube2 = val_red2[cumsizes[j]:(cumsizes[j+1]-1),1:3]
      
      d_temp = as.matrix(dist(rbind(nube1, nube2), method="euclidean"))
      
      e_temp = energy::edist(d_temp, sizes=c(n,n), distance=T)
      e_dist[i,j] = e_dist[j,i] = as.matrix(e_temp)[1,2]
      
      d_temp = d_temp[1:n, (n+1):(2*n)]
      h_dist[i,j] = h_dist[j,i] = haus_dist2(d_temp)
      
      w_dist[i,j] = w_dist[j,i] = transport::wasserstein(transport::pp(nube1),
                                                         transport::pp(nube2))
    }
  } 
  rm (nube1,nube2,i,j,d_temp)
  
  lista = list(h_dist=h_dist, 
               e_dist=e_dist,
               w_dist=w_dist)
  save(lista, file=paste0("preprocessed-data/distances_N100_n",n,".RData"))
}


# Graphical comparison of distances:

library('plot.matrix')

col=colorRampPalette(c("blue","white","red"))(20)
for (n in n_all){
  
  x11(bg='white', width=700, height=400); par(mfrow=c(2,3))
  xl = array(runif(1e4), c(1e2, 1e2))
  brk = 20

  load("preprocessed-data/distances_N100_true.RData")
  for (i in 1:3){
    mat = as.matrix(lista[[i]])
    plot(mat, border=NA, breaks=brk, col=col, 
         key=list(side=4,  font=2, cex.axis=0.75), fmt.key="%.2f", 
         polygon.key=NULL, axis.key=NULL, spacing.key=c(3,2,2), main="")
  }
  
  load(paste0("preprocessed-data/distances_N100_n",n,".RData"))
  for (i in 1:3){
    mat = as.matrix(lista[[i]])
    plot(mat, border=NA, breaks=brk, col=col, 
         key=list(side=4,  font=2, cex.axis=0.75), fmt.key="%.2f", 
         polygon.key=NULL, axis.key=NULL, spacing.key=c(3,2,2), main="")  }
  par(mfrow=c(1,1))
  mtext(paste0("From the left: h_dist, e_dist and w_dist; Top: original size, bottom: reduced n=",n), 
        line=2)
}
graphics.off()

# The structure of the matrix is mantained


# Distance between matrices: Frobenius distance
load("preprocessed-data/distances_N100_true.RData")
h1 = as.matrix(lista$w_dist)
load(paste0("preprocessed-data/distances_N100_n200.RData"))
h200 = as.matrix(lista$w_dist)
load(paste0("preprocessed-data/distances_N100_n150.RData"))
h150 = as.matrix(lista$w_dist)
load(paste0("preprocessed-data/distances_N100_n100.RData"))
h100 = as.matrix(lista$w_dist)

d200 = SMFilter::FDist2(h1,h200)
d150 = SMFilter::FDist2(h1,h150)
d100 = SMFilter::FDist2(h1,h100)

# As expected: smaller distance when higher n



## SAMPLING --------------------------------------------------------------------
# For each image, we randomly select n=150 pixels 

load("preprocessed-data/val_tot_known.RData")

n = 150
N = length(img_name)
val2 = matrix(nrow=0, ncol=3)

set.seed(1234)
for (i in 1:N){
  index = which(val$img_name==img_name[i])
  index = sample(index, n)
  val2 = rbind(val2, val[index,])
}

val = val2
save(val, img_name, osm_surf, osm_type, 
    file="preprocessed-data/val_n150_known.RData")
