# PCA of pixel values
# I take components 2 and 3 (i.e. those on the distribution around the diagonal)
# and I evaluate the radius of the cylinder where the points are distributed 
# as the quantile 65%, 75%, 95% (default, or you can indicate whatever you want) 

quantiles_PCA = function(val, quantiles=c(0.75,0.95), bagplot=F, ari_plot=F) {
  
  PC = princomp(val, scores=T)
  
  score.PC <- PC$scores
  
  dist0 = sqrt(score.PC[,2]^2+score.PC[,3]^2)
  
  if (ari_plot==T){ 
    # homemade bagplot (quantiles: 0.75,0.95)
    
    far = score.PC[which(dist0>quantile(dist0, quantiles[2])),2:3]
    half_far = score.PC[which(dist0>quantile(dist0, quantiles[1])),2:3]
    
    x11()
    plot(score.PC[,2], score.PC[,3], pch=20, xlim=range(score.PC[,2]), ylim=range(score.PC[,3]), 
         col="gray15", xlab="Princ Comp 2", ylab="Princ Comp 3")
    points(half_far, col="gray45", pch=20)
    points(far, col="gray60", pch=20)
    grid()
    
  }
  
  if (bagplot==T) {
    x11(); par(mfrow=c(1,2))
    aplpack::bagplot(score.PC[,2:3], show.whiskers=F, show.loophull=F)
    
    par(col.axis="white", col.lab="white", tck=0);
    plotRGB(img, main=img_name); box(col="white")
    
  }
  
  qu = NULL
  for (quant in quantiles)
    qu = cbind(qu, quantile(dist0, quant))
  
  return(qu)
  
}
