# DATA EXTRACTION AND DESCRIPTIVE DATA ANALYSIS

## VISUAL CHECK OF IMAGES ------------------------------------------------------
# Select an image and plot it

rm(list=ls())
library(terra); library(sf); library(rgl); library(hexbin); library(ggplot2)
source("scripts/utils/funzione_imageCleaner.R", echo=TRUE)

# Visualize one single image:
img1 = rast("data/raster/img__1.tif")
img1 = image_cleaner(img1)

# layer by layer:
x11(); plot(img1, col=gray(1:100/100)) 

# full picture:
x11(); plotRGB(img1)

# histogram of pixel components: red, green, blue
{
  x11(bg="white"); par(mfrow=c(1,3))
  hist(values(img1[[1]]), xlab="", main='red values', col='red')
  hist(values(img1[[2]]), xlab="", main='green values', col='green')
  hist(values(img1[[3]]), xlab="", main='blue values', col='blue')
  par(mfrow=c(1,1))
}

# 3D scatterplot and hexbin plot:
{
  open3d()
  r=c(0,255)
  plot3d(x=values(img1[[1]]), y=values(img1[[2]]), z=values(img1[[3]]), 
         xlab="redBand", ylab="greenBand", zlab="blueBand",
         xlim=r, ylim=r, zlim=r,
         type="p", size=2, col="cadetblue4")
}

# hexbin plot:
{
  x11()
  plot(hexbin(values(img1[[1]]), values(img1[[2]]), xbins=75),
       main="1. Hexagonal binning of red vs green", xlab="redBand", ylab="greenBand")
  #Sys.sleep(5)
  x11()
  plot(hexbin(values(img1[[1]]), values(img1[[3]]), xbins=75),
       main="2. Hexagonal binning of red vs blue", xlab="redBand", ylab="blueBand")
  #Sys.sleep(5)
  x11()
  plot(hexbin(values(img1[[2]]), values(img1[[3]]), xbins=75),
       main="3. Hexagonal binning of green vs blue", xlab="greenBand", ylab="blueBand")
}

graphics.off()
close3d()
rm(img1, image_cleaner,r)


## REMOVE EMPTY IMAGES ---------------------------------------------------------
# Images too far from the GMA are downloaded as black images
# Thus before proceding black images must be identified

rm(list=ls())
source("scripts/utils/funzione_imageCleaner.R",  echo=TRUE)

data_poly = st_read("data/shapefile/Road_buffer_img_epsg3857.shp")
img_names = data_poly$image; rm(data_poly)

black_list = NULL

i = 0
for (name in img_names){  # corretto da 'names' a 'img_names'
  print(name)
  
  name2 = name # to have correspondance between image data names and file names
  name = strsplit(name, "_")
  n = length(name)
  for (j in 1:n)  # cambiato da 'i' a 'j' per evitare conflitti
    name2[j] = paste0("data/raster/", name[[j]][1],"__", name[[j]][2], ".tif", sep="")
  
  img = rast(name2)  # usato rast() invece di brick()
  img = image_cleaner(img)  
  
  if (ncol(img)==1 & nrow(img)==1)
    black_list = c(black_list, name)
  
  i = i+1
  if (i%%10==0)
    print(i/10)
}

save(black_list, file= "preprocessed-data/black_list.RData")

## CLEANING THE OSM DATASET ---------------------------------------------------
# Among all the features, select and filter only the most useful ones:
# delete or modify short segments, useless street types, empty images, ...

rm(list=ls())
data_poly = st_read("data/shapefile/Road_buffer_img_epsg3857.shp")

# only long-enough segments
data_poly = data_poly [data_poly$Length>=50, ] 

# remove the car racing circuit (since no interesting for mobility studies)
data_poly = data_poly[data_poly$osm_typo!="motorcycle",]

# change image name to have coherence with image files
change_name = function(name){ 
  name2 = name
  name = strsplit(name, "_")
  
  n = length(name)
  for (i in 1:n)
    name2[i] = paste0(name[[i]][1],"__", name[[i]][2], ".tif",sep="")
  
  return(name2)
}

names = change_name(data_poly$image)
data_poly$image = names

# remove data from external areas
load("preprocessed-data/black_list.RData")
rm_list = NULL
for (black in black_list)
  rm_list = c(rm_list, which(black==names))
data_poly = data_poly[-rm_list,]
  
# merge "asphalt" and "paved" types
asphalt_index = which(data_poly$osm_surf=="asphalt")
data_poly$osm_surf[asphalt_index] = "paved"

# correct type names
data_poly$osm_typo[which(data_poly$osm_typo == "unk")] = "unclassified"
data_poly$osm_typo[which(data_poly$osm_typo == "residentia")] = "residential"


# new order: paved, unpaved, unknown
unk_index = which(data_poly$osm_surf=="unk")
paved_index = which(data_poly$osm_surf=="paved")
unpaved_index = which(data_poly$osm_surf=="unpaved")
data_poly = data_poly[c(paved_index, unpaved_index, unk_index),]
  

st_write(data_poly, "preprocessed-data/Road_cleaned.shp")



# DESCRIPTIVE ANALYSIS OF OSM DATA ---------------------------------------------

rm(list=ls())
data_poly = st_read("preprocessed-data/Road_cleaned.shp")

summary(data_poly)
colnames(data_poly)
data_poly$osm_typo = as.factor(data_poly$osm_typo)
data_poly$osm_surf = as.factor(data_poly$osm_surf)
data_poly$id = as.character(data_poly$id)
summary(data_poly)
# Surface: 732 paved, 1826 unpaved, 50682 unknown


# PLOT: histogram of Surface type, Maputo by surface type
data_bar = data.frame(osm_surf = c("1. paved", "2. unpaved", "3. unknown"),
                      count = c(length(which(data_poly$osm_surf=="paved")),
                                length(which(data_poly$osm_surf=="unpaved")),
                                length(which(data_poly$osm_surf=="unk")))
                      )
x11(); ggplot(data=data_bar, aes(x=osm_surf, y=count, fill=osm_surf)) +
  geom_bar(stat="identity") +
  ggtitle("Paved, unpaved and unknown street pavement in Maputo")+
  scale_fill_manual(values=c("gray45", "#ffb01e", "snow2")) +
  labs(fill= "Pavement surface")+
  theme(legend.position="none") +
  theme(panel.grid.major = element_line(color = gray(.8), linetype=3, size=0.4), 
        panel.background = element_rect(fill="white"))+
  geom_text(aes(y=count, label=count), vjust=-0.5, color="black",
            position = position_dodge(0), size=3.5) +
  xlab("") + ylab("") 


i_pav = which(data_poly$osm_surf=="paved")
i_unp = which(data_poly$osm_surf=="unpaved")
i_unk = which(data_poly$osm_surf=="unk")

ttt = character(dim(data_poly)[1])
ttt[i_unk]= "1. unknown"; ttt[i_unp]="2. unpaved"; ttt[i_pav]="3. paved"

x11();  ggplot() + 
  geom_sf(data = data_poly, aes(color=ttt,fill=ttt))+
  scale_fill_manual(values=c("snow2", "#ffb01e", "grey45"))+
  scale_color_manual(values=c("snow2", "#ffb01e", "grey45"))+
  labs(fill= "Pavement surface")+
  ggtitle("Maputo Road Network", subtitle="Streets coloured by surface type") + 
  coord_sf() +
  theme(panel.grid.major = element_line(color = gray(.9), linetype=3, size=0.2), 
        panel.background = element_rect(fill="white"))+
  guides(color=FALSE)
rm(ttt, i_unk, i_pav, i_unp)


# PLOT: histogram of street type, Maputo by street type
cols = c("gray93", "gray73", "red4", "blue4", "orange" ,"chartreuse4"  )

ttt = as.character(data_poly$osm_typo)
names = levels(as.factor(ttt))[order(as.numeric(table(ttt)), decreasing=T)]
for (i in 1:length(names)){
  ttt[which(ttt==names[i])] = paste0(i, ". ", names[i])
  names[i] = paste0(i, ". ", names[i])
}
data_bar = data.frame(osm_type = names, count = summary(as.factor(ttt)))

x11(); ggplot(data=data_bar, aes(x=osm_type, y=count, fill=osm_type)) +
  geom_bar(stat="identity") +
  ggtitle("Street types in Maputo")+
  scale_fill_manual(values=cols) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_line(color = gray(.8), linetype=3, size=0.4), 
        panel.background = element_rect(fill="white"))+
  geom_text(aes(y=count, label=count), vjust=-0.5, color="black",
            position = position_dodge(0), size=3.5) +
  xlab("") + ylab("") 


x11();  ggplot() + 
  geom_sf(data = data_poly, aes(color=ttt, fill=ttt))+
  scale_fill_manual(values=cols)+
  scale_color_manual(values=cols)+
  labs(fill="Type of street:")+
  ggtitle("Maputo Road Network", subtitle="Streets coloured by their type") + 
  coord_sf() +
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white")) +
  guides(color = FALSE)

rm(cols,names,ttt,i)


# Relation between street type and pavement surface
data = st_read("preprocessed-data/Road_cleaned.shp")
data = data[-which(data$osm_surf=="unk"),]

t = table(data$osm_typo, data$osm_surf)
cols = c("gray45", "#ffb01e")

t1 = t
t2 = round(t/apply(t,1,sum)*100, 2);
t3 = cbind(t1[,1], t1[,1]+t1[,2]);  t3[2,2]=t3[2,2]+38; t3[4,2]=t3[4,2]+38

t2 = round(t/apply(t,1,sum)*100,1);
data_bar = data.frame(road_type = rep(levels(as.factor(data$osm_typo)),2),
                      road_surf = c(rep("2. paved", 6), rep("1. unpaved", 6)),
                      count = c(t[,1], t[,2]),
                      sum_count = c(t[,1], t[,2]+t[,1])+80, 
                      perc_count = c(paste0(t2[,1],"% pav"), paste0(t2[,2],"% unp")))
data_bar$sum_count[c(8,10)] = data_bar$sum_count[c(8,10)] + 50
x11(width=250, height=150); 
ggplot(data=data_bar, aes(x=road_type, y=count, fill=road_surf)) +
  geom_bar(stat="identity") +
  ggtitle("Distribution of paved and unpaved among street types")+
  labs(fill = "Pavement surface") +
  scale_fill_manual(values=c("#ffb01e", "gray45")) +
  theme(panel.grid.major = element_line(color = gray(.8), linetype=3, size=0.4), 
        panel.background = element_rect(fill="white"))+
  geom_text(aes(y=sum_count, label=perc_count), vjust=1.6, color="black",
            position = position_dodge(0), size=3.5) +
  xlab("") + ylab("") 

graphics.off()
