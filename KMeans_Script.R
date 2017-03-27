######################################
### TEAM 12 K-MEANS CLUSTERING LAB ###
######################################
rm(list=ls())
#install.packages("NbClust")
library(NbClust)
#install.packages("jpeg")
library(jpeg)
#install.packages("ggplot2")
library(ggplot2)
setwd("C:\\Users\\Cole Combs\\Documents\\MSBA\\SEMESTER II\\Machine Learning II\\Team 12 Presentation - Clustering")

#####IRIS DATASET EXAMPLE##### 

#Initial Data Discussion and Prep#
head(iris)

iris_2 <- iris[-5] #Remove Categorical Variables
head(iris_2)

iris_3 <- as.data.frame(scale(iris_2)) #Scale Variables
head(iris_3)

sapply(iris_2,mean) #Unscaled Mean and SD
sapply(iris_2,sd)

sapply(iris_3,mean) #Scaled Mean and SD, AKA Mean Zero - S.D. One
sapply(iris_3,sd)

#Create WSS Plot Function and Run Example on Iris_3 Dataset
#Function will calculate “With-In-Sum-Of-Squares (WSS),” a measure 
#to explain the homogeneity within a cluster

wssplot <- function(data, nc=0, seed=2015){
	#Generate WSS for unclustered dataset
	wss <- (nrow(data)-1)*sum(apply(data,2,var))
	#Generate WSS for 2 trough k clusters
  	for (i in 2:nc){
    		set.seed(seed)
    		wss[i] <- sum(kmeans(data, centers=i,nstart=1)$withinss) 
		#nstart should be larger to account for randomness of initial clusters
	}
  dev.new()
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
wssplot(iris_3,30)

#WSS decreases drastically until K=3, and flattens out around K=7
#Use K=7 and Find WSS for Each Variable's 7 Clusters
iris_kmeans <- kmeans(iris_3,7)
str(iris_kmeans)
# $cluster identifies which cluster each observation was assigned to
# $centers records the centroids/means for each variable and cluster
iris_kmeans$centers

# $totss is total sum of squares, which is the total distance from data points to global mean
# $withinss is the total distance from a cluster's data points to its centroid/mean
# $tot.withinss is the total of all WSS values
# $betweenss is the total weighted distance of various cluster centroids to the global mean
# $size is the size of each cluster
iris_kmeans$size

# $iter is the number of outer iterations
# $ifault is an indicator for possible errors - 0 meaning none

#Compare to species type to examine for patterns
clusters <- iris_kmeans$cluster

iris_3$clusters <- clusters
table(iris$Species,clusters)

########CLUSTERING WINE EXAMPLE#######
#install.packages('rattle')
#install.packages('cluster')
library(rattle)
library(cluster)

data <- wine
head(data)

data <- scale(data[-1]) #Scale data to Mean Zero-SD One
head(data)

k.means.fit <- kmeans(data,3)
o = order(k.means.fit$cluster)
k.means.fit$centers #Look at each variables cluster means
k.means.fit$cluster #Look at cluster assignment 
k.means.fit$size #Examine total size of clusters


wssplot(data,10) #What do we see with looking at 1 to 10 clusters
wssplot(data,50) #Looking at 50 clusters, we see it is important to consider larger picture

#Using clust plot to examine clusters using only the first and second primary components
dev.new()
clusplot(data, k.means.fit$cluster, main = "2D Representation of Cluster Solution", color = TRUE, shade = TRUE, lines = 0)
table(wine[,1],k.means.fit$cluster)

#####IMAGE RECREATION WITH CLUSTERING######
img <- readJPEG("ClusteringImage.jpg") #Read two images into R
#Create a function to pulls RGB colors and coords from JPEG
img_rgb <- function(image){
	img_dim <- dim(image)
	data.frame(
  		x_axis = rep(1:img_dim[2], each = img_dim[1]),
  		y_axis = rep(img_dim[1]:1, img_dim[2]),
  		Red = as.vector(img[,,1]),
  		Green = as.vector(img[,,2]),
  		Blue = as.vector(img[,,3]))
}
img_colors <- img_rgb(img)

#wssplot(img_colors[c(3,4,5)],25) #Examine WSS as more clusters are considered


#Plot Initial Image
dev.new()
ggplot(data = img_colors, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = rgb(img_colors[c("Red", "Green", "Blue")])) +
  labs(title = "Original Image") +
  xlab("x-axis") +
  ylab("y-axis") + 
  coord_fixed(ratio = 1)

#Recreate Initial Image with K clusters
# Try 5 and 10
ks = 10
k_img_clstr <- kmeans(img_colors[, c("Red", "Green", "Blue")], centers = ks)
k_img_colors <- rgb(k_img_clstr$centers[k_img_clstr$cluster,])

#Plotting the Compressed Image
dev.new()
ggplot(data = img_colors, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = k_img_colors) +
  labs(title = paste("k-Means Clustering of", ks, "Colors")) +
  xlab("x") +
  ylab("y") + 
  coord_fixed(ratio = 1)

#####MEDICAL APPLICATION######
#We will use an image from a hemotoxylin and eosin staining procedure
#HE stains allow pathologists and researchers to differentiate between 
#tissue types and cell structures due to variable absorption of the 
#chemicals

img <- readJPEG("ClusteringImage4.jpg") #Medical Image
#Create a function to pulls RGB colors and coords from JPEG
img_rgb <- function(image){
	img_dim <- dim(image)
	data.frame(
  		x_axis = rep(1:img_dim[2], each = img_dim[1]),
  		y_axis = rep(img_dim[1]:1, img_dim[2]),
  		Red = as.vector(img[,,1]),
  		Green = as.vector(img[,,2]),
  		Blue = as.vector(img[,,3]))
}
img_colors <- img_rgb(img)
wssplot(img_colors[c(3,4,5)],30)

#Plot Initial Image
dev.new()
ggplot(data = img_colors, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = rgb(img_colors[c("Red", "Green", "Blue")])) +
  labs(title = "Original Image") +
  xlab("x-axis") +
  ylab("y-axis") + 
  coord_fixed(ratio = 1)


#Recreate Initial Image with K clusters
ks = 15
k_img_clstr <- kmeans(img_colors[, c("Red", "Green", "Blue")], centers = ks)
k_img_colors <- rgb(k_img_clstr$centers[k_img_clstr$cluster,])
str(k_img_colors) #Examining the structure of this new variable, we see it contains
#the hex codes needed to represent each pixel's new cluster color

#Plotting the Compressed Image
dev.new()
ggplot(data = img_colors, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = k_img_colors) +
  labs(title = paste("k-Means Clustering of", ks, "Colors")) +
  xlab("x") +
  ylab("y") + 
  coord_fixed(ratio = 1)

#### PLOTTING SELECTED CLUSTERS IN MEDICAL EXAMPLE#####

#Plot Unique Colors
y = 1:ks
x = rep(3,ks)
col = rep(NA,ks)
names = rep(NA,ks)
subset = 1
for (i in 1:ks){
	x[i] = i
	col[i] = rgb(k_img_clstr$centers[k_img_clstr$cluster[k_img_clstr$cluster == i],])
	names[i] = paste("Grp",i)
	subset = subset + 1

}
z = as.data.frame(cbind(x,y,names,col))
unq = unique(unlist(k_img_colors))

dev.new()
ggplot(data = z, aes(x = x, y = y, label = names)) +
  geom_point(colour = col, size = 6) +
  labs(title = paste("Unique Colors From", ks, "Clusters")) +
  geom_text(aes(label=names),hjust=.5, vjust=2)+
  theme(axis.title.x=element_blank(),
	  axis.title.y=element_blank(),
        axis.text.x=element_blank(),
	  axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
	  axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1)

#Create Base Image Colors for Subsetting Image Color Selection
img <- readJPEG("ClusteringImage4_Grey.jpg")
#Create a function to pulls RGB colors and coords from JPEG
img_rgb <- function(image){
	img_dim <- dim(image)
	data.frame(
  		x_axis = rep(1:img_dim[2], each = img_dim[1]),
  		y_axis = rep(img_dim[1]:1, img_dim[2]),
  		Red = as.vector(img[,,1]),
  		Green = as.vector(img[,,2]),
  		Blue = as.vector(img[,,3]))
}
base_colors <- img_rgb(img)

#Plot Base Image
dev.new()
ggplot(data = base_colors, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = rgb(base_colors[c("Red", "Green", "Blue")])) +
  labs(title = "Base Image") +
  xlab("x-axis") +
  ylab("y-axis") + 
  coord_fixed(ratio = 1)

base_clstr <- kmeans(base_colors[, c("Red", "Green", "Blue")], centers = 30)
subset_colors <- rgb(base_clstr$centers[base_clstr$cluster,])
head(subset_colors)

#Generate Subset Image Colors
subset = 9 #SELECT CHOSEN SUBSET COLOR TO HIGHLIGHT
selected_colors <- rgb(k_img_clstr$centers[k_img_clstr$cluster[k_img_clstr$cluster == subset],])
for (i in 1:length(k_img_clstr$cluster)){
	if (k_img_clstr$cluster[i] == subset){subset_colors[i] <- selected_colors[1]}
}

#Plotting the Subset Image
dev.new()
ggplot(data = img_colors, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = subset_colors) +
  labs(title = paste("Cluster Analysis of Group:", subset)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed(ratio = 1)
