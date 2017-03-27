rm(list=ls())
###########################################################
#############################################################
########### Lab 10.5.1 K -Means Clustering ##################
##############################################################

set.seed(2)
x = matrix(rnorm(50*2),ncol =2)

##### the first 25 observations have a mean shift relative to the next 25 observations #####
##### you can try differet numbers, the larger the number is the more seperate the clusters gonna be in the plot###
#x[1:25,1]=x[1:25,1]+1
#x[1:25,2]=x[1:25,2]-2

x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

#x[1:25,1]=x[1:25,1]+8
#x[1:25,2]=x[1:25,2]-7

####Now we perform K-means clustering with K = 2 #####
######### Which is two dimensional  #################

km.out=kmeans(x,2,nstart = 20)
km.out$cluster
##### Plot with different colors#############
plot(x,col=(km.out$cluster), main ="K -Means Clustering Results with K =2", xlab="", ylab= "", pch =20, cex=2)
plot(x,col=(km.out$cluster+1), main ="K -Means Clustering Results with K =2", xlab="", ylab= "", pch =20, cex=2)
plot(x,col=(km.out$cluster+2), main ="K -Means Clustering Results with K =2", xlab="", ylab= "", pch =20, cex=2)

#####################################################################################################################
##If there is more than two varibales we perform PCA and then plot the first two principal components score vectors##
###################################################################################################################### 

set.seed(4)
km.out=kmeans(x,3,nstart = 20)
km.out
# tot.withinss is the total within cluster sum of squares#
# withinss is within cluster sum of squares #
km.out$withinss
km.out$tot.withinss


## Compare nstart =1 to nstart=20 #######################
## tot.withinss is the total within cluster sum of squares which we are seeking to minimize ##############

set.seed(3)
km.out=kmeans(x,3,nstart = 1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart = 20)
km.out$tot.withinss
##########################################
#### Loop for nstart ##########
###########################################
set.seed(3)
num.start = seq(1,50,by=1)
length.n = length(num.start)
Total.Withinss = rep(NA, length.n)
wssplot <- function(x,nc=3,nstart=1){
  for (i in 1:length.n){
    km.out=kmeans(x,3,nstart = num.start[i])
    Total.Withinss[i]=km.out$tot.withinss
  }
  dev.new()
  plot(1:length.n,  Total.Withinss, type="b", xlab="nstart",
       ylab="Ttoal Within cluster sum of squares")
}
wssplot(x,3)





