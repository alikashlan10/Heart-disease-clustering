# Installing Packages
install.packages("cluster")     
install.packages("stats")  
install.packages("dplyr")
install.packages("ggplot2") 
install.packages("ggportify") 
install.packages("ggfortify") 
install.packages("tidyr") 
install.packages("tidyverse") 
install.packages("psych")
install.packages("ggpubr")
install.packages("lattice")
install.packages("mlr3")
install.packages("mlr3cluster")
install.packages("mlr3viz")
install.packages("dbscan")
library(dbscan)
library(mlr3)
library(mlr3cluster)
library(mlr3viz)
library(lattice)
library(psych)
library(cluster)
library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(tidyverse)
library(factoextra)
library(ggfortify)
library(ggpubr)
library(car)
library(mlr3)
library(mlr3cluster)
library(mlr3viz)




# READING DATASET INTO DATA FRAME VARIABLE "data"

HD_data = read.delim("Heartdiseas.txt" , sep=",")

############################## PRE-PROCESSING ####################################



####### DATA CLEANING #######
#-----------------------------------------------------------------
data_cleaned <- drop_na(HD_data)
# number of obs in "data_cleaned" equals number of obs in "HD_data"
#which means no NA values found ( no dropped rows)

glimpse(data_cleaned)
# all columns are integers and doubles ( no need for converting) 

duplicated(data_cleaned) 
# there are no duplicated obs ( returns FALSE in all rows )
#-----------------------------------------------------------------




###### NORMALIZATION ######## (scaling)
#-----------------------------------------------------------------
data_normalized = (HD_data-min(HD_data))/(max(HD_data)-min(HD_data))
#(0-1) normalization approach
#-----------------------------------------------------------------




##### DETECTING OUTLIERS ######
#-----------------------------------------------------------------
boxplot(data_normalized)
boxplot(data_normalized$cp)
boxplot(data_normalized$trestbps)
boxplot(data_normalized$chol)
boxplot(data_normalized$thalach)
boxplot(data_normalized$oldpeak)
#boxplot(data_normalized$fbs)

dim(data_normalized)
# 303 12


##### REMOVING OUTLIERS


# CHOL OUTLIERS
quartiles <- quantile(data_normalized$chol, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_normalized$chol)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data_normalized, data_normalized$chol > Lower & data_normalized$chol < Upper)
dim(data_no_outlier) #298


# THALAC OUTLIERS
quartiles <- quantile(data_no_outlier$thalach, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_no_outlier$thalach)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data_no_outlier, data_no_outlier$thalach > Lower & data_no_outlier$thalach < Upper)
dim(data_no_outlier) #297


# TRESTBPS OUTLIERS
quartiles <- quantile(data_no_outlier$trestbps, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_no_outlier$trestbps)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data_no_outlier, data_no_outlier$trestbps > Lower & data_no_outlier$trestbps < Upper)
dim(data_no_outlier) #284


# CP OUTLIERS
quartiles <- quantile(data_no_outlier$cp, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_no_outlier$cp)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data_no_outlier, data_no_outlier$cp > Lower & data_no_outlier$cp < Upper)
dim(data_no_outlier) #264


# OLDPEAK OUTLIERS
quartiles <- quantile(data_no_outlier$oldpeak, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_no_outlier$oldpeak)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data_no_outlier, data_no_outlier$oldpeak > Lower & data_no_outlier$oldpeak < Upper)
dim(data_no_outlier) #259
################

boxplot(data_no_outlier)  #no outliers
#-----------------------------------------------------------------


######### end of pre-processing ###############




# loading data after removing outliers into " data " variable
data <- data_no_outlier
dim(data)
# removing "id" column 
data$id <- NULL
dim(data)


############################## STATISTICS OVER DATA #############################

#main statistics overall the data 

summary(data) # shows means,medians,min,max ....
describe(data) # shows variance,sd,range,min,max ....
cor(data) #correlation between coloumns


#histograms

hist(data$age)
hist(data$chol)
hist(data$trestbps)
hist(data$fbs)
hist(data$restecg)
hist(data$exang)
hist(data$oldpeak)
hist(data$slope)



#distribution of ages 
hist(data$age)

# age - thalach (cor=-0.41079717)
#----------------------------------------------------
#scatter plot
plot(data$age,data$thalach, main = "Main title",
     xlab = "age", ylab = "thalach",
     pch = 19, frame = FALSE)
abline(lm(data$thalach ~ data$age, data = data), col = "blue")
# barchart between age and thalach
barchart(data$age~data$thalach,data=data)
#----------------------------------------------------


# age-chol (cor=0.176049458)
#----------------------------------------------------
#scatter plot
plot(data$age,data$chol, main = "Main title",
     xlab = "age", ylab = "thalach",
     pch = 19, frame = FALSE)
abline(lm(data$chol ~ data$age, data = data), col = "blue")
# barchart between age and chol
barchart(data$age~data$chol,data=data)
#----------------------------------------------------


# trestbps - thalac (cor=)
#----------------------------------------------------
#scatter plot
plot(data$trestbps,data$thalach, main = "Main title",
     xlab = "age", ylab = "thalach",
     pch = 19, frame = FALSE)
abline(lm(data$trestbps ~ data$thalach, data = data), col = "blue")
# barchart between trestbps and thalach
barchart(data$trestbps~data$thalach,data=data)
#----------------------------------------------------


# trestbps - oldpeak (cor=)
#----------------------------------------------------
#scatter plot
plot(data$trestbps,data$oldpeak, main = "Main title",
     xlab = "age", ylab = "thalach",
     pch = 19, frame = FALSE)
abline(lm(data$trestbps ~ data$oldpeak, data = data), col = "red")
# barchart between trestbps and thalach
barchart(data$trestbps~data$oldpeak,data=data)
#----------------------------------------------------


#age-trestbps
barchart(data$age~data$trestbps,data=data)

# oldpeak - slope
#barchart(data$oldpeak~data$slope,data=data)

# cp - thalach/exang
barchart(data$cp~data$exang,data=data)


#sex-chol
#barchart(data$sex~data$chol,data=data)



#library(pheatmap)
#pheatmap(data[,5:10],cluster_rows=TRUE,cluster_cols=TRUE,clustering_method="complete")


#trials
#-------------------------------------------------


ggscatter(data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

plot(data$age, data$thalach, main = "Main title",
     xlab = "age", ylab = "thalach",
     pch = 19, frame = FALSE)

# Add regression line
plot(data$age,data$thalach, main = "Main title",
     xlab = "age", ylab = "thalach",
     pch = 19, frame = FALSE)
abline(lm(data$thalach ~ data$age, data = data), col = "blue")


x<-data$age
y<- data$thalach
scatterplot( data$thalach ~ data$age, data = data, 
            smoother = TRUE, grid = FALSE, frame = FALSE)



xyplot(y~x,data=data)
xyplot(y~x,data=data,
       xlab="age",
       ylab="thalach",
       main="",
       type=c("p","r"))

xyplot(data$oldpeak~data$slope,data=data)
xyplot(data$oldpeak~data$slope,data=data,
       xlab="age",
       ylab="thalach",
       main="",
       type=c("p","r"))

xyplot(data$cp~data$thalach,data=data)
xyplot(data$cp~data$thalach,data=data,
       xlab="exang",
       ylab="cp",
       main="",
       type=c("p","r"))





########################## UNSUPERVISED LEARNING TECHINUQES #################



#calculating optimal number of k clusters

fviz_nbclust(data,kmeans,method="wss")
fviz_nbclust(data,kmeans,method="silhouette")
# k = 2 



######### K-MEANS CLUSTRING 
#--------------------------------------------------------------

km_data = kmeans(data, 2)
km_data
km_data$cluster



#visualization

plot(data,col = km_data$cluster) 
autoplot(km_data,data,frame="true")

#plot(data$thalach,col = km_data$cluster) #visualization
#fviz_cluster(km_data, data, geom = "point")

#--------------------------------------------------------------




######### k-medoid
#--------------------------------------------------------------

fviz_nbclust(data, pam, method = "wss")

kmed_data <- pam(x=data,k=2) #applying pam method
print(kmed_data)
kmed_data$clustering 
kmed_data$medoids #show the medoids of the clusters


#visualization

autoplot(kmed_data,data,frame="true")
#fviz_cluster(kmed_data,data,geom="point")


#--------------------------------------------------------------



######## hierarchial 
#--------------------------------------------------------------
c_data <- prcomp(data)


distance<-dist(data,method = "euclidean") # calculating distance
hc_data <- hclust(distance,method="complete")
hc_data

# Dendrogram
fviz_dend(hc_data,k = 2,show_labels = FALSE)
plot(hc_data,cex=0.6) 
rect.hclust(hc_data,k=2,border =2:4)

#cluster viz

hclusters <- cutree(hc_data, k=2) #clusters
autoplot(object =pca_data ,label = FALSE) #data before clustring
autoplot(c_data,colour=hclusters) #data after clustring

#fviz_cluster(list(data=data,cluster=hclusters)) #visualization


#--------------------------------------------------------------




#comparison between clustering algorithms
#-------------------------------------------------

autoplot(kmed_data,data,frame="true") #k-medoids
autoplot(km_data,data,frame="true") #k-means
autoplot(pca_data,colour=hclusters) #hierarchical

#-------------------------------------------------










