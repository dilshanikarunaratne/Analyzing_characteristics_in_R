#Load required libraries
library(dplyr)
library(tidyr) 
library(moments) 
library(reshape2)
library(ggplot2)
library(corrplot)
library('ISLR')
library(MASS)
library(gridExtra)



#Set working directory
setwd("C:/Users/Dilshani/OneDrive - University of Essex/MA335/Assignment")
#Read csv file into a data frame
proj_data <-  read.csv("project data.csv") 
#Count number of rows in the data frame
nrow(proj_data)
#Convert M/F column to a factor variable
proj_data$M.F <- as.factor(proj_data$M.F)
#Convert the categorical variables into numerical values
proj_data$M.F <- as.numeric(proj_data$M.F)
#Check the structure of data frame
str(proj_data)
#Remove rows with "Group" value equal to Converted
proj_data <- subset(proj_data, !(Group == "Converted"))
#Count number of rows left in the data frame
nrow(proj_data)
#Remove rows with missing values
proj_data <- na.omit(proj_data)
#Count number of rows left in the data frame
nrow(proj_data)


#Question 1

#Obtain a summery of the data frame
summary(proj_data)

#Count "Demented" observations
nrow(subset(proj_data, (Group == "Demented")))
#Count "Non-demented" observations
nrow(subset(proj_data, (Group == "Nondemented")))

#Draw a bar plot for the "Group" dependent variable
ggplot(proj_data, aes(x=as.factor(Group), fill=as.factor(Group) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("brown3", "deepskyblue4") )+
  ylab("Count of records") +
  xlab("Group") +
  ggtitle("Disrtibution of  diagnosis of Alzheimer") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold", vjust = 1.5 , margin = margin(b = 10))) +
  guides(fill= "none")
          

#Draw a frequency table for categorical variables of M.F and SES
table_proj <- table(proj_data$M.F, proj_data$SES); table_proj




#Draw a barplot for Gender variable
ggplot(proj_data, aes(x = as.factor(M.F), fill = as.factor(M.F))) +
  geom_bar() +
  scale_fill_manual(values = c("brown3", "deepskyblue4"), labels = c("Male", "Female")) +
  labs(fill = "Gender") +
  ylab("Count of records") +
  xlab("M.F") +
  ggtitle("Distribution of Gender") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold", vjust = 1.5, margin = margin(b = 10))) +
  guides(fill = guide_legend(title = "Gender"))  # Show legend with custom title and labels

#Draw a barplot for Socioeconomic Status variable
ggplot(proj_data, aes(x = as.factor(SES), fill = as.factor(SES))) +
  geom_bar() +
  scale_fill_manual(values = c("brown3", "deepskyblue4", "seagreen", "violetred", "wheat3"),
                    labels = c("Low", "Low Medium", "Medium", "Medium Medium", "High")) +
  labs(fill = "SES") +
  ylab("Count of records") +
  xlab("SES") +
  ggtitle("Distribution of Socioeconomic Status") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold", vjust = 1.5, margin = margin(b = 10))) +
  guides(fill = guide_legend(title = "SES"))  # Show legend with custom title and labels


# Create box plots for each numerical variable
#Numerical variable : Age
ggplot(proj_data, aes(x = as.factor(Group), y = Age, fill = as.factor(Group))) +
  geom_boxplot() +
  xlab("Group") +
  ylab("Age") +
  scale_fill_manual(values = c("brown3", "deepskyblue4")) +
  ggtitle(paste("Boxplot of Age")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#Numerical variable : Year of education
ggplot(proj_data, aes(x = as.factor(Group), y = EDUC, fill = as.factor(Group))) +
  geom_boxplot() +
  xlab("Group") +
  ylab("Year of education") +
  scale_fill_manual(values = c("brown3", "deepskyblue4")) +
  ggtitle(paste("Mini mental state examination")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#Numerical variable : Year of education
ggplot(proj_data, aes(x = as.factor(Group), y = MMSE, fill = as.factor(Group))) +
  geom_boxplot() +
  xlab("Group") +
  ylab("Mini mental state examination") +
  scale_fill_manual(values = c("brown3", "deepskyblue4")) +
  ggtitle(paste("Boxplot of  Mini mental state examination")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#Numerical variable : Clinical dementia rating
ggplot(proj_data, aes(x = as.factor(Group), y = CDR, fill = as.factor(Group))) +
  geom_boxplot() +
  xlab("Group") +
  ylab("Clinical dementia rating") +
  scale_fill_manual(values = c("brown3", "deepskyblue4")) +
  ggtitle(paste("Boxplot of Clinical dementia rating")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#Numerical variable : Estimated total intracranial volume
ggplot(proj_data, aes(x = as.factor(Group), y = eTIV, fill = as.factor(Group))) +
  geom_boxplot() +
  xlab("Group") +
  ylab("Estimated total intracranial volume") +
  scale_fill_manual(values = c("brown3", "deepskyblue4")) +
  ggtitle(paste("Boxplot of Estimated total intracranial volume")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#Numerical variable : Normalize whole brain volume
ggplot(proj_data, aes(x = as.factor(Group), y = nWBV, fill = as.factor(Group))) +
  geom_boxplot() +
  xlab("Group") +
  ylab("Normalize whole brain volume") +
  scale_fill_manual(values = c("brown3", "deepskyblue4")) +
  ggtitle(paste("Boxplot of Normalize whole brain volume")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#Numerical variable : Atlas scaling factor
ggplot(proj_data, aes(x = as.factor(Group), y = ASF, fill = as.factor(Group))) +
  geom_boxplot() +
  xlab("Group") +
  ylab("Atlas scaling factor") +
  scale_fill_manual(values = c("brown3", "deepskyblue4")) +
  ggtitle(paste("Boxplot of  Atlas scaling factor")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


#Store numerical variables into a vector
numerical_vars <- c("Age", "EDUC", "MMSE", "CDR", "eTIV", "nWBV", "ASF")

# Compute the correlation matrix
cor_matrix <- cor(proj_data[, numerical_vars])

# Print the correlation matrix
print(cor_matrix)
corrplot.mixed(cor_matrix, lower.col = "black", number.cex = .7)



#Question 2

#install.packages("factoextra")
library(factoextra)
#Compute distance matrix
#To obtain more clear plot, the data set was divided into 4 sets and compute the distance matrices
data1 <- proj_data[0:53, numerical_vars]
distance.corr1 <- get_dist(data1, stand = TRUE, method = "pearson")
d1 <- fviz_dist(distance.corr1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

data2 <- proj_data[81:160, numerical_vars]
distance.corr2 <- get_dist(data2, stand = TRUE, method = "pearson")
d2 <- fviz_dist(distance.corr1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

data3 <- proj_data[161:240, numerical_vars]
distance.corr3 <- get_dist(data3, stand = TRUE, method = "pearson")
d3 <- fviz_dist(distance.corr3, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

data4 <- proj_data[241:317, numerical_vars]
distance.corr4 <- get_dist(data4, stand = TRUE, method = "pearson")
d4 <- fviz_dist(distance.corr4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Draw the distance plots
library(gridExtra)
grid.arrange(d1, d2, d3 , d4,  ncol = 2, nrow=2)

#K-means clustering

#Make a subset of the proj_data dataset that only contains the columns that relate to numerical variables.
numeric_data <- proj_data[, numerical_vars]
#Scaling to standardize the values of the variables
proj_data_scaled <- scale(numeric_data)
set.seed(123)
#Perform k-means clustering when k=2
kmeans2 <- kmeans(proj_data_scaled, centers = 2, nstart = 20)
#Perform k-means clustering when k=3
kmeans3 <- kmeans(proj_data_scaled, centers = 3, nstart = 20)
#Perform k-means clustering when k=4
kmeans4 <- kmeans(proj_data_scaled, centers = 4, nstart = 20)
#Check what's inside the first cluster created
kmeans2
#Check the structure of the cluster
str(kmeans2)

#Visualize kmeans2 cluster
f1<- fviz_cluster(kmeans2, data = proj_data_scaled)
#Visualize kmeans3 cluster
f2<- fviz_cluster(kmeans3, data = proj_data_scaled)
#Visualize kmeans4 cluster
f3<- fviz_cluster(kmeans4, data = proj_data_scaled)

#Visualize the optimal number of clusters of k-means clustering
f4<- fviz_nbclust(proj_data_scaled, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)

#Draw all the plots in one camvas
grid.arrange(f1, f2, f3, f4 , nrow=2, ncol = 2)


#Hierarchical clustering

#Calculate the distance matrix
d <- dist(data1, method = "euclidean")

#Apply hierarchical clustering for different linkage methods
#Single linkage method
fit.single <- hclust(d, method="single")
#Complete linkage method
fit.complete <- hclust(d, method="complete")
#Average linkage method
fit.average <- hclust(d, method="average")
#Centroid linkage method
fit.centroid <-hclust(d, method="centroid")

#Print the dendogram of single linkage method
plot(fit.single)
#Cut tree into k=7 clusters
groups.fit.single <- cutree(fit.single, k=7) 
#Draw dendogram with red borders around the 7 clusters
rect.hclust(fit.single, k=7, border="red") 
#Checking how many observations are in each cluster of single linkage method 
table(groups.fit.single)

#Print the dendogram of complete linkage method
plot(fit.complete)
#Cut tree into k=6 clusters
groups.fit.complete <- cutree(fit.complete, k=6)
#Draw dendogram with red borders around the 6 clusters
rect.hclust(fit.complete, k=6, border="red")
#Checking how many observations are in each cluster of complete linkage method 
table(groups.fit.complete)

#Print the dendogram of average linkage method
plot(fit.average)
#Cut tree into k=4 clusters
groups.fit.average <- cutree(fit.average, k=4)
#Draw dendogram with red borders around the 4 clusters
rect.hclust(fit.average, k=4, border="red")
#Checking how many observations are in each cluster of average linkage method 
table(groups.fit.average)

#Print the dendogram of centroid linkage method
plot(fit.centroid)
#Cut tree into k=3 clusters
groups.fit.centroid <- cutree(fit.centroid, k=3)
#Draw dendogram with red borders around the 3 clusters
rect.hclust(fit.centroid, k=3, border="red")
#Checking how many observations are in each cluster of centroid linkage method 
table(groups.fit.centroid)

#Check the mean values of each attribute to the cluster in centroid linkage method
aggregate(data1, by=list(cluster=groups.fit.centroid), mean)
#Check the  max distance values of each attribute to the cluster in complete linkage method
aggregate(data1, by=list(cluster=groups.fit.complete), max)
#Check the mean values of each attribute to the cluster in average linkage method
aggregate(data1, by=list(cluster=groups.fit.average), mean)


# table(groups.fit.centroid, proj_data$Group)
# ggplot(proj_data, aes(proj_data$Age, proj_data$SES, color = as.factor(proj_data$Group))) +
#   geom_point(alpha = 0.4, size = 3.5) + geom_point(col = groups.fit.centroid) +
#   scale_color_manual(values = c('black', 'red', 'green', 'blue'))



#Question 3
attach(proj_data)
#Categorical variable is encoded with binary values
proj_data$Group <- ifelse(proj_data$Group == "Demented", 1, 0)
#Build logistic regression model
glm.fit<-glm(Group~Age+EDUC+MMSE+CDR+eTIV+nWBV+M.F+SES+ASF,data=proj_data,family=binomial)
#check summary of the logistic regression model
summary(glm.fit)
#contrasts(as.factor(Group))
#Generate predicted probabilities of the logistic regression model
glm.probs <- predict(glm.fit,type="response") #Pr(Y=1|X)
#glm.predicted <- rep(0,nrow(proj_data))
#glm.predicted[glm.probs>0.5]=1
#Initializes a vector with zeros for each row
glm.predicted <- rep(0,nrow(proj_data))
#Assign values of the vector according to the predicted probabilities
glm.predicted[glm.probs>0.5]=1
#Generate a contingency table
table(glm.predicted, proj_data$Group)
#Calculate the accuracy of the predictions
mean(glm.predicted==proj_data$Group)

#Draw the ROC curve
library(ROCR)
pred <- prediction(glm.predicted, proj_data$Group)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

# #Linear discriminant analysis
# #install.packages("caTools")
# library(caTools)
# sample <- sample.split(proj_data$M.F, SplitRatio = 0.7)
# train  <- as.data.frame(subset(proj_data, sample == TRUE))
# test   <-  as.data.frame(subset(proj_data, sample == FALSE))
# 
# nrow(test);nrow(train)
# 
# lda.fit<-lda(Group~Age+EDUC+MMSE+CDR+eTIV+nWBV+M.F+SES+ASF,data=train)
# lda.fit
# plot(lda.fit)
# 
# lda.predicted <- predict(lda.fit,test)
# table(lda.predicted$class, test$Group)
# mean(lda.predicted$class==proj_data$Group)
# mean(lda.predicted$class==1)
# mean(lda.predicted$class==0)
# sum(lda.predicted$posterior[,1]>=.5)
# sum(lda.predicted$posterior[,1]<.5)
# 
# #Quadratic discriminant analysis
# qda.fit<-qda(Group~Age+EDUC+MMSE+CDR+eTIV+nWBV+M.F+SES+ASF,data=proj_data,subset=train)
# qda.fit
# qda.predicted <- predict(qda.fit,proj_data)$class
# table(qda.predicted, Group)
# mean(qda.predicted==proj_data$Group)
# 
# 
# #K Nearest Neighbours
# library(class)
# 
# # Set the seed for reproducibility
# set.seed(1)
# 
# # Splitting the data into training and testing sets
# train_indices <- sample(1:nrow(proj_data), size = 0.7*nrow(proj_data), replace = FALSE)
# train.X <- proj_data[train_indices, c("Age", "EDUC",  "MMSE", "CDR", "eTIV", "nWBV", "M.F", "SES", "ASF")]
# test.X <- proj_data[-train_indices, c("Age", "EDUC",  "MMSE", "CDR", "eTIV", "nWBV", "M.F", "SES", "ASF")]
# train.Group <- proj_data[train_indices, "Group"]
# 
# # Applying KNN algorithm
# knn.pred <- knn(train.X, test.X, train.Group, k = 10)
# 
# # Generating the confusion matrix
# table(knn.pred, proj_data[-train_indices, "Group"])
# 
# knn.pred <- knn(train.X, test.X, train.Group, k = 3)
# table(knn.pred, proj_data[-train_indices, "Group"])
# mean(knn.pred == proj_data$Group)
# 
# 
# knn.pred <- knn(train.X, test.X, train.Group, k = 2)
# table(knn.pred, proj_data[-train_indices, "Group"])












#Question 4

#Random forest

#boruta
#install.packages("Boruta")
library(Boruta)


boruta1 <- Boruta(proj_data$Group ~., data=proj_data, doTrace=1)
decision<-boruta1$finalDecision
signif <- decision[boruta1$finalDecision %in% c("Confirmed")]
print(signif)
plot(boruta1, xlab="", main="Variable Importance")
attStats(boruta1)


#caret
#install.packages("rlang")
#install.packages(c("recipes", "caret"))


str(proj_data)
library(caret)
install.packages("recipes")
library(MASS)
proj_data$Group <- as.factor(proj_data$Group)
# # For leave-one-out CV
#trControl <- trainControl(method = "LOOCV")
#trControl <- trainControl(method="repeatedcv", number=10, repeats=3)
trControl <- trainControl(method = "cv", number = 10)
lda.fit <- train(Group~M.F+Age+EDUC+SES+MMSE+CDR+eTIV+nWBV+ASF,
                 method = "lda",
                 trControl = trControl,
                 metric = "Accuracy",
                 data = proj_data)
importance <- varImp(lda.fit, scale=FALSE);importance

library(caret)
y<-cbind(Group)
X<-cbind(M.F, Age , EDUC, SES, MMSE, CDR, eTIV, nWBV, ASF)
set.seed(10)
control <- rfeControl(functions = lmFuncs,
                      method = "repeatedcv",#cv
                      repeats = 10,
                      number = 10)
lmProfile <- rfe(proj_data[, -1], proj_data$Group,
                 sizes = c(1:8),
                 rfeControl = control)
lmProfile
plot(lmProfile, type = c("g", "o"))

lmProfile$optVariables
plot(lmProfile, type = c("g", "o"))
plot(lmProfile, metric = "Rsquared", type = c("g", "o"))
predictors(lmProfile)
lmProfile$fit




# 
# #PCA
# library(dplyr)
# proj_data$Group <- as.numeric(proj_data$Group)
# str(proj_data)
# pca1<- prcomp(proj_data , scale =TRUE)
# names(pca1)
# summary(pca1)
# print(pca1,digit=2) # the loadings
# pca1.loadings<- pca1$rotation
# pca1.scores <- pca1$x
# #The percentage of variance explained and the cummulative variance explained
# per.var <- pca1$sdev * pca1$sdev
# prop.var.expl <- per.var/sum(per.var); prop.var.expl
# cumsum(prop.var.expl)
# plot(pca1,type="l")
# plot(prop.var.expl, xlab = "Principal Components", ylab = "Percentage of Variance Explained",
#      ylim = c(0, 1), type = 'b', xlim = c(1, length(prop.var.expl)))
# 
# plot(cumsum(prop.var.expl), xlab = "Principal Component", ylab = "Cumulative Percentage of Variance Explained",
#      ylim = c(0, 1), type = 'b', xlim = c(1, length(prop.var.expl)))

