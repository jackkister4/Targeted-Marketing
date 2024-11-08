#Jack Kister Final Project

#necessary libraries
library(readxl)
library(cluster)
library(caret)
library(gains)
library(pROC)
library(dplyr)

#read excel
data<-read_excel("C:\\Users\\jackk\\OneDrive\\Desktop\\COSC 6520\\Jack Kister Final Project\\consumer_data.xlsx")

#part 1: Principal components analysis
#remove target variable for classification
data2<-(data[ , -1])

#check for correlation
cor(data2)

#standardize data
scaled<-scale(data2)

#remove null columns
scaled = subset(scaled, select = -c(17,18) )

#generate pca
pca<-prcomp(scaled)
summary(pca)

#PC22 captures 95% of the cumulative proportion of variance

#review pca weights
pca$rotation

#review pca scores
pca$x

#combine pca with target variable and remove original variables
data_pca<-data.frame(data, pca$x)
data_pca<-data_pca[ , -(2:34)]
data_pca<-data_pca[ , -(24:32)]



##part 2: classification using k nearest neighbor

#data partition
set.seed(1)
myIndex <- createDataPartition(data_pca$Accepted, p=0.6, list=FALSE)
trainSet <- data_pca[myIndex,]
validationSet <- data_pca[-myIndex,]

#10-fold cross validation
myCtrl <- trainControl(method="cv", number=10)

#expand grid to create k options
myGrid <- expand.grid(.k=c(1:10))

#set random seed to 1
set.seed(1)

#running the KNN model on training data
trainSet$Accepted <- as.factor(trainSet$Accepted)
KNN_fit <- train(Accepted ~ ., data=trainSet, method = "knn", trControl=myCtrl, tuneGrid = myGrid)
KNN_fit

#high predictive capability
#k = 9 yielded highest accuracy

#predict using validation set and confusion matrix statistics
#convert data and reference to factors
validationSet$Accepted <- as.factor(validationSet$Accepted)
KNN_Class <- predict(KNN_fit, newdata = validationSet)
KNN_Class <- as.factor(KNN_Class)
confusionMatrix(KNN_Class, validationSet$Accepted, positive = '1')

#generate predicted classifications from KNN model
KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')

#generate accepted prediction value
accepted_prediction <- ifelse(KNN_Class =='1',1,0)

# Add the accepted_prediction variable to the validation set data frame
validationSet$accepted_prediction <- accepted_prediction



##Part 3: clustering
#calculate distances between standardized pca variables
data_clustering <- dist(data_pca, method = "euclidean")

#agglomerative clustering
aResult <- agnes(data_clustering, diss = TRUE, method = "ward")
aResult

#set number of clusters
plot(aResult, labels = FALSE)
aClusters <- cutree(aResult, k = 10)
aClusters <- cutree(aResult, k = 10)
aClusters <- cutree(aResult, k = 10)

#agglomerative coefficient = 0.9998631

#add cluster data to pca data set
clusterdf<- data.frame(data_pca, aClusters)

#create the validation set equivalent (the last 882 observations)
#to match cluster value with acceptance prediction from KNN
myIndex2 <- createDataPartition(clusterdf$Accepted, p=0.6, list=FALSE)
validationSet_clustermatch <- clusterdf[-myIndex,]

#match classification data set with cluster data set
final_data<- data.frame(validationSet, validationSet_clustermatch)

#remove duplicate columns
final_data<-final_data[ , -(2:23)]
final_data<-final_data[ , -(3)]

#rearrange final data
final_data <- final_data[,c(1:2,25,3:24)]


#criteria for targeting
#criteria 1: must be in a cluster where approximately 40%+ accepted marketing offer
#criteria 2: must be predicted to purchase but not purchase


#generate criteria 1 condition
#high acceptance cluster
summary_table <- final_data %>%  group_by(aClusters)%>% summarise(Percentage_Accepted_1 = mean(Accepted =='1')*100)
summary_table

#clusters 1,7

#make sure clusters are of large enough size
table(final_data$aCluster)

#create high acceptance rate cluster
final_data <- final_data %>% mutate(high_accepted_rate = ifelse(aClusters %in%c(1,7),1,0))


#generate criteria 2 condition
#false positive
final_data <- final_data %>% mutate(false_positive = ifelse(Accepted =='0'& accepted_prediction ==1,1,0))

#generate variable for individuals that fulfill both criteria
final_data <- final_data %>% mutate(target_customer = ifelse(final_data[,26]==1& final_data[,27]==1,1,0))# Check the updated data frame
final_data <- final_data[,c(1:3,26:28,4:25)]

#number of target customers
sum_target_customer <-sum(final_data$target_customer)
sum_target_customer

