setwd("C:/Users/ASUS/Desktop/CACI/Assignment 3")
getwd()
pacman::p_load(reshape2, ggplot2, psych, corrplot, fpc, cluster,
               nnet, mclust, e1071, randomForest)
data<-read.csv("smartwatch_survey.csv")
t(summary(data))
table(data$Income)
write.table(t(summary(data)), file = "mean of overall data",sep = ",", row.names = TRUE, col.names = TRUE)

#<-----Importance variables+WTP------------->>>>>>
data.sc.2 = scale(data[, 2:14])
#euclidean distance
dist.eucl.2 <- dist(data.sc.2,method = "euclidean")
as.matrix(dist.eucl.2)[1:6, 1:6]
cl.ward.eucl <- hclust(dist.eucl.2, method = "ward.D2")    
cor(cophenetic(cl.ward.eucl), dist.eucl.2)
data$cluster.ward <- cutree(cl.ward.eucl, 3)
clust.mean.ward <- aggregate(data[, -c(1,39)], 
                        by = list(cluster = data$cluster.ward), 
                        function(x)c(mean = round(mean(x), 2)))
t(clust.mean.ward)
write.table(t(clust.mean.ward), file = "Importance+WTP clustering ward 3-cluster.txt",sep = ",", row.names = TRUE, col.names = TRUE)
table(data$cluster.ward)
VRC.ward = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward[k] <- cluster.stats(d = dist.eucl.2, 
                               clustering = cutree(cl.ward.eucl, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward
length(VRC.ward)

# save as a data frame
VRC = data.frame(K = 2:10, ward = VRC.ward)

# reshape to long
VRC = melt(VRC, id.vars = "K")

# plot
ggplot(VRC, aes(x = K, y = value)) +
  ggtitle("Clustering with product attributes and WTP using euclidean distance and ward")+
  geom_point() + geom_line() +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()

cluster1 <- scale(data[data$cluster.ward == 1,2:14])

boxplot(as.data.frame(cluster1),main = "cluster 1: 39.8%")

cluster2 <- scale(data[data$cluster.ward == 2,2:14])

boxplot(as.data.frame(cluster2),main = "cluster 2: 18.9%")

cluster3 <- scale(data[data$cluster.ward == 3,2:14])

boxplot(as.data.frame(cluster3),main = "cluster 3: 28.4%")

cluster4 <- scale(data[data$cluster.ward == 4,2:14])

boxplot(as.data.frame(cluster4),main = "cluster 4: 12.9%")


df <- stack(cluster1)
ggplot(df, aes(x = factor(ind, levels = names(df)), y = values)) + geom_boxplot()
ggplot(stack(df), aes(x = ind, y = values)) +
  geom_boxplot()

table(cutree(cl.ward.eucl, 4))

write.table(t(clust.mean.ward), file = "Importance+WTP clustering ward.txt",sep = ",", row.names = TRUE, col.names = TRUE)



# As the k-means initial partition is random, fix the seed for reproducability
set.seed(185) 
#3 cluster k means
cl.kmeans.4 <- kmeans(data.sc.2, centers = 4)

# cluster assignments
table(cl.kmeans.4$cluster)


# combine with the original data
data$cluster_kmeans <- cl.kmeans.4$cluster
str(data)

clust.kmean <- aggregate(data[, -c(1,40,39)], 
                           by = list(cluster = data$cluster_kmeans), 
                           function(x)c(mean = round(mean(x), 2)))
write.table(t(clust.kmean), file = "Importance+WTP clustering kmeans.txt",sep = ",", row.names = TRUE, col.names = TRUE)

t(clust.kmean)
head(data)


#<<<<<<<<<<<<<<------------ kmeans classification-------------------->>>>>>>>>>>>>>>>>>
predictor_classification <- data[,c(17:26,35:38,40)]
predictor_classification <- data
set.seed(04623)   # fix the seed for reproducability
train.pop <- 0.60 # we will use 65-35% split
str(predictor_classification)
N <- nrow(predictor_classification) # total sample size

# randomly sample 60\% of observations
train.cases <- sample(N, N*train.pop)
length(train.cases)
# assign the randomly sampled 60% of obs. to training dataset
data.train <- predictor_classification[train.cases, ] 
nrow(data.train)

# assign the rest to test dataset
data.test <- predictor_classification[-train.cases, ]
nrow(data.test)
#<<<--- all demographic variables exluding iphone,compbuy,amaznp, media use------------->>>>
#<<<<<<----- ward cluster---------------->>>>>>>>>>>>>>>
# Step 2: Train the prediction model ===========================================
# Multinomial logistic regression ----------------------------------------------
# Unique known segments
unique(predictor_classification$cluster_kmeans)
# last level will be treated as reference
head(data.train)
dim(data.train)
logistic.kmeans <- multinom(cluster_kmeans ~ ., data = data.train)
logistic.kmeans <- multinom(cluster_kmeans ~ Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                              Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income, data = data.train)

summary(logistic.kmeans)
# segment allocation
data.test$seg_log_kmeans <- predict(logistic.kmeans, newdata = data.test)
head(data.test)

# How well did the model predict?
mean(data.test$cluster_kmeans == data.test$seg_log_kmeans)*100
# 70.5 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(data.test$seg_log_kmeans, data.test$cluster_kmeans)*100
# 38.98328%


nb_kmeans <- naiveBayes(cluster_kmeans ~ Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                          Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income , data = data.train)
(nb_kmeans)
# predict segment allocation for test data
data.test$seg_nb_kmeans <- predict(nb_kmeans, data.test)
head(data.test)
# How good did the model do?
mean(data.test$cluster_kmeans == data.test$seg_nb_kmeans)*100
# 56.25 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(data.test$seg_nb_kmeans, data.test$cluster_kmeans)*100
# [1] 20.05474 %



#random forest
data.train$cluster_kmeans <- factor(data.train$cluster_kmeans)
set.seed(98040)
head(data.train)

rf_kmeans <- randomForest(cluster_kmeans ~ Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                            Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income , data = data.train, ntree = 3000)
(rf_kmeans)
# predict segment allocation for test data
data.test$seg_rf_kmeans <- predict(rf_kmeans, data.test)
head(data.test)
# How good did the model do?
mean(data.test$cluster_kmeans == data.test$seg_rf_kmeans)*100
#66 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(data.test$seg_rf_kmeans, data.test$cluster_kmeans)*100
# 42.74012%

#<------------ward classification--------->>
pred_ward_classification <- data[,c(17:26,35:38,39)]
str(pred_ward_classification)



set.seed(04623)   # fix the seed for reproducability
train.pop <- 0.60 # we will use 65-35% split
str(pred_ward_classification)
N <- nrow(pred_ward_classification) # total sample size
N
# randomly sample 60\% of observations
train.cases.ward <- sample(N, N*train.pop)
length(train.cases.ward)
# assign the randomly sampled 60% of obs. to training dataset
data.train.ward <- pred_ward_classification[train.cases.ward, ] 
nrow(data.train.ward)

# assign the rest to test dataset
data.test.ward <- pred_ward_classification[-train.cases.ward, ]
nrow(data.test.ward)
#<<<--- all demographic variables exluding iphone,compbuy,amaznp, media use------------->>>>
#<<<<<<----- ward cluster---------------->>>>>>>>>>>>>>>
# Step 2: Train the prediction model ===========================================
# Multinomial logistic regression ----------------------------------------------
# Unique known segments
unique(pred_ward_classification$cluster.ward)
# last level will be treated as reference
head(data.train.ward)
dim(data.train.ward)
logistic.ward <- multinom(cluster.ward ~ . , data = data.train.ward)
summary(logistic.ward)

# segment allocation
data.test.ward$seg_log_ward <- predict(logistic.ward, newdata = data.test.ward)
head(data.test.ward)

# How well did the model predict?
mean(data.test.ward$cluster.ward == data.test.ward$seg_log_ward)*100
#  65.25 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(data.test.ward$seg_log_ward, data.test.ward$cluster.ward)*100
#  32.27213%


nb_ward <- naiveBayes(cluster.ward ~  . , data = data.train.ward)
(nb_ward)
# predict segment allocation for test data
data.test.ward$seg_nb_ward <- predict(nb_ward, data.test.ward)
head(data.test.ward)
# How good did the model do?
mean(data.test.ward$cluster.ward == data.test.ward$seg_nb_ward)*100
#   50.75 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(data.test.ward$seg_nb_ward, data.test.ward$cluster.ward)*100
# [1] 11.77575 %



#random forest
data.train.ward$cluster.ward <- factor(data.train.ward$cluster.ward)
set.seed(98040)
head(data.train.ward)

rf_ward <- randomForest(cluster.ward ~ . , data = data.train.ward, ntree = 3000)
(rf_ward)
# predict segment allocation for test data
data.test.ward$seg_rf_ward <- predict(rf_ward, data.test.ward)
head(data.test.ward)
# How good did the model do?
mean(data.test.ward$cluster.ward == data.test.ward$seg_rf_ward)*100
#72.25 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(data.test.ward$seg_rf_ward, data.test.ward$cluster.ward)*100
?
# 45.5725%
?adjustedRandIndex
