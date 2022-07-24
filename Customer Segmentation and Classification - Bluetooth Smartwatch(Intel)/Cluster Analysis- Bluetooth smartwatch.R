setwd("C:/Users/ASUS/Desktop/CACI/Assignment 3")
getwd()
pacman::p_load(reshape2, ggplot2, psych, corrplot, fpc, cluster,
               nnet, mclust, e1071, randomForest)
data<-read.csv("smartwatch_survey.csv")
fresh <- read.csv("smartwatch_survey.csv")
head(data)
summary(data[,-1])
str(data)
boxplot_var <- data.frame(data[,2])
ggplot(data = data, aes(x=" ",y = data[,3])) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", color = "darkred")+
  labs(y = "")+
  theme_classic()

pairs.panels(data[, 2:13],
             method = "pearson",  # correlation method
             hist.col = "grey60", # color of hist. bins
             density = TRUE,      # show density plots
             lm = TRUE) 

str(colnames(data[,17:26]))
for (z in as.vector(data[,2:12])) {
name <- colnames(data[,17:26])
for (y in name) {
  name.mean <- aggregate(z, 
                         by = list(y = data$y), 
                         function(x)c(mean = round(mean(x), 2)))
}

}
health.mean <- aggregate(data[,2:12], 
                        by = list(health_feature = data$Occup_Health), 
                        function(x)c(mean = round(mean(x), 2)))
Occup_Finc.mean <- aggregate(data[,2:12], 
                        by = list(Occup_Finc = data$Occup_Finc), 
                        function(x)c(mean = round(mean(x), 2)))
Occup_Sales.mean <- aggregate(data[,2:12], 
                             by = list(Occup_Sales = data$Occup_Sales), 
                             function(x)c(mean = round(mean(x), 2)))
Occup_Advt.mean <- aggregate(data[,2:12], 
                             by = list(Occup_Advt = data$Occup_Advt), 
                             function(x)c(mean = round(mean(x), 2)))
Occup_Edu.mean <- aggregate(data[,2:12], 
                             by = list(Occup_Edu = data$Occup_Edu), 
                             function(x)c(mean = round(mean(x), 2)))
Occup_Cons.mean <- aggregate(data[,2:12], 
                             by = list(Occup_Cons = data$Occup_Cons), 
                             function(x)c(mean = round(mean(x), 2)))
Occup_Eng.mean <- aggregate(data[,2:12], 
                             by = list(Occup_Eng = data$Occup_Eng), 
                             function(x)c(mean = round(mean(x), 2)))
Occup_Tech.mean <- aggregate(data[,2:12], 
                             by = list(Occup_Tech = data$Occup_Tech), 
                             function(x)c(mean = round(mean(x), 2)))
Occup_Retail.mean <- aggregate(data[,2:12], 
                             by = list(Occup_Retail = data$Occup_Retail), 
                             function(x)c(mean = round(mean(x), 2)))
gender.mean <- aggregate(data[,2:13], 
                             by = list(gender = data$Female), 
                             function(x)c(mean = round(mean(x), 2)))
str(gender.mean)
CompBuy.mean <- aggregate(data[,2:13], 
                          by = list(CompBuy = data$CompBuy), 
                          function(x)c(mean = round(mean(x), 2)))
str(CompBuy.mean)
barplot(as.matrix(CompBuy.mean[,-1]),
        main = "CompBuy vs importance",
        xlab = "importance", ylab = "Frequency",
        col = c("darkgrey", "darkblue"),
        legend.text = CompBuy.mean$CompBuy,
        args.legend = list(title = "compbuy", x = "topright"),
        beside = TRUE) 
barplot(as.matrix(gender.mean[,-1]),
        main = "Importance vs Gender",
        xlab = "Willingness to Pay", ylab = "Frequency",
        col = c("darkgrey", "darkblue"),
        legend.text = c("male","female"),
        args.legend = list(title = "Gender", x = "topright"),
        beside = TRUE) 

iphone <- aggregate(data[,27:34], 
                            by = list(iphone = data$iPhone), 
                            function(x)c(mean = round(mean(x), 2)))
table(phone=data$iPhone,WTP=data$WTP)
data$age_recoded = cut(data$Age, breaks=c(24,35.5,47),include.lowest = TRUE)
age.mean <- aggregate(data[,2:13], 
                          by = list(age.recoded = data$age_recoded), 
                          function(x)c(mean = round(mean(x), 2)))

par(mar=c(15,3,4,2))
barplot(as.matrix(age.mean[,-1]),
        xlab = "Product Attributes", ylab = "Frequency",las = 2,mgp = c(6.5,1,0),ylim = c(0,7),
        col = c("darkgrey", "darkblue"),
        legend.text = age.mean$age.recoded,
        args.legend = list(title = "AGE", x = 38,cex = 0.75, y = 8),
        beside = TRUE) 



data$WTP_new = cut(data$WTP, breaks=c(100,175,250,325,400),include.lowest = TRUE)

mytable <- table(data$Income,data$WTP_new)
sum(mytable)
par(mfrow = c(1,1))
absfre <- barplot(mytable,
        main = "willingness to pay vs Income",
        xlab = "Willingness to Pay", ylab = "Frequency",ylim = c(200,100),
        col = c("darkgrey", "blue", "lightblue","lightgreen","lightyellow"),
        legend.text = c("< $40 000","$40 000 - $70 000","$71 000 - $100 000","$101 000 - $175 000","> $175 000"),
        args.legend = list(title = "Income", x = "topright",cex = 0.75),beside = TRUE,
        names.arg=c("$100-$175","$175-$250","$250-$325","$325-400")) # Grouped bars

?text
relfre <- barplot(prop.table(mytable) * 100,
        main = "willingness to pay vs Income",
        xlab = "Willingness to Pay", ylab = "Frequency",
        col = c("darkgrey", "blue", "lightblue","lightgreen","lightyellow"),
        legend.text = c("< $40 000","$40 000 - $70 000","$71 000 - $100 000","$101 000 - $175 000","> $175 000"),
        args.legend = list(title = "Income", x = "topright"),beside = TRUE) # Grouped bars
text(relfre, mytable + 2.0, labels = prop.table(mytable) * 100, adj = c(0.5,2))
table2 <- table(innovation = data$Imp_Innov,engg = data$Occup_Eng)
table3 <- table(style = data$Imp_Style,gender = data$Female)

#<------------importance variables only------------------------------------------------>>>>>>>>>
data.sc = data[, 2:13]
data.sc = scale(data.sc)
head(data.sc)
summary(data.sc)
#manhattan distance
dist.manhat <- dist(data.sc, method = "manhattan")
as.matrix(dist.manhat)[1:6, 1:6]
?kmeans

#heirarchial clustering with euclidean distance
#euclidean distance
dist.eucl <- dist(data.sc)
as.matrix(dist.eucl)[1:6, 1:6]
cl.single.eucl <- hclust(dist.eucl, method = "single")     # single linkage method
cl.complete.eucl <- hclust(dist.eucl, method = "complete") # complete linkage method
cl.average.eucl <- hclust(dist.eucl, method = "average")   # average linkage method
cl.centroid.eucl <- hclust(dist.eucl, method = "centroid") # centroid linkage method
cl.median.eucl <- hclust(dist.eucl, method = "median")     # median linkage method
cl.ward.eucl <- hclust(dist.eucl, method = "ward.D2")      


#plot single linkage
plot(as.dendrogram(cl.single.eucl), ylim = c(0, 3),leaflab = "none") #chain
rect.hclust(cl.single.eucl, k = 3, border = "darkred")
table(cutree(cl.single.eucl, 3))
#complete linkage
plot(as.dendrogram(cl.complete.eucl))
rect.hclust(cl.complete.eucl, k = 3, border = "darkred")
table(cutree(cl.complete.eucl, 3))
#average linkage
plot(as.dendrogram(cl.average.eucl)) #chain
rect.hclust(cl.average.eucl, k = 3, border = "darkred")
table(cutree(cl.average.eucl, 10))
#centroid linkage
plot(as.dendrogram(cl.centroid.eucl)) #chain
rect.hclust(cl.centroid.eucl, k = 3, border = "darkred")
table(cutree(cl.centroid.eucl, 20))
#median linkage
plot(as.dendrogram(cl.median.eucl)) #chain
rect.hclust(cl.median.eucl, k = 3, border = "darkred")
table(cutree(cl.median.eucl, 20))
#wards method
plot(as.dendrogram(cl.ward.eucl))
rect.hclust(cl.ward.eucl, k = 3, border = "darkred")
table(cutree(cl.ward.eucl, 4))
#VRC - Variance ratio criterion or Calinski-Harabasz (CH) index ---------------------
# we will use fpc package
VRC.ward = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward[k] <- cluster.stats(d = dist.eucl, 
                               clustering = cutree(cl.ward.eucl, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward

VRC.complete = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.complete[k] <- cluster.stats(d = dist.eucl, 
                                   clustering = cutree(cl.complete.eucl, k))$ch
}
VRC.complete = VRC.complete[-1]

# save as a data frame
VRC = data.frame(K = 2:10, complete = VRC.complete, ward = VRC.ward)

# reshape to long
VRC = melt(VRC, id.vars = "K")

# plot
ggplot(VRC, aes(x = K, y = value)) +
  ggtitle("Importance variables with euclidean distance")+
  geom_point() + geom_line() +
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()

par(mfrow = c(1,1))
#heirarchial clustering with manhattan distance
cl.single.manhat <- hclust(dist.manhat, method = "single")     # single linkage method
cl.complete.manhat <- hclust(dist.manhat, method = "complete") # complete linkage method
cl.average.manhat <- hclust(dist.manhat, method = "average")   # average linkage method
cl.centroid.manhat <- hclust(dist.manhat, method = "centroid") # centroid linkage method
cl.median.manhat <- hclust(dist.manhat, method = "median")     # median linkage method
cl.ward.manhat <- hclust(dist.manhat, method = "ward.D2")   
#plot single linkage
plot(as.dendrogram(cl.single.manhat), ylim = c(0, 10),leaflab = "none") #chain
rect.hclust(cl.single.manhat, k = 3, border = "darkred")
table(cutree(cl.single.manhat, 3))
#complete linkage
plot(as.dendrogram(cl.complete.manhat))
rect.hclust(cl.complete.manhat, k = 3, border = "darkred")
table(cutree(cl.complete.manhat, 3))
#average linkage
plot(as.dendrogram(cl.average.manhat)) #chain
rect.hclust(cl.average.manhat, k = 4, border = "darkred")
table(cutree(cl.average.manhat, 10))
#centroid linkage
plot(as.dendrogram(cl.centroid.manhat)) #chain
rect.hclust(cl.centroid.manhat, k = 3, border = "darkred")
table(cutree(cl.centroid.manhat, 20))
#median linkage
plot(as.dendrogram(cl.median.manhat)) #chain
rect.hclust(cl.median.manhat, k = 3, border = "darkred")
table(cutree(cl.median.manhat, 20))
#wards method
plot(as.dendrogram(cl.ward.manhat))
rect.hclust(cl.ward.manhat, k = 3, border = "darkred")
table(cutree(cl.ward.manhat, 2))

VRC.ward.manhat = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward.manhat[k] <- cluster.stats(d = dist.manhat, 
                               clustering = cutree(cl.ward.manhat, k))$ch
}
VRC.ward.manhat = VRC.ward.manhat[-1]
VRC.ward.manhat

VRC.complete.manhat = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.complete.manhat[k] <- cluster.stats(d = dist.manhat, 
                                   clustering = cutree(cl.complete.manhat, k))$ch
}
VRC.complete.manhat = VRC.complete.manhat[-1]

# save as a data frame
VRC.manhat = data.frame(K = 2:10, complete = VRC.complete.manhat, ward = VRC.ward.manhat)

# reshape to long
VRC.manhat = melt(VRC.manhat, id.vars = "K")

# plot
ggplot(VRC.manhat, aes(x = K, y = value)) +
  ggtitle("Importance variables with manhattan distance")+
  geom_point() + geom_line() +
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()

# Input for k-means are the original variables, not distance matrix. 

# As the k-means initial partition is random, fix the seed for reproducability
set.seed(185) 
#3 cluster k means
cl.kmeans.3 <- kmeans(data.sc, centers = 3)

str(cl.kmeans.3)

# cluster assignments
table(cl.kmeans.3$cluster)

# combine with the original data
data$cluster_kmeans_3 <- cl.kmeans.3$cluster
str(data)

clust.kmean.3 <- aggregate(data[, -c(1,40,41)], 
                         by = list(cluster = data$cluster_kmeans_3), 
                         function(x)c(mean = round(mean(x), 2)))

str(clust.kmean.3)
#4 cluster k means
cl.kmeans.4 <- kmeans(data.sc, centers = 4)

str(cl.kmeans.4)

# cluster assignments
table(cl.kmeans.4$cluster)

# combine with the original data
data$cluster_kmeans_4 <- cl.kmeans.4$cluster
str(data)

cl.kmean.4 <- aggregate(data[, -c(1,40,41)], 
                           by = list(cluster = data$cluster_kmeans_4), 
                           function(x)c(mean = round(mean(x), 2)))

str(cl.kmean.4)
#plot

par(mfrow = c(2,3))
clusplot(data.sc, cutree(cl.complete.eucl, 3), color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "complete linkage 3 cluster plot")
clusplot(data.sc, cutree(cl.ward.eucl, 3), color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "Ward's 3 cluster plot")
clusplot(data.sc, cl.kmeans.3$cluster, color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "K-means 3 cluster plot")
clusplot(data.sc, cutree(cl.complete.eucl, 4), color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "complete linkage 4 cluster plot")

clusplot(data.sc, cutree(cl.ward.eucl, 4), color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "Ward's 4 cluster plot")

clusplot(data.sc, cl.kmeans.4$cluster, color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "K-means 4 cluster plot")





#<-----Importance variables+WTP------------->>>>>>
data.sc.2 = scale(data[, 2:14])
#euclidean distance
dist.eucl.2 <- dist(data.sc.2,method = "euclidean")
dist.manhat.2 <- dist(data.sc.2, method = "manhattan")
as.matrix(dist.manhat.2)[1:6, 1:6]
cl.single.eucl <- hclust(dist.eucl.2, method = "single")     # single linkage method
cl.complete.eucl <- hclust(dist.eucl.2, method = "complete") # complete linkage method
cl.average.eucl <- hclust(dist.eucl.2, method = "average")   # average linkage method
cl.centroid.eucl <- hclust(dist.eucl.2, method = "centroid") # centroid linkage method
cl.median.eucl <- hclust(dist.eucl.2, method = "median")     # median linkage method
cl.ward.eucl <- hclust(dist.eucl.2, method = "ward.D2")      


#plot single linkage
plot(as.dendrogram(cl.single.eucl), ylim = c(0, 3),leaflab = "none") #chain
rect.hclust(cl.single.eucl, k = 3, border = "darkred")
table(cutree(cl.single.eucl, 3))
#complete linkage
plot(as.dendrogram(cl.complete.eucl))
rect.hclust(cl.complete.eucl, k = 3, border = "darkred")
table(cutree(cl.complete.eucl, 3))
#average linkage
plot(as.dendrogram(cl.average.eucl)) #chain
rect.hclust(cl.average.eucl, k = 3, border = "darkred")
table(cutree(cl.average.eucl, 10))
#centroid linkage
plot(as.dendrogram(cl.centroid.eucl)) #chain
rect.hclust(cl.centroid.eucl, k = 3, border = "darkred")
table(cutree(cl.centroid.eucl, 20))
#median linkage
plot(as.dendrogram(cl.median.eucl)) #chain
rect.hclust(cl.median.eucl, k = 3, border = "darkred")
table(cutree(cl.median.eucl, 20))
#wards method
plot(as.dendrogram(cl.ward.eucl))
rect.hclust(cl.ward.eucl, k = 3, border = "darkred")
table(cutree(cl.ward.eucl, 4))
#<<,,,--- describe data----------------------------------------------->>>>>
fresh$cluster <- cutree(cl.ward.eucl, 4)
clust.mean <- aggregate(fresh[, -c(1,39)], 
                        by = list(cluster = fresh$cluster), 
                        function(x)c(mean = round(mean(x), 2)))
t(clust.mean)
data$cluster.comp.eucl <- cutree(cl.complete.eucl, 4)
clust.mean.compl.eucl <- aggregate(data[, -c(1,39,40,41,42)], 
                        by = list(cluster = data$cluster.comp.eucl), 
                        function(x)c(mean = round(mean(x), 2)))
t(clust.mean.compl.eucl)
write.table(t(clust.mean.ward), file = "Importance+WTP clustering complete euclidean.txt",sep = ",", row.names = TRUE, col.names = TRUE)

#VRC - Variance ratio criterion or Calinski-Harabasz (CH) index ---------------------
# we will use fpc package
VRC.ward = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward[k] <- cluster.stats(d = dist.eucl.2, 
                               clustering = cutree(cl.ward.eucl, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward

VRC.complete = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.complete[k] <- cluster.stats(d = dist.eucl.2, 
                                   clustering = cutree(cl.complete.eucl, k))$ch
}
VRC.complete = VRC.complete[-1]

# save as a data frame
VRC = data.frame(K = 2:10, complete = VRC.complete, ward = VRC.ward)

# reshape to long
VRC = melt(VRC, id.vars = "K")

# plot
ggplot(VRC, aes(x = K, y = value)) +
  ggtitle("Importance variables+WTP with euclidean distance")+
  geom_point() + geom_line() +
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()

par(mfrow = c(1,1))
#heirarchial clustering with manhattan distance
cl.single.manhat <- hclust(dist.manhat.2, method = "single")     # single linkage method
cl.complete.manhat <- hclust(dist.manhat.2, method = "complete") # complete linkage method
cl.average.manhat <- hclust(dist.manhat.2, method = "average")   # average linkage method
cl.centroid.manhat <- hclust(dist.manhat.2, method = "centroid") # centroid linkage method
cl.median.manhat <- hclust(dist.manhat.2, method = "median")     # median linkage method
cl.ward.manhat <- hclust(dist.manhat.2, method = "ward.D2")   
#plot single linkage
plot(as.dendrogram(cl.single.manhat), ylim = c(0, 10),leaflab = "none") #chain
rect.hclust(cl.single.manhat, k = 3, border = "darkred")
table(cutree(cl.single.manhat, 3))
#complete linkage
plot(as.dendrogram(cl.complete.manhat))
rect.hclust(cl.complete.manhat, k = 3, border = "darkred")
table(cutree(cl.complete.manhat, 3))
#average linkage
plot(as.dendrogram(cl.average.manhat)) #chain
rect.hclust(cl.average.manhat, k = 4, border = "darkred")
table(cutree(cl.average.manhat, 10))
#centroid linkage
plot(as.dendrogram(cl.centroid.manhat)) #chain
rect.hclust(cl.centroid.manhat, k = 3, border = "darkred")
table(cutree(cl.centroid.manhat, 20))
#median linkage
plot(as.dendrogram(cl.median.manhat)) #chain
rect.hclust(cl.median.manhat, k = 3, border = "darkred")
table(cutree(cl.median.manhat, 20))
#wards method
plot(as.dendrogram(cl.ward.manhat))
rect.hclust(cl.ward.manhat, k = 3, border = "darkred")
table(cutree(cl.ward.manhat, 2))
data$cluster.ward <- cutree(cl.ward.eucl, 4)
clust.mean.ward <- aggregate(data[, -c(1,39)], 
                             by = list(cluster = data$cluster.ward), 
                             function(x)c(mean = round(mean(x), 2)))

VRC.ward.manhat = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward.manhat[k] <- cluster.stats(d = dist.manhat.2, 
                                      clustering = cutree(cl.ward.manhat, k))$ch
}
VRC.ward.manhat = VRC.ward.manhat[-1]
VRC.ward.manhat

VRC.complete.manhat = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.complete.manhat[k] <- cluster.stats(d = dist.manhat.2, 
                                          clustering = cutree(cl.complete.manhat, k))$ch
}
VRC.complete.manhat = VRC.complete.manhat[-1]

# save as a data frame
VRC.manhat = data.frame(K = 2:10, complete = VRC.complete.manhat, ward = VRC.ward.manhat)

# reshape to long
VRC.manhat = melt(VRC.manhat, id.vars = "K")

# plot
ggplot(VRC.manhat, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  ggtitle("Importance variables+WTP with manhattan distance")+
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()

# Input for k-means are the original variables, not distance matrix. 

# As the k-means initial partition is random, fix the seed for reproducability
set.seed(185) 
#3 cluster k means
cl.kmeans.3 <- kmeans(data.sc.2, centers = 3)

str(cl.kmeans.3)

# cluster assignments
table(cl.kmeans.3$cluster)

# combine with the original data
data$cluster_kmeans_3 <- cl.kmeans.3$cluster
str(data)

clust.kmean.3 <- aggregate(data[, -c(1,40,41)], 
                           by = list(cluster = data$cluster_kmeans_3), 
                           function(x)c(mean = round(mean(x), 2)))

str(clust.kmean.3)
fresh$cluster_kmeans <- cl.kmeans.3$cluster
clust.kmean <- aggregate(fresh[, -c(1,39,40)], 
                        by = list(cluster = fresh$cluster_kmeans), 
                        function(x)c(mean = round(mean(x), 2)))
clust.kmean

#4 cluster k means
cl.kmeans.4 <- kmeans(data.sc.2, centers = 4)

str(cl.kmeans.4)

# cluster assignments
table(cl.kmeans.4$cluster)

# combine with the original data
data$cluster_kmeans_4 <- cl.kmeans.4$cluster
str(data)

cl.kmean.4 <- aggregate(data[, -c(1,40,41)], 
                        by = list(cluster = data$cluster_kmeans_4), 
                        function(x)c(mean = round(mean(x), 2)))
t(cl.kmean.4)

str(cl.kmean.4)

fresh$cluster_kmeans <- cl.kmeans.4$cluster
clust.kmean <- aggregate(fresh[, -c(1,39,40)], 
                         by = list(cluster = fresh$cluster_kmeans), 
                         function(x)c(mean = round(mean(x), 2)))
clust.kmean
#plot

head(data.sc.2)
str(data)
table(cutree(cl.ward.manhat, 4))
data$cluster.ward.manhat <- cutree(cl.ward.manhat, 4)
clust.mean.ward.manhat <- aggregate(data[, -c(1,40,41,39)], 
                             by = list(cluster = data$cluster.ward.manhat), 
                             function(x)c(mean = round(mean(x), 2)))
t(clust.mean.ward.manhat)
write.table(t(clust.mean.ward.manhat), file = "Importance+WTP clustering manhattan.txt",sep = ",", row.names = TRUE, col.names = TRUE)

par(mfrow = c(2,2))
clusplot(data.sc.2, cutree(cl.complete.manhat, 3), color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "complete linkage 3 cluster plot")
clusplot(data.sc.2, cutree(cl.ward.manhat, 3), color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "Ward's 3 cluster plot")
clusplot(data.sc.2, cutree(cl.complete.manhat, 4), color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "complete linkage 4 cluster plot")

clusplot(data.sc.2, cutree(cl.ward.manhat, 4), color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "Ward's 4 cluster plot")

clusplot(data.sc.2, cl.kmeans.3$cluster, color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "K-means 3 cluster plot")

clusplot(data.sc.2, cl.kmeans.4$cluster, color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "K-means 4 cluster plot")


#<<<<<<<<<<<<-----------All the data for cluster analysis---------------->>>>>>
set.seed(185)
new_data <- data[,-c(1,39,40,41)]
head(new_data)
str(new_data[,c(14:33,35,36,37)])
new_data$Degree <- ifelse(new_data$Degree == 1,0,1)
str(new_data)
diss.gower = daisy(new_data, metric = "gower",type = list(symm = c(14:33,35),asymm = 36))
typeof(diss.gower)
as.matrix(diss.gower)[1:6, 1:6]
#k-medoids for all data
cl.medoids <- pam(x = diss.gower, k = 3, diss = TRUE)
cl.medoids.4 <- pam(x = diss.gower, k = 4, diss = TRUE)
cl.medoids.2 <- pam(x = diss.gower, k = 2, diss = TRUE)
new_data$cl.medoids.4 <- cl.medoids.4$clustering
new_data$cl.medoids.2 <- cl.medoids$clustering
table(cl.medoids.4$clustering)
table(cl.medoids.2$clustering)
cl.medoids$clustering
summary(cl.medoids)
new_data$cluster.medoids <- cl.medoids$clustering
fviz_nbclust(data[,-c(39,40,41)], pam, method = "wss")
gap_stat <- clusGap(data[,-c(39,40,41)],
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
fviz_cluster(cl.medoids, data = new_data)
x <- as.vector(data[,-c(39,40,41)])
y <- as.vector(cl.medoids)

cl.kmedoid <- aggregate(new_data[, -c(38)], 
                        by = list(cluster = new_data$cluster.medoids), 
                        function(x)c(mean = round(mean(x), 2)))
table(new_data$cluster.medoids)

#heirarchical clustering methods
par(mfrow = c(1,1))
cl.single.gower <- hclust(diss.gower, method = "single")     # single linkage method
cl.complete.gower <- hclust(diss.gower, method = "complete") # complete linkage method
cl.average.gower <- hclust(diss.gower, method = "average")   # average linkage method
cl.centroid.gower <- hclust(diss.gower, method = "centroid") # centroid linkage method
cl.median.gower <- hclust(diss.gower, method = "median")     # median linkage method
cl.ward.gower <- hclust(diss.gower, method = "ward.D2")   
#plot single linkage
plot(as.dendrogram(cl.single.gower),leaflab = "none") #chain
rect.hclust(cl.single.gower, k = 3, border = "darkred")
table(cutree(cl.single.gower, 3))
#complete linkage
plot(as.dendrogram(cl.complete.gower))
rect.hclust(cl.complete.gower, k = 3, border = "darkred")
table(cutree(cl.complete.gower, 3))
#average linkage
plot(as.dendrogram(cl.average.gower)) 
rect.hclust(cl.average.gower, k = 4, border = "darkred")
table(cutree(cl.average.gower, 4))
#centroid linkage
plot(as.dendrogram(cl.centroid.gower)) #chain
rect.hclust(cl.centroid.gower, k = 3, border = "darkred")
table(cutree(cl.centroid.gower, 20))
#median linkage
plot(as.dendrogram(cl.median.gower)) #chain
rect.hclust(cl.median.gower, k = 3, border = "darkred")
table(cutree(cl.median.gower, 20))
#wards method
plot(as.dendrogram(cl.ward.gower))
rect.hclust(cl.ward.gower, k = 3, border = "darkred")
table(cutree(cl.ward.gower, 2))
new_data$cluster_ward <- cutree(cl.ward.gower, 4)
clust.ward.all.data <- aggregate(fresh[, -c(39,40,1)], 
                         by = list(cluster = new_data$cluster_ward), 
                         function(x)c(mean = round(mean(x), 2)))
clust.ward.all.data
 t(clust.ward.all.data)

write.table(t(clust.ward.all.data), file = "all variables clustering ward.txt",sep = ",", row.names = TRUE, col.names = TRUE)
VRC.ward.gower = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward.gower[k] <- cluster.stats(d = diss.gower, 
                                      clustering = cutree(cl.ward.gower, k))$ch
}
VRC.ward.gower = VRC.ward.gower[-1]
VRC.ward.gower

VRC.complete.gower = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.complete.gower[k] <- cluster.stats(d = diss.gower, 
                                          clustering = cutree(cl.complete.gower, k))$ch
}
VRC.complete.gower = VRC.complete.gower[-1]

# save as a data frame
VRC.gower = data.frame(K = 2:10, complete = VRC.complete.gower, ward = VRC.ward.gower)

# reshape to long
VRC.gower = melt(VRC.gower, id.vars = "K")

# plot
ggplot(VRC.gower, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  ggtitle("all variables gower distance")+
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()
?daisy
par(mfrow = c(2,2))
x<-as.matrix(diss.gower)
clusplot(x, cutree(cl.complete.gower, 3), color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "complete linkage 3 cluster plot")
clusplot(x, cutree(cl.ward.gower, 3), color = TRUE , shade = TRUE ,
         labels = 3, lines = 0, main = "Ward's 3 cluster plot")
clusplot(x, cutree(cl.complete.gower, 4), color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "complete linkage 4 cluster plot")

clusplot(x, cutree(cl.ward.gower, 4), color = TRUE , shade = TRUE ,
         labels = 4, lines = 0, main = "Ward's 4 cluster plot")



# Classification Methods =======================================================
# Split the data into training and test sets -----------------------------------
set.seed(04625)   # fix the seed for reproducability
train.pop <- 0.60 # we will use 65-35% split
str(fresh)
N <- nrow(fresh) # total sample size

# randomly sample 60\% of observations
train.cases <- sample(N, N*train.pop)

# assign the randomly sampled 60% of obs. to training dataset
fresh.train <- fresh[train.cases, ] 
nrow(fresh.train)

# assign the rest to test dataset
fresh.test <- fresh[-train.cases, ]
nrow(fresh.test)
#<<<--- all demographic variables exluding iphone,compbuy,amaznp, media use------------->>>>
#<<<<<<----- ward cluster---------------->>>>>>>>>>>>>>>
# Step 2: Train the prediction model ===========================================
# Multinomial logistic regression ----------------------------------------------
# Unique known segments
fresh.demo <- fresh[,15:38]
unique(fresh$cluster)
# last level will be treated as reference

logistic <- multinom(cluster ~ Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                       Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income, data = fresh.train)
summary(logistic)

logistic_kmeans <- multinom(cluster_kmeans ~ Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                       Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income, data = fresh.train)
summary(logistic_kmeans)
# segment allocation
fresh.test$seg_log_kmeans <- predict(logistic_kmeans, newdata = fresh.test)
head(fresh.test)
# How well did the model predict?
mean(fresh.test$cluster_kmeans == fresh.test$seg_log_kmeans)*100
# 60.25 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_log_kmeans, fresh.test$cluster_kmeans)*100
# 30.11036%
# Coefficents
t(round(summary(logistic)$coefficients, 2))

# standard errors
t(round(summary(logistic)$standard.errors, 3))


# Does not include p-value calculation for the regression coefficients, 
# so we calculate p-values using Wald tests (here z-tests).
z <- summary(logistic)$coefficients/summary(logistic)$standard.errors

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(t(p), 3)


# Compute odds ratio
x = exp(summary(logistic)$coefficients)

round(t(x), 2)
round(x[3, -1], 2)


# Predicted probabilities (fitted values)
pp <- fitted(logistic)
head(round(pp * 100, 2), 10)


# Now let's predict for the test data
# Probabilities of belonging to a segment
round(predict(logistic, newdata = fresh.test, "probs") * 100, 2)

# segment allocation
fresh.test$seg_log <- predict(logistic, newdata = fresh.test)
head(fresh.test)

# How well did the model predict?
mean(fresh.test$cluster == fresh.test$seg_log)*100
# 65.25 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_log, fresh.test$cluster)*100
# 31.49755%

# Cross.table: predicted vs observed segments
table(fresh.test$seg_log, fresh.test$cluster)
# rows are the predicted shares

# Naive Bayes ------------------------------------------------------------------
nb <- naiveBayes(cluster ~  Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                   Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income, data = fresh.train)
(nb)

nb_kmeans <- naiveBayes(cluster_kmeans ~  Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                   Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income, data = fresh.train)
(nb_kmeans)
# predict segment allocation for test data
fresh.test$seg_nb_kmeans <- predict(nb_kmeans, fresh.test)
head(fresh.test)
# How good did the model do?
mean(fresh.test$cluster_kmeans == fresh.test$seg_nb_kmeans)*100
# 49.5 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_nb_kmeans, fresh.test$cluster_kmeans)*100
# 12.67166%
# predict segment allocation for test data
fresh.test$seg_nb <- predict(nb, fresh.test)
head(fresh.test)

# to get the probabilities
head(round(predict(nb, fresh.test, type = "raw")*100, 2), 10)

# How good did the model do?
mean(fresh.test$cluster == fresh.test$seg_nb)*100
# 48.25 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_nb, fresh.test$cluster)*100
# 9.793256%

# Random Forest ----------------------------------------------------------------
# redefine the Segment variable as a factor
# random forest will need y to be of factor not character class
fresh.train$cluster <- factor(fresh.train$cluster)
fresh.train$cluster_kmeans <- factor(fresh.train$cluster_kmeans)
set.seed(98040)
head(fresh.train)

rf <- randomForest(cluster ~ Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                     Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income, data = fresh.train, ntree = 3000)
(rf)
rf_kmeans <- randomForest(cluster_kmeans ~ Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                     Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+Age+Female+Degree+Income, data = fresh.train, ntree = 3000)
(rf_kmeans)
# predict segment allocation for test data
fresh.test$seg_rf_kmeans <- predict(rf_kmeans, fresh.test)
head(fresh.test)
table(fresh$cluster_kmeans)
# How good did the model do?
mean(fresh.test$cluster_kmeans == fresh.test$seg_rf_kmeans)*100
#63.5 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_rf_kmeans, fresh.test$cluster_kmeans)*100
# 36.08486%
# predict segment allocation for test data
fresh.test$seg_rf <- predict(rf, fresh.test)
head(fresh.test)

# How good did the model do?
mean(fresh.test$cluster == fresh.test$seg_rf)*100
#70.5 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_rf, fresh.test$cluster)*100
# 45.03947%

# Comparison of methods --------------------------------------------------------
par(mfrow = c(1,3))
clusplot(fresh.test[, 1:6], fresh.test$seg_log, 
         color = TRUE, shade = TRUE,
         labels = 4, lines = 0, 
         main = "Logistic Regression classification")

clusplot(fresh.test[, 1:6], fresh.test$seg_nb, 
         color = TRUE, shade = TRUE,
         labels = 4, lines = 0, 
         main = "Naive Bayes classification")

clusplot(fresh.test[, 1:6], fresh.test$seg_rf, 
         color = TRUE, shade = TRUE,
         labels = 4, lines = 0, 
         main = "Random Forest classification")



# Split the data into training and test sets -----------------------------------
set.seed(04625)   # fix the seed for reproducability
train.pop <- 0.60 # we will use 65-35% split
str(fresh)
N <- nrow(fresh) # total sample size

# randomly sample 60\% of observations
train.cases <- sample(N, N*train.pop)

# assign the randomly sampled 60% of obs. to training dataset
fresh.train <- fresh[train.cases, ] 
nrow(fresh.train)

# assign the rest to test dataset
fresh.test <- fresh[-train.cases, ]
nrow(fresh.test)
#<<<--- all demographic variables exluding iphone,compbuy,amaznp, media use------------->>>>
#<<<<<<----- ward cluster---------------->>>>>>>>>>>>>>>
# Step 2: Train the prediction model ===========================================
# Multinomial logistic regression ----------------------------------------------
# Unique known segments
unique(fresh$cluster)
# last level will be treated as reference

logistic <- multinom(cluster ~ iPhone+CompBuy+Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                       Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+FB_Insta+Twit+Snap+YouTube+Pod_radio+TV+NewsP+AmznP+
                       Age+Female+Degree+Income, data = fresh.train)
summary(logistic)

logistic_kmeans <- multinom(cluster_kmeans ~ iPhone+CompBuy+Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                              Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+FB_Insta+Twit+Snap+YouTube+Pod_radio+TV+NewsP+AmznP+
                              Age+Female+Degree+Income, data = fresh.train)
summary(logistic_kmeans)
# segment allocation
fresh.test$seg_log_kmeans <- predict(logistic_kmeans, newdata = fresh.test)
head(fresh.test)
# How well did the model predict?
mean(fresh.test$cluster_kmeans == fresh.test$seg_log_kmeans)*100
# 65.75 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_log_kmeans, fresh.test$cluster_kmeans)*100
# 38.02605%

# segment allocation
fresh.test$seg_log <- predict(logistic, newdata = fresh.test)
head(fresh.test)

# How well did the model predict?
mean(fresh.test$cluster == fresh.test$seg_log)*100
# 79.25 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_log, fresh.test$cluster)*100
# 53.06052%

# Cross.table: predicted vs observed segments
table(fresh.test$seg_log, fresh.test$cluster)
# rows are the predicted shares

# Naive Bayes ------------------------------------------------------------------
nb <- naiveBayes(cluster ~  iPhone+CompBuy+Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                   Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+FB_Insta+Twit+Snap+YouTube+Pod_radio+TV+NewsP+AmznP+
                   Age+Female+Degree+Income, data = fresh.train)
(nb)

nb_kmeans <- naiveBayes(cluster_kmeans ~  iPhone+CompBuy+Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                          Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+FB_Insta+Twit+Snap+YouTube+Pod_radio+TV+NewsP+AmznP+
                          Age+Female+Degree+Income, data = fresh.train)
(nb_kmeans)
# predict segment allocation for test data
fresh.test$seg_nb_kmeans <- predict(nb_kmeans, fresh.test)
head(fresh.test)
# How good did the model do?
mean(fresh.test$cluster_kmeans == fresh.test$seg_nb_kmeans)*100
# 56 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_nb_kmeans, fresh.test$cluster_kmeans)*100
# 22.76335%
# predict segment allocation for test data
fresh.test$seg_nb <- predict(nb, fresh.test)
head(fresh.test)

# to get the probabilities
head(round(predict(nb, fresh.test, type = "raw")*100, 2), 10)

# How good did the model do?
mean(fresh.test$cluster == fresh.test$seg_nb)*100
# 64 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_nb, fresh.test$cluster)*100
# 26.84288%

# Random Forest ----------------------------------------------------------------
# redefine the Segment variable as a factor
# random forest will need y to be of factor not character class
fresh.train$cluster <- factor(fresh.train$cluster)
fresh.train$cluster_kmeans <- factor(fresh.train$cluster_kmeans)
set.seed(98040)
head(fresh.train)

rf <- randomForest(cluster ~ iPhone+CompBuy+Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                     Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+FB_Insta+Twit+Snap+YouTube+Pod_radio+TV+NewsP+AmznP+
                     Age+Female+Degree+Income, data = fresh.train, ntree = 3000)
(rf)
rf_kmeans <- randomForest(cluster_kmeans ~ iPhone+CompBuy+Occup_Health+Occup_Finc+Occup_Sales+Occup_Advt+Occup_Edu+Occup_Cons+
                            Occup_Eng+Occup_Tech+Occup_Retail+Occup_SMB+FB_Insta+Twit+Snap+YouTube+Pod_radio+TV+NewsP+AmznP+
                            Age+Female+Degree+Income, data = fresh.train, ntree = 3000)
(rf_kmeans)
# predict segment allocation for test data
fresh.test$seg_rf_kmeans <- predict(rf_kmeans, fresh.test)
head(fresh.test)
table(fresh$cluster_kmeans)
# How good did the model do?
mean(fresh.test$cluster_kmeans == fresh.test$seg_rf_kmeans)*100
#66 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_rf_kmeans, fresh.test$cluster_kmeans)*100
# 42.74012%
# predict segment allocation for test data
fresh.test$seg_rf <- predict(rf, fresh.test)
head(fresh.test)

# How good did the model do?
mean(fresh.test$cluster == fresh.test$seg_rf)*100
#79.5 agreement between predicted and actual segment membership

# vs. chance (adjusted for base rate)
adjustedRandIndex(fresh.test$seg_rf, fresh.test$cluster)*100
# 53.49521%
