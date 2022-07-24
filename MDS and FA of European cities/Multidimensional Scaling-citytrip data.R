setwd("C:/Users/ASUS/Desktop/CACI")
getwd()
install.packages("zoo")
library("zoo")
pacman::p_load(MASS, smacof, ggrepel, ggforce,psych, coefplot, corrplot,reshape2, ggplot2, dplyr, stringr,scatterplot3d)
citytrip_data <- read.csv("QuestionaireData_CityTrips.csv")
names(citytrip_data)
dim(citytrip_data)
str(citytrip_data)
citytrip_data$id_unique <- 1:nrow(citytrip_data)
head(citytrip_data)
str(citytrip_data)
names(citytrip_data)



data.long <- citytrip_data %>% 
  select(c("Sample", "ID", "id_unique"), # we still need the respondent index
         contains("_Att"),  # any column that contains _Att in the name
         starts_with("Pref_")) # any column that starts with Pref_              

head(data.long)
dim(data.long) # 266 402
?melt
# Reshape to long format
data.long <- melt(data.long, id.vars = c("Sample", "ID", "id_unique"))
head(data.long)
dim(data.long)

# Let's delete the NAs
data.long <- data.long[!is.na(data.long$value), ]
dim(data.long)


# Now we have both attribute evaluations of each city and preference rating stacked
# under each other
# Let's create a variable that differentiates whether it is attribute eval.
# or preference rating
data.long$type <- ifelse(str_detect(data.long$variable, "_Att"), "Attribute", "Pref")
head(data.long)


# Now we want to create a column City which is the 1st part of the variable
# string (before _) if it's type = "Attribute" and 2nd part (after _) if
# it's type = "Pref"
# For this we will use a useful function strsplit(), which 
# splits a string by the provided delimiter (here, underscore _)
# Let's run this part separately for the 1st row (observation)
strsplit(data.long[1, ]$variable, "_") # not working as variable is a factor
str(data.long)

# so we need to convert it to a character class and then apply strsplit()
strsplit(as.character(data.long[1, ]$variable), "_")

# Now we want to select only the 1st element. 
# We can do this using sapply()
sapply(strsplit(as.character(data.long[1, ]$variable), "_"), `[`, 1)

# if we wanted to choose the 2nd element, then
sapply(strsplit(as.character(data.long[1, ]$variable), "_"), `[`, 2)

# Ok, now let's apply it to each row and use ifelse() by column type
data.long$City <- ifelse(data.long$type == "Attribute",
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 1),
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2))
head(data.long)
unique(data.long$City)

# Now let's create column Attribute, which is the index for attributes now contained
# in column variable if type = "Attribute" and let's set the value to "Pref" if type = "Pref
data.long$Attribute <- ifelse(data.long$type == "Attribute",
                              sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2),
                              "Pref")
head(data.long)
unique(data.long$Attribute)

# Attribute column is a character. Let's save it as a factor and
# assign meaningful labels from the documentation file
# Define the vector of labels
attLabels <- c("friendly", "historical", "affordable", "trendy",
               "vibrant nightlife", "delicious food", "easy-to-get-around",
               "good shopping", "cultural events", "interesting museums",
               "clean", "green", "international", "too touristic",
               "fun", "noisy", "romantic", "safe", "beautiful", 
               "english-speaker-friendly")
length(attLabels) # should be 20


# Set Attribute variable as a factor, define levels from 1 to 20, and 
# define the labels 
data.long$Attribute <- factor(data.long$Attribute, levels = c(paste0("Att", 1:20), "Pref"),
                              labels = c(attLabels, "Pref"))
head(data.long)

# Let's reshape to wide format 
# so that we have one column for each attribute and one column for Pref
data.long <- dcast(data.long, 
                   Sample + ID + id_unique + City ~ Attribute, 
                   value.var = "value")
head(data.long)
str(data.long)

dim(data.long) # 1590   25

# Are these correct dimensions?
# Each respondent was supposed to evaluate 6 cities
length(unique(data.long$id_unique)) # 266
266 * 6 # 1596 

# How come?
# There are still NAs in the data, these are the real missing values:
# item non-response: respondents didn't give an answer to this questions.

# We can save this data frame separately as a csv and load and work with it later
write.csv(data.long, file = "data_long.csv", row.names = FALSE)
citydata <- read.csv("data_long.csv")
dim(citydata)
str(citydata)
names(citydata)
summary(citydata)

#mean values 
citydata_refined <- na.aggregate(citydata[,-c(1,2,3,4)])        
citydata_refined <- cbind(citydata[,c(1,2,3,4)],citydata_refined)
summary(citydata_refined)
citydata_std<-cbind(citydata[,c(1,2,3,4)],apply(citydata_refined[,-c(1,2,3,4)],2,scale))
summary(citydata_std)
head(citydata_std)
str(citydata_std)

attrEval_long <- melt(citydata_std, id.vars = c("Sample", "ID", "id_unique","City"),
                      variable.name = "attribute")
head(attrEval_long,10)


attrEval_long_2 <- melt(citydata_refined, id.vars = c("Sample", "ID", "id_unique","City"),
                      variable.name = "attribute")
head(attrEval_long_2,10)
str(attrEval_long_2)

attrMean_2 <- aggregate(attrEval_long_2[, "value"], 
                      by = list(attribute = attrEval_long_2$attribute), 
                      FUN = mean)
attrMean_2
any(is.na(attrEval_long_2))




ggplot(data = attrEval_long_2, aes(y = City, x = value)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", color = "darkred")+
  labs(y = "distribution of rating")+
  theme_classic()+
  # Make a panel grouped by attribute
  facet_wrap(attribute~.) 
# Attribute evals across channels  
ggplot(data = attrEval_long_2, aes(y = City, x = value)) +
  geom_bar(stat = "summary", fun = "mean") + # to plot the mean
  geom_vline(xintercept = 3, linetype = "dashed") +
  labs(x = "", y = "") +
  theme_bw()+
  # Make a panel grouped by attribute
  facet_wrap(attribute~.) 
str(citydata_refined)

ggplot(data = citydata_std, aes(y = historical, x = City)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", color = "darkred")+
  labs(y = "distribution of rating")+
  theme_classic()




# Attribute evals of each channel
ggplot(data = attrEval_long_2, aes(y = attribute, x = value)) +
  geom_bar(stat = "summary", fun = "mean") + # to plot the mean
  geom_vline(xintercept = 3, linetype = "dashed") +
  facet_wrap(City~.) +
  labs(x = "", y = "") +
  theme_bw()
str(attrEval_long_2)

# Explore Correlation between Attribute Evaluations
corrplot(cor(citydata_refined[, -c(1,2,3,4)]),
         method = "circle", 
         type = "upper",
         number.cex = 0.8,
         tl.cex = 0.9)    

# Explore Correlation between TV Channels
# reshape so we have TV channels as columns
attrEval_wide <- dcast(attrEval_long_2, Sample+ID+id_unique + attribute ~ City)
head(attrEval_wide)
summary(attrEval_wide)
corrplot(cor(attrEval_wide[,-c(1,2,3,4)]),
         method = "number", 
         type = "upper",
         number.cex = 0.9,
         tl.cex = 0.9)

str(attrEval_wide)

# Derive Proximity Measure =====================================================
# Compute Euclidean distance measure 
# Compute dissimilarity on mean attribute evaluations --------------------------
channel.mean <- aggregate(citydata_refined[, -c(1, 2, 3,4,25)], 
                          by = list(City = citydata_refined$City), 
                          FUN = mean)
rownames(channel.mean) <- channel.mean$City

# Compute distance on mean attribute evaluations
dist.onmean <- dist(channel.mean[, -1], method = "euclidean",
                    diag = TRUE, upper = TRUE)
str(dist.onmean)
mds4.dist.onmean <- smacofSym(dist.onmean, 
                                  ndim = 2, 
                                  type = "ordinal")

#with std data
channel.mean_std <- aggregate(citydata_std[, -c(1, 2, 3,4,25)], 
                          by = list(City = citydata_std$City), 
                          FUN = mean)
rownames(channel.mean_std) <- channel.mean_std$City

# Compute distance on mean attribute evaluations
dist.onmean_std <- dist(channel.mean_std[, -1], method = "euclidean",
                    diag = TRUE, upper = TRUE)
dist.onmean_std
x<-as.matrix(dist.onmean_std)
corrplot(x,
         is.corr = FALSE,    # not a correlation matrix
         method = "number",  # display numbers
         type = "upper",     # only upper triangle
         number.cex = 1,     # size of the number elements
         tl.cex = 1,         # size of text labels
         tl.col = "black",   # color of text labels
         col= colorRampPalette(c("grey80", "grey0"))(5)) # color pallet

attrEval <- citydata_std
attrEval$id_new <- citydata_std$id_unique

channel.dist = dist.i = NULL # initialize (creates an empty list)
for(i in unique(attrEval$id_new)){
  
  # To check what is done for each iteration, uncomment the line below
  # and run step-by-step
   i = unique(attrEval$id_new)[1] # fix the i index
  
  # subset the data for each respondent i
  attrEval.i = attrEval[attrEval$id_new == i, -c(1, 2)]
  attrEval.i = attrEval.i[order(attrEval.i$City), ] # sort by City
  rownames(attrEval.i) <- attrEval.i$City
  
  dist.i[[i]] <- dist(attrEval.i[,-1], method = "euclidean")
  channel.dist[[i]] <- as.matrix(dist.i[[i]])
  dist.i[[i]] <- as.dist(dist.i[[i]])
  
  # save as a data frame
  channel.dist[[i]] <- data.frame(channel.dist[[i]])
  
  # add individual counter (id)
  channel.dist[[i]]$id_new <- i
  channel.dist[[i]]$City <- rownames(attrEval.i)
}

# combine the list into one data frame
channel.dist <- do.call(rbind, channel.dist)
dim(channel.dist)
head(channel.dist)

# Compute mean dissimilarity between Citys across respondents
dist.mean <- aggregate(channel.dist[, -c(7,8)], 
                       by = list(City = channel.dist$City), 
                       mean)

rownames(dist.mean) <- dist.mean$City
dist.mean = as.matrix(dist.mean[, -1])
dist.mean

# Also save dist.onmean as matrix
dist.onmean <- as.matrix(dist.onmean)

# Compare the resulting dissimilarity matrices --------------------------------
round(dist.mean, 2)
round(dist.onmean, 2)

corrplot(dist.mean,
         is.corr = FALSE,    # not a correlation matrix
         method = "number",  # display numbers
         type = "upper",     # only upper triangle
         number.cex = 1,     # size of the number elements
         tl.cex = 1,         # size of text labels
         tl.col = "black",   # color of text labels
         col= colorRampPalette(c("grey80", "grey0"))(5)) # color pallet

# Fix the code
corrplot(dist.onmean,
         is.corr = FALSE,
         method = "number",
         type = "upper",
         number.cex = 1,
         tl.cex = 1,
         tl.col = "black") 


mds1.distonmean_std_d1 <- cmdscale(dist.onmean_std, k = 1)

mds2.distonmean_std_d1 <- smacofSym(dist.onmean_std, 
                                 ndim = 1, 
                                 type = "ratio")


mds3.dist.onmean_std_d1 <- smacofSym(dist.onmean_std, 
                                     ndim = 1, 
                                     type = "interval")

mds4.dist.onmean_std_d1 <- smacofSym(dist.onmean_std, 
                                  ndim = 1, 
                                  type = "ordinal")
r_d1_ratio <- cor(c(dist.onmean_std),c(mds2.distonmean_std_d1$confdist))
r_d1_ratio^2
r_d1_interval <- cor(c(dist.onmean_std),c(mds3.dist.onmean_std_d1$confdist))
r_d1_interval^2
r_d1_ordinal <- cor(c(dist.onmean_std),c(mds4.dist.onmean_std_d1$confdist))
r_d1_ordinal^2

#dimensions = 2
mds1.distonmean_std_d2 <- cmdscale(dist.onmean_std, k = 2)

mds2.distonmean_std_d2 <- smacofSym(dist.onmean_std, 
                             ndim = 2, 
                             type = "ratio")
mds3.dist.onmean_std_d2 <- smacofSym(dist.onmean_std, 
                                     ndim = 2, 
                                     type = "interval")

mds4.dist.onmean_std_d2 <- smacofSym(dist.onmean_std, 
                           ndim = 2, 
                           type = "ordinal")





msd1_std <- data.frame(City = rownames(mds1.distonmean_std_d2),
                       mds_method = "absolute",
                       dist_method = "dist.onmean",
                       mds1.distonmean_std_d2)

msd2_std <- data.frame(City = rownames(mds2.distonmean_std_d2$conf),
                       mds_method = "ratio",
                       dist_method = "dist.onmean",
                       mds2.distonmean_std_d2$conf)

msd4_std <- data.frame(City = rownames(mds4.dist.onmean_std_d2$conf),
                       mds_method = "ordinal",
                       dist_method = "dist.onmean",
                       mds4.dist.onmean_std_d2$conf)


msd3_std <- data.frame(City = rownames(mds3.dist.onmean_std_d2$conf),
                       mds_method = "interval",
                       dist_method = "dist.onmean",
                       mds3.dist.onmean_std_d2$conf)


names(msd1_std)[4] <- "D1"
names(msd1_std)[5] <- "D2"
mds <- rbind(msd3_std, msd4_std)
mds_method <- c("interval", "ordinal")



mds$mds_method <- factor(mds$mds_method, levels = mds_method)
# For distance method: distances on mean evals

par(mar = c(4.1, 4.4, 4.1, 1.9),mfrow = c(1,1))
ggplot(data = mds, 
       aes(x = D1, y = D2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 2,
                   box.padding   = 0.8,
                   point.padding = 0.5) +
  facet_wrap(.~mds_method, scales = "free", nrow = 1) +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = "MDS on Dissimilarities based on Mean Attribute Evaluations") +
  theme_bw()
corrplot(x,
         is.corr = FALSE,    # not a correlation matrix
         method = "number",  # display numbers
         type = "upper",     # only upper triangle
         number.cex = 1,     # size of the number elements
         tl.cex = 1,         # size of text labels
         tl.col = "black",   # color of text labels
         col= colorRampPalette(c("grey80", "grey0"))(5)) # color pallet

mds4.dist.onmean_std_d2$confdist
r_d2_ratio <- cor(c(dist.onmean_std),c(mds2.distonmean_std_d2$confdist))
r_d2_ratio^2
r_d2_interval <- cor(c(dist.onmean_std),c(mds3.dist.onmean_std_d2$confdist))
r_d2_interval^2

r_d2_ordinal <- cor(c(dist.onmean_std),c(mds4.dist.onmean_std_d2$confdist))
r_d2_ordinal^2


#dimensions = 3
mds1.distonmean_std_d3 <- cmdscale(dist.onmean_std, k = 3)

mds2.distonmean_std_d3 <- smacofSym(dist.onmean_std, 
                                 ndim = 3, 
                                 type = "ratio")
mds3.dist.onmean_std_d3 <- smacofSym(dist.onmean_std, 
                                     ndim = 3, 
                                     type = "interval")

mds4.dist.onmean_std_d3 <- smacofSym(dist.onmean_std, 
                                  ndim = 3, 
                                  type = "ordinal")

par(mar = c(4.1, 4.4, 4.1, 1.9),mfrow=c(1,2))
plot(mds3.dist.onmean_std_d3,"Shepard",main = "Shepard Diagram for Interval MDS")
plot(mds4.dist.onmean_std_d3,"Shepard", main = "Shepard Diagram for non-Metric MDS")
r_d3_ratio <- cor(c(dist.onmean_std),c(mds2.distonmean_std_d3$confdist))
r_d3_ratio^2
r_d3_interval <- cor(c(dist.onmean_std),c(mds3.dist.onmean_std_d3$confdist))
r_d3_interval^2
r_d3_ordinal <- cor(c(dist.onmean_std),c(mds4.dist.onmean_std_d3$confdist))
r_d3_ordinal^2
#scatterplot 3d for ordinal data on MDS dissimilarity mean attr evaluations ----------
#plot 2d and 3d of ordinal
msd4_std_d3 <- data.frame(City = rownames(mds4.dist.onmean_std_d3$conf),
                       mds_method = "ordinal",
                       dist_method = "dist.onmean",
                       mds4.dist.onmean_std_d3$conf)
msd_std_d3 <- data.frame(City = rownames(mds3.dist.onmean_std_d3$conf),
                          mds_method = "interval",
                          dist_method = "dist.onmean",
                          mds3.dist.onmean_std_d3$conf)
mds$mds_method <- factor(mds$mds_method, levels = mds_method)
#using plotly for 3d plot of MDS--------------
require(plotly)

head(citydata_std)
mds.selected <- msd4_std_d3[, c("City", "D1", "D2","D3")]
mds.selected

# Add coordinates to attrEval (original dataset)
attrEval <- merge(citydata_std, mds.selected, by = "City")
head(attrEval)
profit.vector <- lm(cbind(friendly, historical, affordable,trendy,vibrant.nightlife, delicious.food, easy.to.get.around, good.shopping, cultural.events,interesting.museums,clean,green, international, too.touristic,fun,noisy, romantic,safe,beautiful,english.speaker.friendly) 
                    ~ -1 + D1 + D2+ D3, data = attrEval)
summary(profit.vector) 
param <- data.frame(t(coef(profit.vector)))
param$station <- rownames(param)
m <- list( symbol = 200, size = 8, line = list( color = toRGB("black"), width = 2 ) ) 
#abc - historical, abc2- affordable , abc 3- trendy
a1 <- c(0,0.64704)
b1<- c(0,-0.47659)
c1 <- c(0,-0.34608)
a2<- c(0,0.62831)
b2<-  c(0,0.35505)
c2<- c(0,0.50168)
a3<- c(0,0.78468)
b3<-  c(0,0.55580)
c3<- c(0,0.36919)

plot_ly(x = msd4_std_d3$D1,y= msd4_std_d3$D2, z= msd4_std_d3$D3) %>%
  add_markers() %>% 
  layout(title = 'non metric MDS in 3 dimensions',
         scene = list(xaxis = list(title = 'dimension 1', autorange = TRUE,gridcolor ="black", showgrid = TRUE, zeroline = FALSE, showline = TRUE, autotick = TRUE, ticks = '', showticklabels = TRUE),
                      yaxis = list(title = 'dimesion 2', autorange = TRUE,gridcolor ="black", showgrid = TRUE, zeroline = FALSE, showline = TRUE, autotick = TRUE, ticks = '', showticklabels = TRUE),
                      zaxis = list(title = 'dimension 3', autorange = TRUE,gridcolor ="black", showgrid = FALSE, zeroline = FALSE, showline = TRUE, autotick = TRUE, ticks = '', showticklabels = TRUE),
                      annotations = list(showarrow = TRUE, arrowhead = 4)
         )
  ) %>%
  add_trace(x = a1, y = b1, z = c1, type = "scatter3d", mode = "lines", 
            name = "Historical", showlegend = TRUE, line = list(text = c("hist"), width = 6))%>%
  add_trace(x = a2, y = b2, z = c2, type = "scatter3d", mode = "lines", 
            name = "fun", showlegend = TRUE, line = list(text = c("afford"), width = 6)) %>%
  add_trace(x = a3, y = b3, z = c3, type = "scatter3d", mode = "lines", 
            name = "friendly", showlegend = TRUE, line = list(text = c("hist"), width = 6))%>%
  add_trace(x = msd4_std_d3$D1,y= msd4_std_d3$D2, z= msd4_std_d3$D3, marker = m, type = "scatter3d", mode = "text+markers", 
            name = "original", linetypes = NULL, text = msd4_std_d3$City)

#end of plotly 3d plot mds---------------------------------------------------------
#scatterplot3d package start ------------------------------------------------
s3d<- scatterplot3d(x= msd4_std_d3$D1, y = msd4_std_d3$D2, z = msd4_std_d3$D3,col.axis = "blue", col.grid = "lightblue", 
              main="MDS on Dissimilarities based on Mean Attribute Evaluations - 3d", xlab = "Dim 1", ylab = "Dim 2", zlab ="Dim 3", scale.y = 1.5)

s3d.coords <- s3d$xyz.convert(msd4_std_d3$D1, msd4_std_d3$D2, msd4_std_d3$D3) # convert 3D coords to 2D projection
text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
     labels=msd4_std_d3$City,               # text to plot
     cex= 1.0, pos=4,col = "black", adj = 0.5)           # shrink text 50% and place to right of points)

#scatterplot3d package ends ------------------------------------------------
# Plot3d package starts---------------------------------------------------------
install.packages("plot3D")
library("plot3D")
x0 <- rep(0,20)
y0 <- rep(0,20)
z0 <- rep(0,20)
x1<-c(0,0)
y1<-c(0,0)
z1<-c(0,0)
x2<-c(0.05616025, -0.6493268)
y2<-c( 0.43164262, -0.4839045)
z2<-c(-0.30286617,  0.3296286)
scatter3D(msd4_std_d3$D1, msd4_std_d3$D2, msd4_std_d3$D3, clab = c("Dim 1", "Dim 2", "Dim 3"),phi = 0, 
          main = "3d mds", xlab = "Dim 1",
          ylab ="Dim 2", zlab = "Dim 3" )+
text3D(msd4_std_d3$D1, msd4_std_d3$D2, msd4_std_d3$D3,labels = msd4_std_d3$City,
       add = TRUE, colkey = FALSE, cex = 0.5)
arrows3D(x1, y1, z1,x2, y2, z2, colvar = x2^2,phi = 0,
         lwd = 2, d = 3, clab = c("Quality", "score"), 
         main = "Arrows 3D", bty ="g", ticktype = "detailed")+
# Add starting point of arrow
points3D(x1, y1, z1, add = TRUE, col="darkred", 
         colkey = FALSE, pch = 19, cex = 1)+
# Add labels to the arrows
text3D(x2, y2, z2, c("friendly","historic"),
       colvar = x2^2,  add=TRUE, colkey = FALSE)
# Plot3d package ends---------------------------------------------------------

#end of scatterlot 3d for ordinal data on MDS dissimilarity mean attr evaluations ----------

#dimensions = 4
mds1.distonmean_std_d4 <- cmdscale(dist.onmean_std, k = 4)

mds2.distonmean_std_d4 <- smacofSym(dist.onmean_std, 
                                 ndim = 4, 
                                 type = "ratio")
mds3.dist.onmean_std_d4 <- smacofSym(dist.onmean_std, 
                                     ndim = 4, 
                                     type = "interval")

mds4.dist.onmean_std_d4 <- smacofSym(dist.onmean_std, 
                                  ndim = 4, 
                                  type = "ordinal")
r_d4_ratio <- cor(c(dist.onmean_std),c(mds2.distonmean_std_d4$confdist))
r_d4_ratio^2
r_d4_interval <- cor(c(dist.onmean_std),c(mds3.dist.onmean_std_d4$confdist))
r_d4_interval^2
r_d4_ordinal <- cor(c(dist.onmean_std),c(mds4.dist.onmean_std_d4$confdist))
r_d4_ordinal^2

#dimensions = 5
mds1.distonmean_std_d5 <- cmdscale(dist.onmean_std, k = 5)

mds2.distonmean_std_d5 <- smacofSym(dist.onmean_std, 
                                    ndim = 5, 
                                    type = "ratio")
mds3.dist.onmean_std_d5 <- smacofSym(dist.onmean_std, 
                                     ndim = 5, 
                                     type = "interval")

mds4.dist.onmean_std_d5 <- smacofSym(dist.onmean_std, 
                                     ndim = 5, 
                                     type = "ordinal")

r_d5_ratio <- cor(c(dist.onmean_std),c(mds2.distonmean_std_d5$confdist))
r_d5_ratio^2
r_d5_interval <- cor(c(dist.onmean_std),c(mds3.dist.onmean_std_d5$confdist))
r_d5_interval^2
r_d5_ordinal <- cor(c(dist.onmean_std),c(mds4.dist.onmean_std_d5$confdist))
r_d5_ordinal^2


dim <- c(1,2,3,4,5)
Stress_ordinal <- c(0.25,0.09,0.04,0.02,0.01)

stress_interval <- c(0.292,0.114,0.06,0.033,0.024)
par(mar = c(4.1, 4.4, 4.1, 1.9),mfrow = c(1,1))
plot(x= dim,y= stress_interval,type = "b")
plot(x= dim,y= Stress_ordinal,type = "b")


par(mfrow = c(1,1))

msd3_std$confdist

names(msd1_std)[4] <- "D1"
names(msd1_std)[5] <- "D2"
mds <- rbind(msd1_std, msd2_std, msd3_std, msd4_std)
mds_method <- c("absolute", "ratio", "interval", "ordinal")



mds$mds_method <- factor(mds$mds_method, levels = mds_method)
# For distance method: distances on mean evals
ggplot(data = mds, 
       aes(x = D1, y = D2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 2,
                   box.padding   = 0.8,
                   point.padding = 0.5) +
  facet_wrap(.~mds_method, scales = "free", nrow = 1) +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = "MDS on Dissimilarities based on Mean Attribute Evaluations") +
  theme_bw()

