getwd()
setwd("C:/Users/ASUS/Desktop/CACI")
getwd()
pacman::p_load(reshape2, ggplot2, car, psych, coefplot, corrplot)
library(AICcmodavg)
airbnb_data <- read.csv("AirBnB_TravelerData.csv")
names(airbnb_data[,-1])

data.dist<-dist(apply(airbnb_data[,c("Email_25","Gmail","Edu", "AlaskaFF","Add_Ore","Add_Eug","Age","Tickets","RoundTrip")],2,scale)) # one set of variables selected
head(data.dist)
as.matrix(data.dist)[1:5,1:5]


cluster.ward <- hclust(data.dist, method ="ward.D2")
plot(cluster.ward)
cluster.ward.segment <- cutree(cluster.ward, k=5)
table(cluster.ward.segment)

table(cluster.ward.segment)/nrow(airbnb_data)
nrow(airbnb_data[,"Choice"])

describe.seg <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}

describe.seg(airbnb_data,cluster.ward.segment)



head(airbnb_data)
dim(airbnb_data["Gmail"])
summary(airbnb_data[,-1])
str(airbnb_data)
library(DataExplorer)

create_report(airbnb_data)
scatterplotMatrix(airbnb_data[, -1])

pairs.panels(airbnb_data[, -1],
             method = "pearson",  # correlation method
             hist.col = "grey60", # color of hist. bins
             density = TRUE,      # show density plots
             lm = TRUE)    


install.packages("tidyverse")
install.packages("lsr")
install.packages("rcompanion")
library('rcompanion')

library(tidyverse)
library(lsr)

# function to get chi square p value and Cramers V
f = function(x,y) {
  tbl = airbnb_data %>% select(x,y) %>% table()
  #cor_pval=round(cor.test(airbnb_data %>% select(x),airbnb_data %>% select(y))$p.value,4)
  chisq_pval = round(chisq.test(tbl)$p.value, 4)
  cramV = round(cramerV(tbl), 4) 
  data.frame(x, y, chisq_pval, cramV) }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
df_comb = data.frame(t(combn(sort(names(airbnb_data[,c(-1,-11,-12)])), 2)), stringsAsFactors = F)

# apply function to each variable combination
df_res = map2_df(df_comb$X1, df_comb$X2, f)

df_res %>%
  ggplot(aes(x,y,fill=chisq_pval))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="white", high="purple")+
  theme_classic()
str(airbnb_data)
cor(airbnb_data[,c(2,11,12)])
corrplot(cor(airbnb_data[,c(2,11,12)]),
         method = "number", # Show correlation coefficients (see help & 
         # documentation for other options)
         type = "upper") 

x=0
email25 = airbnb_data$Email_25
email25[1]
for (email25 in nrow(airbnb_data)) {
  i=1
  if(email25[i] == 1){
    x = x+1
  }
  i= i+1
}
x
cormat <- round(cor(airbnb_data[,-1]),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

log_reg1 <- glm(data = airbnb_data, Choice~Email_25+Email_Taxi+Gmail+yahoo+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+Tickets+RoundTrip, family = "binomial")
summary(log_reg1)
plot(log_reg1$fitted.values)

log_reg11 <- glm(data = airbnb_data, Choice~Email_25+Email_Taxi+Gmail+yahoo+Edu+as.factor(AlaskaFF)+Add_Ore+Add_Eug+Age+Tickets+RoundTrip, family = "binomial")
summary(log_reg11)
plot(log_reg1$fitted.values)

log_reg2 <- glm(data = airbnb_data, Choice~Email_25+Gmail+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+RoundTrip, family = "binomial")
summary(log_reg2)


#standardized data logitmodel
airbnb_std <- data.frame(scale(airbnb_data[,c(-1,-2)]),Choice=airbnb_data$Choice)
log_reg3 <- glm(data = airbnb_std, Choice~Email_25+Email_Taxi+Gmail+yahoo+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+Tickets+RoundTrip, family = "binomial")
summary(log_reg3)

log_reg4 <- glm(data = airbnb_std, Choice~Email_25+Gmail+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+RoundTrip, family = "binomial")
summary(log_reg4)

#standardized linear regression
airbnb_std_lm <- data.frame(scale(airbnb_data[,-1]))

log_reg5 <- lm(data = airbnb_std_lm, Choice~Email_25+Email_Taxi+Gmail+yahoo+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+Tickets+RoundTrip)
summary(log_reg5)
AIC(log_reg5)
round(log_reg5$coefficients,3)
#linear regression with unstandardized data
log_reg6 <- lm(data = airbnb_data, Choice~Email_25+Email_Taxi+Gmail+yahoo+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+Tickets+RoundTrip)
summary(log_reg6)
predicted <- predict(log_reg6, type = "response")
# Visual comparison of Actual vs. Fitted satisfaction
model.fitted <- data.frame(actual = airbnb_data$Choice,
                           fitted_m1 = predicted)
plot( airbnb_data$Choice)
plot(log_reg6$fitted.values)
predicted <- predict(log_reg1, type = "response")
plot(predicted)
head(predicted)
plot(predicted,log_reg1$fitted.values,xlab = "predicted",ylab = "observed")

ggplot(data = model.fitted, aes(x = actual, y = fitted_m1)) +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 1, slope = 1) +
  labs(x = "Actual choice", 
       y = "Fitted choice") +
  theme_classic()

str(log_reg6$fitted.values)
hist(log_reg6$fitted.values)
vec = is.na(airbnb_data$Choice)
print(vec)



count = sum(vec)
log_reg7 <- lm(data = airbnb_data, Choice~Email_25+Gmail+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+RoundTrip)
summary(log_reg7)


log_reg8 <- lm(data = airbnb_std_lm, Choice~Email_25+Gmail+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+RoundTrip)
summary(log_reg8)

log_res9 <- glm(data = airbnb_data, Choice~Email_25+Email_Taxi, family = "binomial")
summary(log_res9)

models <- c(log_reg1,log_reg2,log_reg3,log_reg4,log_reg5,log_reg6,log_reg7,log_reg8)

#calculate AIC of each model
aictab(cand.set = log_reg7)



log_reg1 <- glm(data = airbnb_data, Choice~Email_25+Email_Taxi+Gmail+yahoo+Edu+AlaskaFF+Add_Ore+Add_Eug+Age+as.factor(Tickets)+RoundTrip, family = "binomial")
summary(log_reg1)
