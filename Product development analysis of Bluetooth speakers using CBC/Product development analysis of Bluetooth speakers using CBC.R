setwd("C:/Users/ASUS/Desktop/CACI/Assignment 4")
getwd()

# load the libraries (add the packages you require that are missing)
pacman::p_load(reshape2, ggplot2, psych, lavaan, mlogit, gmnl, MASS,EFAtools,fastDummies,psych, corrplot, fpc, cluster,
               nnet, mclust, e1071, randomForest)
library(data.table)
install.packages("naniar")
library(naniar)
# load the data
choiceData_all = read.csv("choiceData.csv")
indivData_all = read.csv("indivData.csv")

# This is the full dataset
dim(indivData_all)  # 697  29
dim(choiceData_all) # 33456    23

head(indivData_all)
head(choiceData_all)

# Use your student id number as the seed and run the code to
# get a subsample of 400 respondents
all = unique(indivData_all$id)
length(all)
set.seed(613826) # INPUT YOUR STUDENT ID NUMBER INSTEAD!!!
subsample = sample(all, 400, replace = FALSE)

# subset the full data files
indivData = subset(indivData_all, id %in% subsample)
choiceData = subset(choiceData_all, id %in% subsample)

dim(indivData)  # 400  29
dim(choiceData) # 19200    23
#check for missing values in the indivdata- which shows some characteristics of the pollution
any(is.na(indivData))#FALSE
any(is.na(choiceData))#FALSE
#check for outliers in the indivData
head(indivData)
summary(indivData)
summary(choiceData)
table(indivData$Own)
table(indivData$IntentToBuy)
table(indivData$BrandAwareness)
table(indivData$Education)
table(indivData$IncomeLabel)
#collapsing income levels
#collapsing income level - "<= 500 ???","501 - 1000 ???" and "1001 - 1500 ???" into "<1500 euro"
#collapsing income levels - "1501 - 2000 ???","2001 - 2500 ???" and "2501 - 3000 ???" into "1501-3000"
indivdata_new = indivData
indivdata_new$Income = ifelse(indivdata_new$Income == 2,1,indivdata_new$Income)
indivdata_new$Income = ifelse(indivdata_new$Income == 3,1,indivdata_new$Income)
indivdata_new$Income = ifelse(indivdata_new$Income == 4,2,indivdata_new$Income)
indivdata_new$Income = ifelse(indivdata_new$Income == 5,2,indivdata_new$Income)
indivdata_new$Income = ifelse(indivdata_new$Income == 6,2,indivdata_new$Income)
indivdata_new$Income = ifelse(indivdata_new$Income == 7,3,indivdata_new$Income)
indivdata_new$Income = ifelse(indivdata_new$Income == 8,4,indivdata_new$Income)

indivdata_new$IncomeLabel = ifelse(indivdata_new$IncomeLabel == "<500","<=1500",indivdata_new$IncomeLabel)
indivdata_new$IncomeLabel = ifelse(indivdata_new$IncomeLabel == "501-1000","<=1500",indivdata_new$IncomeLabel)
indivdata_new$IncomeLabel = ifelse(indivdata_new$IncomeLabel == "1001-1500","<=1500",indivdata_new$IncomeLabel)
indivdata_new$IncomeLabel = ifelse(indivdata_new$IncomeLabel == "1501-2000","1500-3000",indivdata_new$IncomeLabel)
indivdata_new$IncomeLabel = ifelse(indivdata_new$IncomeLabel == "2001-2500","1500-3000",indivdata_new$IncomeLabel)
indivdata_new$IncomeLabel = ifelse(indivdata_new$IncomeLabel == "2501-3000","1500-3000",indivdata_new$IncomeLabel)
table(indivdata_new$IncomeLabel)
table(indivdata_new$Income)
indivdata_long <- indivData[, c("id","Income","Education","Occupation","Age","BrandAwareness","Gender")]
indivdata_long <- melt(indivdata_long, id.vars = "id",
                   variable.name = "item")

#histogram
ggplot(data = indivdata_long, aes(x = value)) +
  geom_histogram() +
  facet_wrap(item~.) +
  theme_classic()

ggplot(data = indivdata_long, aes(x = value)) +
  geom_density() +
  facet_wrap(item~.) +
  theme_classic()
library(corrplot)

cmplt_data = merge(indivdata_new,choiceData,by = "id")
mytable <- table(sort(cmplt_data$IncomeLabel),cmplt_data$priceL)
mytable6 <- table(cmplt_data$GenderLabel,cmplt_data$priceL)

absfre <- barplot(mytable[,-1],
                  main = "willingness to pay vs Income",
                  xlab = "Willingness to Pay", ylab = "Frequency",ylim = c(0,2000),
                  col = c("lightblue","lightgreen","lightyellow","grey"),
                  legend.text = rownames(mytable[,-1]),
                  args.legend = list(title = "Income", x = "topright",cex = 0.65,inset = c(-0.06,0)),beside = TRUE,
                  names.arg=c("$70","$90","$110","$130","150")) # Grouped bars
sum(mytable)
#mean_test = tapply(cmplt_data$priceL, list(cmplt_data$id, cmplt_data$choice == 1,cmplt_data$alt), 
              # FUN=function(x){mean(x)})
#mean_test
dim(cmplt_data[,c(2,3,4,15:18,19,21,23,25,27,32:44,49:51)])
head(cmplt_data[,c(2,3,4,15:18,19,21,23,25,27,32:44,49:51)])
library(DataExplorer)

create_report(cmplt_data[,c(2,3,4,15:18,19,21,23,25,27,32:44,49:51)])
# Plot distribution across respondents and demograhics
# boxplot
ggplot(data = indivdata_long, aes(x = item, y = value)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", color = "darkred") +
  labs(y = "Distribution of demographics") +
  theme_classic()
ggsave("Distribution of demographics.png", device = png, width = 5, height = 4,
               path = "C:/Users/ASUS/Desktop/CACI/Assignment 4")
       
# Mean rating on relative importance of product attributes
importance_mean <-  indivData[, c("id","RelImp_battery","RelImp_weight","RelImp_price","RelImp_sound")]
importance_mean_long <- melt(importance_mean, id.vars = "id",
                       variable.name = "attributes")
ggplot(data = importance_mean_long, aes(y = attributes, x = value)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(y = "", x = "Mean Rating") +
  theme_classic()
table(indivData$Own)
table(indivData$IntentToBuy)

#Peforming Confirmatory factor analysis---------------------------------------------------------
#No. 1 - Subject Knowledge 5-Item likert model
SubjKnow_Model <- "Subjknow =~ SubjKnow_r1 + SubjKnow_r2 + SubjKnow_r3+SubjKnow_r4+SubjKnow_r5"

Subjknow.fit <- cfa(SubjKnow_Model , data = indivData)
SubjKnow <- as.data.frame(lavPredict(Subjknow.fit,method = "Bartlett"))
?lavPredict
summary(SubjKnow)
summary(Subjknow.fit, fit.measures = TRUE, standardized = TRUE)
#no. 2 = product category involvement 5-item likert model
PII_Model <- "PII =~ PII_1 + PII_2 + PII_3+PII_4+PII_5"

PII.fit <- cfa(PII_Model , data = indivData)
pred_PII <- lavPredict(PII.fit,method = "Bartlett")
pred_PII <- as.data.frame(pred_PII)
indivdata_new <- cbind(indivdata_new,pred_PII,SubjKnow)
head(indivdata_new)
summary(PII.fit, fit.measures = TRUE, standardized = FALSE)
par(mfrow = c(1,1))
Own.mean <- aggregate(indivdata_new[,c("PII","Subjknow")], 
                          by = list(Own = indivdata_new$Own), 
                          function(x)c(mean = round(mean(x), 2)))

barplot(as.matrix(Own.mean[,-1]),horiz = TRUE,xlim = c(-0.7,0.7),
        xlab = "Mean Factor Score", ylab = "knowledge and product involvement",
        col = c("darkgrey", "darkblue"),
        legend.text = Own.mean$Own,
        args.legend = list(title = "Own", x = "topright",cex = 0.75,inset = c(0.9, 0)),
        beside = TRUE)
summary(Own.mean)
gender.mean <- aggregate(indivdata_new[,c("PII","Subjknow","Own","IntentToBuy","BrandAwareness")], 
                         by = list(gender = indivdata_new$Gender), 
                         function(x)c(mean = round(mean(x), 2)))
barplot(as.matrix(gender.mean[-3,-1]),
        main = "Gender vs product interest",horiz = TRUE,xlim = c(-1,4),
        xlab = "interest in product", ylab = "Frequency",
        col = c("darkgrey", "darkblue"),
        legend.text = (gender.mean$gender != 3),
        args.legend = list(title = "gender", x = "topright",inset = c(-1,1)),
        beside = TRUE) 



IntentToBuy.mean <- aggregate(indivdata_new[,c("PII","Subjknow")], 
                      by = list(IntentToBuy  = indivdata_new$IntentToBuy ), 
                      function(x)c(mean = round(mean(x), 2)))
barplot(as.matrix(IntentToBuy.mean[,-1]),horiz = TRUE,xlim = c(-0.2,0.5),
        ylab = "knowledge and product involvement", xlab = "Mean Factor Score",
        col = c("darkgrey", "darkblue"),
        legend.text = IntentToBuy.mean$IntentToBuy,
        args.legend = list(title = "Intent to Buy"),
        beside = TRUE)
summary(IntentToBuy.mean)
summary(indivdata_new$BrandAwareness)
xx = indivdata_new[, c("Gender","RelImp_price","RelImp_weight","RelImp_battery","RelImp_sound")]
corrplot(cor(xx), type = "upper")
yy = indivdata_new[, c("IntentToBuy","PII","Subjknow","RelImp_price","RelImp_weight","RelImp_battery","RelImp_sound")]
corrplot(cor(yy), type = "upper")

attr = c("battery","weight","price","sound")
for(i in attr){
  # select column representing dummies for an attribute
  coltemp = names(choiceData[, grep(colnames(choiceData), pattern = paste0(i, "_"))])
  
  # compute rowSum
  choiceData[, "sumtemp"] = rowSums(choiceData[, coltemp])
  
  # If the sum is 0 all effect-coded variables representing the attribute are -1,
  # else the same as dummy coding
  for(m in 1:length(coltemp)){
    choiceData[, paste0("D_", coltemp[m])] = ifelse(choiceData$sumtemp == -length(coltemp), 0, 
                                                        choiceData[, coltemp[m]])
  }
}


head(choiceData)
str(choiceData)
# Estimate several MNL models -------------------------------------
# Create a unique chid counter and index the data object
chid <- unique(choiceData[, c("id", "cs")])
dim(chid)
chid$chid <- 1:nrow(chid)
head(chid, 20)

choiceData <- merge(chid, choiceData, by = c("id", "cs"))
dim(choiceData)
head(choiceData)
str(choiceData)

# sort
choiceData = choiceData[order(choiceData$id, choiceData$chid, choiceData$alt), ]


# Indexing using dfidx() this will be the input into mlogit function 
cbc_ml <- dfidx(choiceData, 
                shape = "long", 
                choice = "choice", # name of the variable representing choice dummy
                idx = list(c("chid", "id")), # outline panel structure 
                idnames = c(NA, "alt"))
head(cbc_ml)

I = length(unique(choiceData$id))
T = length(unique(choiceData$cs))
J = length(unique(choiceData$alt))
# LL of Null model
LL0 = T * I * log(1/J) 
LL0 #-6654.213

# 1. Main effect model with linear price effect and dummy-coding 
#    for other attributes
mnl1 <- mlogit(choice ~ none + priceL + D_battery_8h + D_battery_10h + D_battery_12h + D_battery_14h + D_weight_400g + D_weight_500g +D_weight_600g +
                 D_sound_3.5s + D_sound_4s + D_sound_4.5s | 0, 
               data = cbc_ml)
summary(mnl1)

# Log-likelihood
LL_1 = as.numeric(mnl1$logLik) #-4597.541

# Pseudo-R^2
r1 = 1 - (LL_1 / LL0)
r1

# AIC and BIC
K1 = length(mnl1$coefficients)
AIC_1 = 2*K1 - 2*LL_1
BIC_1 = K1*log(T*I) - 2*LL_1

AIC_1
BIC_1

# 2. Main effect model with linear price effect and effect-coding 
#    for other attributes
mnl2 <- mlogit(choice ~ none + priceL + battery_8h + battery_10h + battery_12h + battery_14h + weight_400g + weight_500g + weight_600g +
                 sound_3.5s + sound_4s + sound_4.5s| 0, 
               data = cbc_ml)
summary(mnl2)

# Log-likelihood
LL = as.numeric(mnl2$logLik)

# Pseudo-R^2
r2 = 1 - (LL / LL0)
r2

# AIC and BIC
K = length(mnl2$coefficients)
AIC = 2*K - 2*LL
BIC = K*log(T*I) - 2*LL

AIC
BIC

# Recover left-out part-worth utilities
alpha = mnl2$coefficients
alpha["battery_16h"] = -(alpha["battery_8h"] + alpha["battery_10h"]+alpha["battery_12h"]+alpha["battery_14h"])
alpha["weight_700g"] = -(alpha["weight_400g"] + alpha["weight_500g"]+alpha["weight_600g"])
alpha["sound_5.0s"] = -(alpha["sound_3.5s"] + alpha["sound_4s"]+ alpha["sound_4.5s"])
round(alpha, 3)


# 3. Main effect model with linear (mean-centered) price effect and 
#    effect-coding for other attributes 
# Mean-center price
cbc_ml$Price_MC = ifelse(cbc_ml$none == 0, cbc_ml$priceL - mean(c(110, 150, 70, 90, 130)), 0)
mnl3 <- mlogit(choice ~ none + Price_MC + battery_8h + battery_10h + battery_12h + battery_14h + weight_400g + weight_500g + weight_600g +
                 sound_3.5s + sound_4s + sound_4.5s| 0, 
               data = cbc_ml)
summary(mnl3)
LL3 = as.numeric(mnl3$logLik)

# Pseudo-R^2
r3 = 1 - (LL3 / LL0)
r3

# AIC and BIC
K3 = length(mnl3$coefficients)
AIC3 = 2*K3 - 2*LL3
BIC3 = K3*log(T*I) - 2*LL3

AIC3
BIC3

# Recover left-out part-worth utilities
coef_summary = as.data.frame(rbind(mnl2$coefficients, mnl3$coefficients))
coef_summary["battery_16h"] = -(coef_summary["battery_8h"] + coef_summary["battery_10h"]+coef_summary["battery_12h"] + coef_summary["battery_14h"])
coef_summary["weight_700g"] = -(coef_summary["weight_400g"] + coef_summary["weight_500g"]+coef_summary["weight_600g"])
coef_summary["sound_5.0s"] = -(coef_summary["sound_3.5s"] + coef_summary["sound_4s"]+coef_summary["sound_4.5s"])
round(coef_summary, 3)

coef_summary$model = c("mnl2", "mnl3")
coef_summary = melt(coef_summary, id.vars = "model", variable.name = "param")

coef_summary$param = as.character(coef_summary$param)

# estimates from dummy coding
beta = data.frame(model = "mnl1", 
                  param = coef_summary[coef_summary$model == "mnl2", ]$param,
                  value = c(as.vector(mnl1$coefficients), rep(0, 3)))

coef_summary = rbind(coef_summary, beta)

coef_summary = dcast(coef_summary, param ~ model)
coef_summary$param = factor(coef_summary$param, 
                            levels = c(paste0("battery_", c("10h", "12h", "14h","16h","8h")),
                                       "none", "priceL", 
                                       paste0("sound_", c("3.5s", "4.5s","4s","5.0s")),
                                       paste0("weight_", c("400g", "500g", "600g","700g"))))



# sort
coef_summary = coef_summary[order(coef_summary$param), ]
coef_summary
str(coef_summary)
write.table(coef_summary, file = "mnl.txt",sep = ",", row.names = TRUE, col.names = TRUE)

# Interpretation --------------------------------------------------
# Compute Relative importance (RI) of attributes
# Delete the None
coef_summary = coef_summary[-6, ]
coef_summary
# add attribute column
coef_summary$attr = c(rep("battery", 5),"Price", rep("sound", 4),
                      rep("weight", 4))


# Compute range of attributes (we will do this based on mnl2,
# Note that coefficient for mnl2 and mnl3 are the same,
# we would also get the same ranges based on mnl1
Range = tapply(coef_summary$mnl2, coef_summary$attr, 
               FUN=function(x){diff(range(x))})

# for price we need to compute range as abs(beta)*(maxPrice - minPrice)
coef_price = coef_summary[coef_summary$param == "priceL", ]$mnl2
price = sort(c(70, 150, 110,90 ,130))# price levels
unique(choiceData$priceL)
# compute Range for price
Range["Price"] = abs(coef_price)*(max(price) - min(price))

# save as a data frame and merge with coef_summary
Range = data.frame(Range)
Range$attr = rownames(Range)
rownames(Range) = NULL

coef_summary = merge(coef_summary, Range, by = "attr")

# sort
coef_summary = coef_summary[order(coef_summary$param), ]
coef_summary

# Compute Relative importance
total_range = sum(Range$Range)
coef_summary$RI = coef_summary$Range / total_range * 100
coef_summary


# Utility in Monetary terms
# Utility in Monetary terms: divide the alphas by price coefficient
coef_summary$UM = coef_summary$mnl2/-coef_price
coef_summary[coef_summary$param == "priceL", ]$UM = NA
coef_summary

# WTP (in reference to the 1st attribute levels)
# We compute based on parameters from dummy coding (betas), 
# but can similarly compute from partworth estimates (alphas)
coef_summary$WTP = coef_summary$mnl1/-coef_price
coef_summary[coef_summary$param == "priceL", ]$WTP = NA
coef_summary



write.table(coef_summary, file = "Choice Model.txt",sep = ",", row.names = TRUE, col.names = TRUE)

#looking at the unobserved heterogenity in the preferences by using mixed logit model
#let's consider linear price effect and effect-coding for the remaining attributes
#so let's choose mnl2 model calculated above
#1. population level estimates
set.seed(125)
# Trade-off: Higher R - more precision, higher estimation time
mxl.rpar <- rep("n", length = length(mnl2$coef))
names(mxl.rpar) <- names(mnl2$coef)
mxl.rpar
cbc_mlogit = mlogit.data(choiceData, 
                         choice = "choice", 
                         shape = "long",
                         id.var = "id", 
                         alt.var = "alt",
                         chid = "chid")
# We will switch to gmnl package for convenience, as there is already 
# a function to compute individual-level estimates
# It is a bit slower than mlogit!!!
mxl_gmnl = NULL # initialize
r = c(200, 600) # vary the number of draws
mxl_gmnl = NULL # initialize

  
for(i in 1:length(r)){
  R = r[i]
  mxl.temp <-  gmnl(choice ~ none + priceL + battery_8h + battery_10h + battery_12h + battery_14h + weight_400g
                    + weight_500g + weight_600g + sound_3.5s+sound_4.5s + sound_4s| 0, 
                    data = cbc_mlogit,
                    model = "mixl", correlation = FALSE, haltons = NULL,
                    R = R, panel = TRUE, 
                    seed = 125,       # set seed for replicability
                    ranp = mxl.rpar)
  
  save(mxl.temp, file = paste0("mxl_diag", R, ".RData"))
  
  mxl_gmnl[[i]] = mxl.temp
}

# load the results
mxl_diag200 = get(load("mxl_diag200.RData"))
mxl_diag600 = get(load("mxl_diag600.RData"))

summary(mxl_diag200)
summary(mxl_diag600)


res = cbind(mxl_diag200$coefficients,mxl_diag600$coefficients)
res = rbind(res, c(as.numeric(mxl_diag200$logLik$maximum),
                   as.numeric(mxl_diag600$logLik$maximum)))
rownames(res) = c(rownames(res)[-nrow(res)], "LL")

round(res, 2)
write.table(round(res, 2), file = "mixed logit model.txt",sep = ",", row.names = TRUE, col.names = TRUE)
# Population-level estimates --------------------------------------------------
beta = mxl_diag600$coefficients[1:12]
sigma = mxl_diag600$coefficients[13:24]

names(sigma) = names(beta)

beta
sigma

# Draw from the population-level estimates
set.seed(158)
pop = mvrnorm(n = 600, mu = beta, Sigma = diag(sigma^2))
pop = data.frame(pop)
pop$id = 1:nrow(pop)
head(pop)
dim(pop)
dim(subset(pop, priceL < 0))
# reshape to long format
pop_long = melt(pop, id.vars = "id", variable.name = "param")
head(pop_long)

# Compute individual-level estimates ------------------------------------------
betai = data.frame(effect.gmnl(mxl_diag600)$mean)
betai$id = unique(cbc_mlogit$id)
head(betai)
dim(betai)
head(indivdata_new)
# Reshape to long format
betai_long = melt(betai, id.vars = "id",
                  variable.name = "param")
head(betai_long)

# Plot the distribution of individual- and population-level estimates
ggplot(data = betai_long, aes(x = value)) +
  geom_histogram(bins = 40, aes(y=..density..)) +
  geom_histogram(data = pop_long, bins = 40, aes(y=..density..), alpha = 0.2) +
  facet_wrap(.~ param, scales = "free") +
  labs(x = "") +
  theme_bw()


# Interpretation ==============================================
# Relative Importance of Attributes ---------------------------
# First based on Individual-level Estimates
# Recover left-out part-worth utilities
betai = subset(betai,priceL < 0)
betai["battery_16h"] = -(betai["battery_8h"] + betai["battery_10h"]+betai["battery_12h"]+betai["battery_14h"])
betai["weight_700g"] = -(betai["weight_400g"] + betai["weight_500g"] + betai["weight_600g"])
betai["sound_5.0s"] = -(betai["sound_3.5s"] + betai["sound_4s"] + betai["sound_4.5s"])

betai_long = melt(betai, id.vars = "id", variable.name = "param")

betai_long$param = as.character(betai_long$param)
head(betai_long)

# Compute Relative importance (RI) of attributes
# Delete the None
betai_long = subset(betai_long, param != "none")

# add attribute column
betai_long$attr = sapply(strsplit(betai_long$param, "_"), `[`, 1)
unique(betai_long$attr)
betai_long_clus = betai_long
# Compute range of attributes 
Range = tapply(betai_long$value, list(betai_long$id, betai_long$attr), 
               FUN=function(x){diff(range(x))})

# for price we need to compute range as abs(beta)*(maxPrice - minPrice)
coef_price = betai_long[betai_long$param == "priceL", ]$value
price = c(110,70,90,130,150) # price levels
unique(cmplt_data$priceL)
# compute Range for price
Range[, "priceL"] = abs(coef_price)*(max(price) - min(price))
dim(Range)
# save as a data frame
Range = data.frame(Range)
rownames(Range) = NULL

# Compute RI
RI = Range / rowSums(Range) * 100
head(RI)
RI$id = unique(betai$id)
colnames(RI) <- c("battery_RI","price_RI","sound_RI","weight_RI","id")
indiv_estimates <- merge(indivdata_new,RI, by ="id")
head(indiv_estimates)
length(unique(indiv_estimates$id))
length(unique(RI$id))
length(unique(indiv_estimates$id))
dim(indiv_estimates)
# reshape to long format
RI_long = melt(RI, id.vars = "id", variable.name = "attribute",
               value.name = "RI")

ggplot(data = RI_long, aes(y = attribute, x = RI)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(y = "", x = "Mean Rating") +
  theme_classic()

# plot
ggplot(data = RI_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Individual-level Estimates") +
  theme_bw()



# save the figure
ggsave("RI_indiv.png", device = png, width = 4, height = 4,
        path = "C:/Users/ASUS/Desktop/CACI/Assignment 4")


# Based on population-level estimates 
# omit those draws that result in a positive price coefficient
pop = subset(pop, priceL < 0)
dim(pop)
price_pos = subset(pop, priceL > 0)
dim(price_pos) #23 people have positive price coefficients 
# Recover left-out part-worth utilities
pop["battery_16h"] = -(pop["battery_8h"] + pop["battery_10h"]+pop["battery_12h"]+pop["battery_14h"])
pop["weight_700g"] = -(pop["weight_400g"] + pop["weight_500g"] + pop["weight_600g"])
pop["sound_5.0s"] = -(pop["sound_3.5s"] + pop["sound_4s"] + pop["sound_4.5s"])


# Compute Relative importance (RI) of attributes
# reshape to long format
pop_long = melt(pop, id.vars = "id", variable.name = "param")
head(pop_long)

# Delete the None
pop_long = subset(pop_long, param != "none")

pop_long$param = as.character(pop_long$param)

# add attribute column
pop_long$attr = sapply(strsplit(pop_long$param, "_"), `[`, 1)
head(pop_long)
unique(pop_long$attr)
# Compute range of attributes 
Range = tapply(pop_long$value, list(pop_long$id, pop_long$attr), 
               FUN=function(x){diff(range(x))})

# for price we need to compute range as abs(beta)*(maxPrice - minPrice)
coef_price = pop_long[pop_long$param == "priceL", ]$value
price = c(110,70,90,130,150) # price levels

# compute Range for price
Range[, "priceL"] = abs(coef_price)*(max(price) - min(price))

# save as a data frame
Range = data.frame(Range)
rownames(Range) = NULL

# Compute RI
RI_pop = Range / rowSums(Range) * 100
RI_pop$id = 1:nrow(RI_pop)
head(RI_pop)

# reshape to long format
RI_pop_long = melt(RI_pop, id.vars = "id", variable.name = "attribute",
                   value.name = "RI")
head(RI_pop_long)
ggplot(data = RI_pop_long, aes(y = attribute, x = RI)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(y = "", x = "Mean Rating") +
  theme_classic()
# plot
ggplot(data = RI_pop_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Population-level Estimates") +
  theme_bw()


# save the figure
ggsave("RI_pop.png", device = png, width = 4, height = 4,
       path = "C:/Users/ASUS/Desktop/CACI/Assignment 4")



# Let's also investigate the individual difference in RI patterns
RI$pattern = apply(as.matrix(RI[, -5]), 1, which.max)
RI$plot = ifelse(duplicated(RI$pattern), 0, 1)
head(RI)

indiv_ids_plot = subset(RI, plot == 1)
indiv_ids_plot = melt(indiv_ids_plot, id.vars = "id", 
                      variable.name = "attribute",
                      value.name = "RI")
indiv_ids_plot = subset(indiv_ids_plot, !attribute %in% c("pattern", "plot"))

ggplot(data = RI_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  geom_point(data = indiv_ids_plot, alpha = 0.3) + 
  geom_line(data = indiv_ids_plot, aes(group = id), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Individual-level Estimates") +
  theme_bw()

# save
 ggsave("RI_indiv_patterns.png", device = png, width = 4, height = 4,
        path = "C:/Users/ASUS/Desktop/CACI/Assignment 4")


RI_pop$pattern = apply(as.matrix(RI_pop[, -5]), 1, which.max)
RI_pop$plot = ifelse(duplicated(RI_pop$pattern), 0, 1)
head(RI_pop)

pop_ids_plot = subset(RI_pop, plot == 1)
pop_ids_plot = melt(pop_ids_plot, id.vars = "id", 
                    variable.name = "attribute",
                    value.name = "RI")
pop_ids_plot = subset(pop_ids_plot, !attribute %in% c("pattern", "plot"))

ggplot(data = RI_pop_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  geom_point(data = pop_ids_plot, alpha = 0.3) + 
  geom_line(data = pop_ids_plot, aes(group = id), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Population-level Estimates") +
  theme_bw()

# save
ggsave("RI_pop_patterns.png", device = png, width = 4, height = 4,
        path = "C:/Users/ASUS/Desktop/CACI/Assignment 4")


# Willigness-to-Pay (based on individual estimates) ----------------------------------
pricei = subset(betai_long, attr == "priceL")[, c("id", "value")]
names(pricei) = c("id", "UM")
head(pricei)

betai_long = merge(betai_long, pricei, by = "id")
head(betai_long)

betai_long$UM = betai_long$value / -betai_long$UM
head(betai_long)
UM = subset(betai_long, param != "priceL")[, c("id", "attr", "param", "UM")] 
base = subset(UM, param %in% c("battery_8h", "sound_4s", "weight_700g"))
base = base[, c("id", "attr", "UM")]
names(base) = c("id", "attr", "base")
head(base)
UM = merge(UM, base, by = c("id", "attr"))
UM$WTP = UM$UM - UM$base
head(UM)
betai_WTP = betai_long
betai_WTP$WTP = ifelse(betai_WTP$param == "priceL",0,UM$WTP)
data1 = subset(UM, WTP != 0)
head(data1)
boxplot(data1[,c("WTP")])
#handling outliers in WTP using Inter-Quantile Range
qnt <- quantile(UM[,c("WTP")], probs=c(.25, .75), na.rm = T)
caps <- quantile(UM[,c("WTP")], probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(UM[,c("WTP")], na.rm = T)
UM[,c("WTP")][UM[,c("WTP")] < (qnt[1] - H)] <- caps[1]
UM[,c("WTP")][UM[,c("WTP")] > (qnt[2] + H)] <- caps[2]

qnt <- quantile(UM[,c("UM")], probs=c(.25, .75), na.rm = T)
caps <- quantile(UM[,c("UM")], probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(UM[,c("UM")], na.rm = T)
UM[,c("UM")][UM[,c("UM")] < (qnt[1] - H)] <- caps[1]
UM[,c("UM")][UM[,c("UM")] > (qnt[2] + H)] <- caps[2]
summary(UM)
UM_wide = reshape(data = UM[,c(1,3,4,6)],timevar = "param", idvar = "id" , direction = "wide")
summary(UM_wide)

# Plot WTP
ggplot(data = data1, aes(x = WTP)) + 
  geom_histogram(bins = 30, alpha = 0.8) +
  geom_vline(xintercept = 0) +
  facet_wrap(.~param, scales = "free_x") +
  labs(x = "Willingness-to-pay", y = "Frequency") +
  theme_bw()
# save
# ggsave("wtp.png", device = png, width = 6, height = 4,
#        path = "C:/Users/narin/Dropbox/TEACHING/CACI/CACI_WS2122/Slides/Slides_new/8_Conjoint Analysis/graphics")


# NOTE: Similarly, you can investigate the distribution of WTP on population-level
# e.g., based on pop_long data frame, where we have generated 600 draws 
# from population-level estimates
price_pop = subset(pop_long, attr == "priceL")[, c("id", "value")]
names(price_pop) = c("id", "UM")
summary(price_pop)

pop_long = merge(pop_long, price_pop, by = "id")
head(pop_long)
summary(pop_long)
pop_long$UM = pop_long$value / -pop_long$UM
head(pop_long)

UM_pop = subset(pop_long, param != "priceL")[, c("id", "attr", "param", "UM")] 
unique(UM_pop$param)
head(UM_pop)
base = subset(UM_pop, param %in% c("battery_8h", "sound_4s", "weight_700g"))
base = base[, c("id", "attr", "UM")]
names(base) = c("id", "attr", "base")
head(base)

UM_pop = merge(UM_pop, base, by = c("id", "attr"))
UM_pop$WTP = UM_pop$UM - UM_pop$base
head(UM_pop)
summary(UM_pop)
data1 = subset(UM_pop, WTP != 0)
any(is.na(betai))
head(data1)
boxplot(data1[,c("WTP")])
#handling outliers in WTP using Inter-Quantile Range
qnt <- quantile(data1[,c("WTP")], probs=c(.25, .75), na.rm = T)
caps <- quantile(data1[,c("WTP")], probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(data1[,c("WTP")], na.rm = T)
data1[,c("WTP")][data1[,c("WTP")] < (qnt[1] - H)] <- caps[1]
data1[,c("WTP")][data1[,c("WTP")] > (qnt[2] + H)] <- caps[2]
summary(data1)
# Plot WTP
ggplot(data = data1, aes(x = WTP)) +xlim(-150,150)+
  geom_histogram(bins = 30, alpha = 0.8) +
  geom_vline(xintercept = 0) +
  facet_wrap(.~param, scales = "free_x") +
  labs(x = "Willingness-to-pay", y = "Frequency") +
  theme_bw()

UM_pop_wide = reshape(data = UM_pop[,c(1,3,4,6)],timevar = "param", idvar = "id" , direction = "wide")
summary(UM_pop_wide)

#Clustering the individuals based on consumer preferences for individual level estimates based on wide format of the data
indiv_estimates_cluster_wide = merge(UM_wide,indiv_estimates, by = "id")
pred_vars = indiv_estimates_cluster_wide[,c(2:27)]
pred_vars = scale(pred_vars[,-c(4,12,26)])
?scale
dim(data.sc_wide_indiv)
head(data.sc_wide_indiv)
summary(data.sc_wide_indiv)
dist.eucl_wide_indiv <- dist(pred_vars)
as.matrix(dist.eucl_wide_indiv)[1:6, 1:6]
#heirarchial clustering with euclidean distance
#euclidean distance
cl.single.eucl.wide <- hclust(dist.eucl_wide_indiv, method = "single")     # single linkage method
cl.complete.eucl.wide <- hclust(dist.eucl_wide_indiv, method = "complete") # complete linkage method
cl.average.eucl.wide <- hclust(dist.eucl_wide_indiv, method = "average")   # average linkage method
cl.centroid.eucl.wide <- hclust(dist.eucl_wide_indiv, method = "centroid") # centroid linkage method
cl.median.eucl.wide <- hclust(dist.eucl_wide_indiv, method = "median")     # median linkage method
cl.ward.eucl.wide <- hclust(dist.eucl_wide_indiv, method = "ward.D2") 

plot(as.dendrogram(cl.single.eucl.wide), ylim = c(0, 3),leaflab = "none") #chain
rect.hclust(cl.single.eucl.wide, k = 3, border = "darkred")
table(cutree(cl.single.eucl.wide, 3))
#complete linkage
plot(as.dendrogram(cl.complete.eucl.wide))
rect.hclust(cl.complete.eucl.wide, k = 3, border = "darkred")#chain
table(cutree(cl.complete.eucl.wide, 3))
#average linkage
plot(as.dendrogram(cl.average.eucl.wide)) #chain
rect.hclust(cl.average.eucl.wide, k = 3, border = "darkred")#chain
table(cutree(cl.average.eucl.wide, 3))
#centroid linkage
plot(as.dendrogram(cl.centroid.eucl.wide)) #chain
rect.hclust(cl.centroid.eucl.wide, k = 3, border = "darkred")
table(cutree(cl.centroid.eucl.wide, 3))
#median linkage
plot(as.dendrogram(cl.median.eucl.wide)) #chain
rect.hclust(cl.median.eucl.wide, k = 3, border = "darkred")
table(cutree(cl.median.eucl.wide, 3))
#wards method
plot(as.dendrogram(cl.ward.eucl.wide))
rect.hclust(cl.ward.eucl.wide, k = 3, border = "darkred")
table(cutree(cl.ward.eucl.wide, 3))


VRC.ward = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward[k] <- cluster.stats(d = dist.eucl_wide_indiv, 
                               clustering = cutree(cl.ward.eucl.wide, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward
VRC = data.frame(K = 2:10, ward = VRC.ward)

# reshape to long
VRC = melt(VRC, id.vars = "K")

ggplot(VRC, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()
indiv_estimates_cluster_wide$cluster.ward <- cutree(cl.ward.eucl.wide, 3)
clust.mean <- aggregate(indiv_estimates_cluster_wide[,c(2:30,41:45,47,49,51,53,56:61)], 
                        by = list(cluster = indiv_estimates_cluster_wide$cluster.ward), 
                        function(x)c(mean = round(mean(x), 2)))
t(clust.mean)
write.table(clust.mean, file = "segmentation based on RI,UM,WTP_v2.txt",sep = ",", row.names = TRUE, col.names = TRUE)
table(indiv_estimates$cluster.ward)
#Clustering the individuals based on consumer preferences for individual level estimates based on long format of the data
#segmenting the data based on estimated importance of product attributes
#finding the UM and WTP post for each cluster post the segmenation of data

indiv_estimates_cluster = indiv_estimates[, c(32:35)]
data.sc = scale(indiv_estimates_cluster)
dim(indiv_estimates_cluster)
head(data.sc)
summary(data.sc)
dist.eucl <- dist(data.sc)
as.matrix(dist.eucl)[1:6, 1:6]
#heirarchial clustering with euclidean distance
#euclidean distance
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
rect.hclust(cl.complete.eucl, k = 3, border = "darkred")#chain
table(cutree(cl.complete.eucl, 3))
#average linkage
plot(as.dendrogram(cl.average.eucl)) #chain
rect.hclust(cl.average.eucl, k = 3, border = "darkred")#chain
table(cutree(cl.average.eucl, 3))
#centroid linkage
plot(as.dendrogram(cl.centroid.eucl)) #chain
rect.hclust(cl.centroid.eucl, k = 3, border = "darkred")
table(cutree(cl.centroid.eucl, 3))
#median linkage
plot(as.dendrogram(cl.median.eucl)) #chain
rect.hclust(cl.median.eucl, k = 3, border = "darkred")
table(cutree(cl.median.eucl, 3))
#wards method
plot(as.dendrogram(cl.ward.eucl))
rect.hclust(cl.ward.eucl, k = 4, border = "darkred")
table(cutree(cl.ward.eucl, 4))

#looking at the above dendograms it is evident that wards method is the best choice given our data, with has outliers. 
#Wards being robust to outliers gives us clearly defined clusters witout chain formation

#VRC - Variance ratio criterion or Calinski-Harabasz (CH) index ---------------------
# we will use fpc package
VRC.ward = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward[k] <- cluster.stats(d = dist.eucl, 
                               clustering = cutree(cl.ward.eucl, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward
VRC = data.frame(K = 2:10, ward = VRC.ward)

# reshape to long
VRC = melt(VRC, id.vars = "K")

ggplot(VRC, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()
indiv_estimates$cluster.ward <- cutree(cl.ward.eucl, 3)
clust.mean <- aggregate(indiv_estimates[, -c(1,5:14,20,22,24,26,28,29,36)], 
                        by = list(cluster = indiv_estimates$cluster.ward), 
                        function(x)c(mean = round(mean(x), 2)))
t(clust.mean)
id_cluster1 = subset(indiv_estimates, cluster.ward == 1)[,c("id")]
id_cluster2 = subset(indiv_estimates, cluster.ward == 2)[,c("id")]
id_cluster3 = subset(indiv_estimates, cluster.ward == 3)[,c("id")]
library(dplyr)
betai_long_clus1 <- betai_long_clus %>% 
  filter(id %in% id_cluster1)
betai_long_clus2 <- betai_long_clus %>% 
  filter(id %in% id_cluster2)
betai_long_clus3 <- betai_long_clus %>% 
  filter(id %in% id_cluster3)
length(unique(betai_long_clus1$id))
#188
length(unique(betai_long_clus2$id))
# 134
length(unique(betai_long_clus3$id))
#72

#UM and WTP for cluster 1---------------------------------------------------------------------------
pricei_clus1 = subset(betai_long_clus1, attr == "priceL")[, c("id", "value")]
names(pricei_clus1) = c("id", "UM")
head(pricei_clus1)

betai_long_clus1 = merge(betai_long_clus1, pricei_clus1, by = "id")
head(betai_long_clus1)

betai_long_clus1$UM = betai_long_clus1$value / -betai_long_clus1$UM
head(betai_long_clus1)
UM_clus1 = subset(betai_long_clus1, param != "priceL")[, c("id", "attr", "param", "UM")] 
base = subset(UM_clus1, param %in% c("battery_8h", "sound_4s", "weight_700g"))
base = base[, c("id", "attr", "UM")]
names(base) = c("id", "attr", "base")
head(base)
UM_clus1 = merge(UM_clus1, base, by = c("id", "attr"))
UM_clus1$WTP = UM_clus1$UM - UM_clus1$base

#UM and WTP for cluster 2---------------------------------------------------------------------------
pricei_clus2 = subset(betai_long_clus2, attr == "priceL")[, c("id", "value")]
names(pricei_clus2) = c("id", "UM")
head(pricei_clus2)

betai_long_clus2 = merge(betai_long_clus2, pricei_clus2, by = "id")
head(betai_long_clus2)

betai_long_clus2$UM = betai_long_clus2$value / -betai_long_clus2$UM
head(betai_long_clus2)
UM_clus2 = subset(betai_long_clus2, param != "priceL")[, c("id", "attr", "param", "UM")] 
base = subset(UM_clus2, param %in% c("battery_8h", "sound_4s", "weight_700g"))
base = base[, c("id", "attr", "UM")]
names(base) = c("id", "attr", "base")
head(base)
UM_clus2 = merge(UM_clus2, base, by = c("id", "attr"))
UM_clus2$WTP = UM_clus2$UM - UM_clus2$base

#UM and WTP for cluster 3---------------------------------------------------------------------------
pricei_clus3 = subset(betai_long_clus3, attr == "priceL")[, c("id", "value")]
names(pricei_clus3) = c("id", "UM")
head(pricei_clus3)

betai_long_clus3 = merge(betai_long_clus3, pricei_clus3, by = "id")
head(betai_long_clus3)

betai_long_clus3$UM = betai_long_clus3$value / -betai_long_clus3$UM
head(betai_long_clus3)
UM_clus3 = subset(betai_long_clus3, param != "priceL")[, c("id", "attr", "param", "UM")] 
base = subset(UM_clus3, param %in% c("battery_8h", "sound_4s", "weight_700g"))
base = base[, c("id", "attr", "UM")]
names(base) = c("id", "attr", "base")
head(base)
UM_clus3 = merge(UM_clus3, base, by = c("id", "attr"))
UM_clus3$WTP = UM_clus3$UM - UM_clus3$base
#checking outliers in WTP
summary(UM_clus3)
summary(UM_clus1)
boxplot(UM_clus1[,c("WTP")])
boxplot(UM_clus2[,c("WTP")])
boxplot(UM_clus3[,c("WTP")])

install.packages("outliers")
library(outliers)
outliers::scores(UM_clus3[,c("WTP")], type="z", prob=0.95)  # beyond 95th %ile based on z-scores
clus_names = c("UM_clus1","UM_clus2","UM_clus3")

qnt <- quantile(UM_clus3[,c("WTP")], probs=c(.25, .75), na.rm = T)
caps <- quantile(UM_clus3[,c("WTP")], probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(UM_clus3[,c("WTP")], na.rm = T)
UM_clus3[,c("WTP")][UM_clus3[,c("WTP")] < (qnt[1] - H)] <- caps[1]
UM_clus3[,c("WTP")][UM_clus3[,c("WTP")] > (qnt[2] + H)] <- caps[2]

qnt <- quantile(UM_clus2[,c("WTP")], probs=c(.25, .75), na.rm = T)
caps <- quantile(UM_clus2[,c("WTP")], probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(UM_clus2[,c("WTP")], na.rm = T)
UM_clus2[,c("WTP")][UM_clus2[,c("WTP")] < (qnt[1] - H)] <- caps[1]
UM_clus2[,c("WTP")][UM_clus2[,c("WTP")] > (qnt[2] + H)] <- caps[2]

qnt <- quantile(UM_clus1[,c("WTP")], probs=c(.25, .75), na.rm = T)
caps <- quantile(UM_clus1[,c("WTP")], probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(UM_clus1[,c("WTP")], na.rm = T)
UM_clus1[,c("WTP")][UM_clus1[,c("WTP")] < (qnt[1] - H)] <- caps[1]
UM_clus1[,c("WTP")][UM_clus1[,c("WTP")] > (qnt[2] + H)] <- caps[2]
# Plot WTP for all 3 clusters
#cluster 1
ggplot(data = subset(UM_clus1, WTP != 0), aes(x = WTP)) +
  geom_histogram(bins = 40, alpha = 0.8) +
  geom_vline(xintercept = 0) +
  facet_wrap(.~param, scales = "free_x", nrow = 3) +
  labs(x = "Willingness-to-pay", y = "Frequency") +
  theme_bw()
#cluster 2
ggplot(data = subset(UM_clus2, WTP != 0), aes(x = WTP)) +
  geom_histogram(bins = 40, alpha = 0.8) +
  geom_vline(xintercept = 0) +
  facet_wrap(.~param, scales = "free_x", nrow = 3) +
  labs(x = "Willingness-to-pay", y = "Frequency") +
  theme_bw()
#cluster 3
ggplot(data = subset(UM_clus3, WTP != 0), aes(x = WTP)) +
  geom_histogram(bins = 40, alpha = 0.8) +
  geom_vline(xintercept = 0) +
  facet_wrap(.~param, scales = "free_x", nrow = 3) +
  labs(x = "Willingness-to-pay", y = "Frequency") +
  theme_bw()





#Market Simulation
predict.mxl = function(X, beta) {
  
  beta = as.matrix(beta[, names(X)])
  I = nrow(beta)
  J = nrow(X)
  
  data = NULL
  for (i in 1:I) {
    xi = as.matrix(X)
    
    data[[i]] = data.frame(id = i, 
                           Alt = rep(1:J),
                           u = c(xi %*% beta[i,]))
  }
  data = do.call(rbind, data) 
  
  data$exp_u = exp(data$u)
  data$sum_exp_u = rep(tapply(data$exp_u, data$id, sum), each = J)
  
  data$p_hat = data$exp_u/data$sum_exp_u
  data$choice_hat = rep(tapply(data$p_hat, data$id, which.max), each = J)
  data$choice_hat = ifelse(data$choice_hat == data$Alt, 1, 0)
  
  
  data[, c("exp_u", "sum_exp_u")] = NULL
  return(data)
}

# market scenario: 

# market scenario: 
#1) Firm 1: Sound quality of 5.0 stars, weight of 600grams, battery life of 12 hours
#2) Firm 2: Sound quality of 4.0 stars, weight of 400grams, battery life of 16 hours
#Scenario 2:
#a.	Firm 1, product 1: Sound quality of 5.0 stars, weight of 600grams, battery life of 12 hours
#b.	Firm 1, product 2:Sound quality of 5.0 stars, weight of 600grams, battery life of 16 hours
#c.	Firm 2, product 3 : Sound quality of 4.0 stars, weight of 400grams, battery life of 16 hours
x = data.frame(none = c(0,0, 0, 1),
                priceL = c(114, 90, 114, 0),
                battery_8h = c(0,-1, -1, 0),
                battery_10h = c(0,-1, -1, 0),
                battery_12h = c(1,-1, -1, 0),
                battery_14h = c(0,-1,-1,0),
                weight_400g = c(0,0, 1, 0),
                weight_500g = c(0,0,0,0),
                weight_600g = c(1,1,0,0),
                sound_3.5s = c(-1,0,0,0),
                sound_4.5s = c(-1,0,0,0),
                sound_4s = c(-1,1,1,0))
x




# predict based on individual-level estimates
pred_indiv = predict.mxl(x, betai)
head(pred_indiv)

# predicted choice shares
pred_indiv_ms = subset(pred_indiv, choice_hat == 1)
prop.table(table(pred_indiv_ms$Alt)) * 100

# predicted based on population-level estimates
pred_pop = predict.mxl(x, pop)
head(pred_pop)


# predicted choice shares
pred_pop_ms = subset(pred_pop, choice_hat == 1)
prop.table(table(pred_pop_ms$Alt)) * 100


# Changes in market shares due to price adjustments in alt 2
price = seq(60, 150, by = 5)

indiv_ms = data.frame(price = price, Alt1 = 0, Alt2 = 0, Alt3 = 0, Alt4 = 0)
pop_ms = data.frame(price = price, Alt1 = 0, Alt2 = 0, Alt3 = 0, Alt4 = 0)
for(i in 1:length(price)){
  
  # adjust the X matrix
  x[2, "priceL"] = price[i]
  
  # based on indiv-level est
  temp = predict.mxl(x, betai)
  temp = tapply(temp$choice_hat, list(temp$Alt), mean)
  indiv_ms[i, -1] = temp
  
  # based on pop-level est
  temp = predict.mxl(x, pop)
  temp = tapply(temp$choice_hat, list(temp$Alt), mean)
  pop_ms[i, -1] = temp
}

indiv_ms = melt(indiv_ms, id.vars = "price")
names(indiv_ms) = c("price", "Alt", "indiv_ms")

pop_ms = melt(pop_ms, id.vars = "price")
names(pop_ms) = c("price", "Alt", "pop_ms")

# merge
ms = merge(indiv_ms, pop_ms, by = c("price", "Alt"))
head(ms)

ms = melt(ms, id.vars = c("price", "Alt"))
ms$Alt = as.integer(substr(ms$Alt, 4, 4))
ms$variable = sapply(strsplit(as.character(ms$variable), "_"), `[`, 1)
head(ms)

# plot
ggplot(data = ms, aes(x = price, y = value, 
                      color = as.factor(Alt),
                      alpha = variable)) +
  geom_point() + geom_line() +
  scale_alpha_manual(values = c(1, 0.3)) +
  labs(x = "Price for Alt 3 (114 for Alt1 and 114 for Alt2)",
       y = "Market share",
       color = "Alternative",
       alpha = "Estimates") +
  theme_bw()




# Optimal Price for Alt 2 to maximize combined profits --------------------
ms$costs = ifelse(ms$Alt == 1, 75, 
                  ifelse(ms$Alt == 2, 75, 
                         ifelse(ms$Alt == 3, 70, 0)))
head(ms)

# Profit per unit = (Price - Costs)
ms$profit_unit = ifelse(ms$Alt == 1, x[1, "priceL"] - ms$costs,
                        ifelse(ms$Alt == 2, x[2, "priceL"] - ms$costs,
                               ms$price - ms$costs)) 

# Profit = Market share * Profit per unit
ms$profit = ms$value * ms$profit_unit
ms$profit = ifelse(ms$Alt == 4, 0, ms$profit)

# compute joint profits
profits = tapply(ms$profit, list(ms$variable, ms$price), sum)
profits = data.frame(t(profits))
profits$price = price

profits = melt(profits, id.vars = "price")

# maximum joint profit
maxprofit = tapply(profits$value, list(profits$variable), max)

# at which price?
profits$maxprofit = ifelse(profits$variable == "indiv", 
                           maxprofit["indiv"], maxprofit["pop"])
profits$maxprofit = ifelse(profits$value == profits$maxprofit, 1, 0)

maxprofit = subset(profits, maxprofit == 1)

# plot
ggplot(data = profits, aes(x = price, y = value,
                           alpha = variable)) +
  geom_point() + geom_line() +
  # add vertical lines at the optimal price level (for joint profits)
  geom_vline(data = maxprofit, aes(xintercept = price, 
                                   alpha = variable)) +
  # add optimal price level on the graph
  #geom_text_repel(data = maxprofit,
  #                aes(label = paste0(price, "EUR"),
   #                   alpha = variable), show.legend = FALSE) +
  scale_alpha_manual(values = c(1, 0.3)) +
  labs(x = "Price for Alt 3 (79EUR for Alt1 and 99EUR for Alt2)",
       y = "Joint Profits",
       alpha = "Estimates",
       title = "Joint Profits") +
  theme_bw()

# save
# ggsave("profits_joint.png", device = png, width = 6, height = 4,
#        path = "C:/Users/narin/Dropbox/TEACHING/CACI/CACI_WS2122/Slides/Slides_new/8_Conjoint Analysis/graphics")


# How do profits for each alternative look like?
head(ms)

# plot
ggplot(data = ms, aes(x = price, y = profit, 
                      color = as.factor(Alt),
                      alpha = variable)) +
  geom_point() + geom_line() +
  scale_alpha_manual(values = c(1, 0.3)) +
  labs(x = "Price for Alt 3 (114EUR for Alt1 and 114EUR for Alt2)",
       y = "Profits",
       color = "Alternative",
       alpha = "Estimates",
       title = "Profits per Product") +
  theme_bw()

#save
# ggsave("profits_alt.png", device = png, width = 6, height = 4,
#        path = "C:/Users/narin/Dropbox/TEACHING/CACI/CACI_WS2122/Slides/Slides_new/8_Conjoint Analysis/graphics")
