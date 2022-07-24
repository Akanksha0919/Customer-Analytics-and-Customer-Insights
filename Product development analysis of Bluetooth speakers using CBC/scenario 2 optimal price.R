getwd()
install.packages("pacman")

# (Install and) Load the necessary packages and data ----------------------------
pacman::p_load(reshape2, ggplot2, fastDummies, mlogit, gmnl, MASS)

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
cbc = choiceData
head(cbc)
dim(cbc)

# load the results
mxl_diag600 = get(load("mxl_diag600.RData"))
names(cbc)[names(cbc) == 'cs'] <- 'Task'
names(cbc)[names(cbc) == 'alt'] <- 'Alt'
names(cbc)[names(cbc) == 'none'] <- 'None'
names(cbc)[names(cbc) == 'priceL'] <- 'Price'


# Compute individual-level estimates ------------------------------------------
betai = data.frame(effect.gmnl(mxl_diag600)$mean)
betai$id = unique(cbc$id)
head(betai)
dim(betai)
names(betai)[names(betai) == 'none'] <- 'None'
names(betai)[names(betai) == 'priceL'] <- 'Price'

# Market Simulation ------------------------------------------------------------
# function for predictions (adjusted to incorporate more than one choice task)
# beta should be a matrix each row representing a respondent, columns - parameters
# with the names corresponding to the names in design matrix X
# param is vector with the names of parameters
predict.mxl = function(X, beta) {
  
  counter = X[, c("Task", "Alt")]
  
  # as id counter initial may not be sequential, 
  # we create a new one that is sequential
  ids = data.frame(id_new = 1:length(beta$id))
  
  param = names(beta)[names(beta) != "id"]
  beta = as.matrix(beta[, param])
  
  I = nrow(beta)
  
  data = NULL
  for (i in 1:I) {
    xi = as.matrix(X[, param])
    data[[i]] = X[, param]
    
    # add counters
    data[[i]]$id_new = i 
    data[[i]] = cbind(counter, data[[i]])
    
    # Compute V_ijt (utility)
    data[[i]]$u = c(xi %*% beta[i,]) 
  }
  data = do.call(rbind, data) 
  
  # merge with ids
  data = merge(ids, data, by = "id_new", all = TRUE)
  
  # compute exp(V_ijt)
  data$exp_u = exp(data$u)
  
  # compute sum of exp(V_ijt) across J alternatives for each id and Task
  data$sum_exp_u = rep(c(tapply(data$exp_u, list(data$Task, data$id), sum)), each = J) 
  
  # compute choice probs
  data$p_hat = data$exp_u/data$sum_exp_u
  
  # max util rule: predict chosen alternative
  data$choice_hat = rep(c(tapply(data$p_hat, list(data$Task, data$id), which.max)), each = J)
  data$choice_hat = ifelse(data$choice_hat == data$Alt, 1, 0)
  
  return(data)
}

#Scenario 2:
#a.	Firm 1, product 1: Sound quality of 5.0 stars, weight of 600grams, battery life of 12 hours
#b.	Firm 1, product 2:Sound quality of 5.0 stars, weight of 600grams, battery life of 16 hours
#c.	Firm 2, product 3 : Sound quality of 4.0 stars, weight of 400grams, battery life of 16 hours
x2 = data.frame(None = c(0,0, 0, 1),
                Price = c(NA, NA, NA, 0),
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
x2

# We can start with some initial price for firm 1 product 1 and firm 2 product 3 as 113 and 97 euros
#Setting them to the price obtained when only two products one for each firm were available
# Firm 1 will respondent by setting a price which optimizes its overall profits from both produts 1 and 2
# given the 90EUR price of the competition
# I will write this in general terms, so it is easy to switch the focal firm

# set the marginal costs
mc = c(75, 75,70, 0)
# set the market size
N = 400

price_vec = c(113, NA,97, 0) # initial prices

# which one is the focal firm?
focal_firm = which(is.na(init_price) == TRUE)

# Let's generate sets with varying prices
# the price for the focal firm changes (we generate sequence of prices)
# and the price for the other firm is fixed
J = nrow(x2)
price = seq(70, 150, by = 1)
T = length(price)

X = NULL
for(t in 1:T){
  temp = x2
  temp[, "Price"] = price_vec # set prices based on the init_price vector
  temp[focal_firm, "Price"] = price[t] # update price value for focal firm
  temp$Alt = 1:J # create Alt variable
  temp$Task = t # create Task variable
  X[[t]] = temp
}
X = do.call(rbind, X)

head(X)
dim(X) # J*T

# predict choices and choice probs based on individual-level estimates
# vector of parameters
pred_indiv = predict.mxl(X, betai)
head(pred_indiv)
dim(pred_indiv)

# Note each task indicates a specific price for the focal firm with price for the other fixed
# we can compute market shares of each alternative (aggregate across respondents) for each 
# Task, i.e., price of the focal firm
ms = aggregate(pred_indiv$choice_hat, 
               by = list(Alt = pred_indiv$Alt, Task = pred_indiv$Task),
               mean)
names(ms) = c("Alt", "Task", "ms")

# let's add the price of the focal firm
price_df = data.frame(Task = 1:T,
                      price_seq = price)

ms = merge(ms, price_df, by = c("Task"), all.x = TRUE)

# define the prices of all alternatives: 1, 2, and 3 (the none)
ms$price = init_price
ms$price = ifelse(ms$Alt == focal_firm, ms$price_seq, ms$price)

# add the marginal costs
ms$mc = mc

# compute profit
ms$profit = N * ms$ms * (ms$price - ms$mc)

# At what price is the profit of the focal firm maximized?
maximum = subset(ms, Alt == focal_firm)
maximum$price_star = ifelse(maximum$profit == max(maximum$profit), 1, 0)

subset(maximum, price_star == 1)
maximum = subset(maximum, price_star == 1)$price_seq
maximum

optimal = subset(ms, price_seq == maximum)
optimal


# to make things more optimal we can write the upper part as a function, so we can change 
# the focal firm, initial prices, marginal costs, and market size
# findOptimum() function
#   X           is the choice scenario (we have defined that in x object)
#   mc          is the vector of marginal costs
#   N           is the market size (by default is 1)
#   init_price  is the vector of initial prices, for the focal firm for which 
#               you are optimizing should be NA.
#   price_seq   is the sequences of prices out of which you want to find the optimum
#   beta        is the matrix of estimates (each row represents each individual)
findOptimum = function(x, mc, N = 1, init_price, price_seq, beta){
  # set the the focal firm
  focal_firm = which(is.na(init_price) == TRUE)
  J = nrow(x)
  T = length(price_seq)
  
  X = NULL
  for(t in 1:T){
    temp = x
    temp[, "Price"] = init_price # set prices based on the init_price vector
    temp[focal_firm, "Price"] = price_seq[t] # update price value for focal firm
    temp$Alt = 1:J # create Alt variable
    temp$Task = t # create Task variable
    X[[t]] = temp
  }
  X = do.call(rbind, X)
  
  # predict choices and choice probs based on individual-level estimates
  # vector of parameters
  pred_indiv = predict.mxl(X, beta)
  
  # Note each task indicates a specific price for the focal firm with price for the other fixed
  # we can compute market shares of each alternative (aggregate across respondents) for each 
  # Task, i.e., price of the focal firm
  ms = aggregate(pred_indiv$choice_hat, 
                 by = list(Alt = pred_indiv$Alt, Task = pred_indiv$Task),
                 mean)
  names(ms) = c("Alt", "Task", "ms")
  
  # let's add the price of the focal firm
  price_df = data.frame(Task = 1:T,
                        price_seq = price_seq)
  
  ms = merge(ms, price_df, by = c("Task"), all.x = TRUE)
  
  # define the prices of all alternatives: 1, 2, and 3 (the none)
  ms$price = init_price
  ms$price = ifelse(ms$Alt == focal_firm, ms$price_seq, ms$price)
  ms$mc = mc # add the marginal costs
  ms$profit = N * ms$ms * (ms$price - ms$mc) # compute profit
  
  # At what price is the profit of the focal firm maximized?
  maximum = subset(ms, Alt == focal_firm)
  maximum$price_star = ifelse(maximum$profit == max(maximum$profit), 1, 0)
  maximum = subset(maximum, price_star == 1)$price_seq
  
  optimal = subset(ms, price_seq == maximum)
  rownames(optimal) = NULL
  return(optimal)
}

mc = c(75, 75,70, 0)  #set the market size
N = 400
price_vec = c(113, NA,97, 0) # initial prices

# initial iteration (r = 1) ---
focal = which(is.na(price_vec) == TRUE) # focal firm
price_seq = seq(mc[focal], 150, by = 1) # the min in the price sequence is the marginal cost

iter1 = findOptimum(x2, mc, N, 
                    init_price = price_vec, 
                    price_seq = price_seq,
                    beta = betai)
iter1


# Firms will continue reacting to until both get to point where changing the price 
# doesn't make sense. That is we will find that point by iterating and getting no 
# change in price from one to the next iteration. What would this point be?

# We can do this using a for loop 
# Note: this is for 2 + none alternatives, you need to adjust the code to 
# work for more alternatives
res = NULL     # initialize
diff = NA      # initialize
max_iter = 100 # you can change this


mc = c(75, 75,70, 0) # set the market size
N = 400
price_vec = c(113, NA,97, 0) # initial prices

# which one is the focal firm?
focal_firm = which(is.na(init_price) == TRUE)
# the loop will stop when max_iter is reached or earlier 
# when the difference between prices in r and r-1 is 0
for(r in 1:max_iter){
  if(r == 1){
    price_temp = price_vec
  }else{
    # update price_vec
    if(r %in% seq(3, 100, by = 2)){ # for these iterations alt 2 is the focal option
      price_temp = c(res[[r-1]][res[[r-1]]$Alt == focal, "price"], NA, res[[r-1]][res[[r-1]]$Alt == focal, "price"],0)
      
    }else if(r %in% seq(2, 100, by = 2)){ # for these iterations alt 1 is the focal option
      price_temp = c(NA, res[[r-1]][res[[r-1]]$Alt == focal, "price"], res[[r-1]][res[[r-1]]$Alt == focal, "price"],0)
      
    }
    else if(r %in% seq(4, 100, by = 2)){ # for these iterations alt 3 is the focal option
      price_temp = c(res[[r-1]][res[[r-1]]$Alt == focal, "price"],res[[r-1]][res[[r-1]]$Alt == focal, "price"],NA,0)
      
    }
  }
  
  # update focal and min in the price sequence
  focal = which(is.na(price_temp) == TRUE) 
  price_seq = seq(mc[focal], 100, by = 1) 
  
  res[[r]] = findOptimum(x2, mc, N, 
                         init_price = price_temp, 
                         price_seq = price_seq,
                         beta = betai)
  
  if(r == 1){
    diff[r] = NA
  }else{
    diff[r] = res[[r]][res[[r]]$Alt == focal, "price"] - 
      res[[r-1]][res[[r-1]]$Alt == focal, "price"]
  }
  
  if(r > 1 & diff[r] == 0){ break}
}

# how many iterations did it take?
r

# what are the prices we reach convergence, i.e., Nash equilibrium?
res
# Doesn't seem to converge with price difference 0. it can see that the minimum difference between prices we reach is 2
# Optimizing the above function with input variable which will accomodate the minimum price difference criteria
# In this case, the min_diff = 0 might be too low. We can set it to 2 for reaching convergence
res = NULL        # initialize
diff = NA          # initialize
max_iter = 100     # you can change this
min_diff = 2       # what should be the minimum difference between 
price_max = 150    # the maximum value the price can be (when generating sequences of prices)
price_interval = 1 # the interval by which price sequence should be built 

N = 400                  # define market size
mc = c(75, 75,70, 0)       # define the marginal costs
price_vec = c(113, NA,97, 0) # initial prices
# the loop will stop when max_iter is reached or earlier 
# when the difference between prices in r and r-1 is 0
for(r in 1:max_iter){
  if(r == 1){
    price_temp = price_vec
  }else{
    # update price_vec
    if(r %in% seq(3, 100, by = 2)){ # for these iterations alt 2 is the focal option
      price_temp = c(res[[r-1]][res[[r-1]]$Alt == focal, "price"], NA, res[[r-1]][res[[r-1]]$Alt == focal, "price"],0)
      
    }else if(r %in% seq(2, 100, by = 2)){ # for these iterations alt 1 is the focal option
      price_temp = c(NA, res[[r-1]][res[[r-1]]$Alt == focal, "price"], res[[r-1]][res[[r-1]]$Alt == focal, "price"],0)
      
    }
    else if(r %in% seq(4, 100, by = 2)){ # for these iterations alt 3 is the focal option
      price_temp = c(res[[r-1]][res[[r-1]]$Alt == focal, "price"],res[[r-1]][res[[r-1]]$Alt == focal, "price"],NA,0)
      
    }
  }
  # update focal and min in the price sequence
  focal = which(is.na(price_temp) == TRUE) 
  price_seq = seq(mc[focal], price_max, by = price_interval) 
  
  res[[r]] = findOptimum(x2, mc, N, 
                         init_price = price_temp, 
                         price_seq = price_seq,
                         beta = betai)
  
  if(r == 1){
    diff[r] = NA
  }else{
    diff[r] = abs(res[[r]][res[[r]]$Alt == focal, "price"] - 
                    res[[r-1]][res[[r-1]]$Alt == focal, "price"])
  }
  
  if(r > 1 & diff[r] == min_diff){ break}
}

# how many iterations did it take?
r

# what are the prices we reach convergence, i.e., Nash equilibrium?
res