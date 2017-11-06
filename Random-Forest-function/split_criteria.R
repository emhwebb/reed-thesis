#splitting_criterias for random forest 
library(nnet)
library(mvtnorm)

#generally, GINI Impurity for classification and RSS for regression

#note that we assume that first column of the data is the response
#the example here is to take a multivariate normal with 20 predictors and have the response 
#be the sum of the squares
p <- 10
sigma <- diag(p)
x <- rmvnorm(n = 20, mean = rep(0, p), sigma = sigma)

test_function <- function(data){
  sum(data)^2
}

y <- apply(x, 1, test_function)

d <- data.frame

#given split variable and split point
#computes left and right node rss 
LR_rss <- function(data, split_var, split_pt){
  rss_vect <- numeric()
  split_data <- split(data, data[,split_var]<data[,split_var][split_pt])
  #note first dataframe in split_data is the right side data
  #and second dataframe in split_data is the left side data
  Ldata <- split_data[[2]] 
  Rdata <- split_data[[1]]
  rss_vect <- c(sum((Ldata[,1]-mean(Ldata[,1]))^2), sum((Rdata[,1]-mean(Rdata[,1]))^2))
  rss_vect
}
                   
#choosing best split point for the given splitting variable given current node rss
#outputs position of the best split point
point_chooser <- function(data, split_var, current_rss){
  current_best <- rep(TRUE, 4)
  
  for(i in 1:(nrow(data)-1)){
    LR_vals <- LR_rss(data, split_var, i+1)
    if(current_rss-sum(LR_vals)<current_best[4]) 
      current_best <- c(i, LR_vals, current_rss-sum(LR_vals))
  }
  current_best
}


#chooses best splitting variable using point_chooser
#outputs optimal splitting variable and splitting point
split_chooser <- function(data, current_rss){
  current_best <- rep(TRUE, 5)
  
  for(i in 1:(ncol(data)-1)){
      next_vector <- point_chooser(data, i+1, current_rss)
      if(next_vector[4]<current_best[5])
        current_best <- c(i, next_vector)
      }
  
  #note should think of actual position of var_index as being var_index+1
  
  current_best
}

split_chooser(d,k)



splitvarst <- matrix(data = NA, nrow = 9, ncol = 3)
for(i in 1:9){
  splitvarst[i,] <- point_chooser(d, i+1, k)
}

split_rss <- matrix(data = NA, nrow = 19, ncol = 3)

for(i in 1:19){
  LR_vals <- LR_rss(d, 10, i+1)
  split_rss[i,] <- c(LR_vals, k-sum(LR_vals))
}

split_rss[1,] <- c(LR_rss(d, 2, 1+1), 
                   k-sum(split_rss[1,][1:2]))
c(19, split_rss[19,])


#split_chooser <- function(data, current_rss){
 # current_best
  
  #for(i in 1:(ncol(data)-1)){
  #  splitvars[i,] <- point_chooser(data, i+1, current_rss)
  #}
  #note should think of actual position of var_index as being var_index+1
  #var_index <- which.is.max(-splitvars[,3])
  #c(var_index, splitvars[var_index,])
#}