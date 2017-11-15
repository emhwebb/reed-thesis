#splitting_criterias for random forest 
library(nnet)
library(mvtnorm)
library(compare)

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

d <- data.frame(y,x)

d_rssv <- d[,1]
d_rssv <- d_rssv-mean(d_rssv)
d_rssv <- d_rssv^2
d_rss <- sum(d_rssv)

#given split variable and split point
#computes left and right node rss 
LR_rss <- function(data, split_var, split_pt){
  rss_vect <- numeric()
  split_data <- split(data, data[,split_var]<data[,split_var][split_pt])
  #note first dataframe in split_data is the right side data
  #and second dataframe in split_data is the left side data
  Ldata <- split_data$"FALSE"
  Rdata <- split_data$"TRUE"
  
  #need this condition because splits at minimum or maximum values of 
  #the data are disallowed. 
  if(is.null(split_data$"FALSE") | is.null(split_data$"TRUE")){ 
    L_rss <- 0
    R_rss <- 0}
   else{  
     L_rss <- sum((Ldata[,1]-mean(Ldata[,1]))^2)
     R_rss <- sum((Rdata[,1]-mean(Rdata[,1]))^2)}
  
  rss_vect <- c(L_rss, R_rss)
  rss_vect
}

#choosing best split point for the given splitting variable given current node rss
#outputs position of the best split point
point_chooser <- function(data, split_var, current_rss){
  mat <- matrix(NA, nrow = nrow(data), ncol = 4)
  
  for(i in 1:(nrow(data))){
    LR_vals <- LR_rss(data, split_var, i)
    mat[i,] <- c(i, LR_vals, current_rss-sum(LR_vals))
  }
  var_index <- which.is.max(-mat[,4])
  c(mat[var_index,])
  #output is: c(split_pt, l_rss, r_rss, rss-(l_rss+r_rss))
}

#chooses best splitting variable using point_chooser
#outputs optimal splitting variable and splitting point
split_chooser <- function(data, current_rss){
  current_best <- rep(TRUE, 5)
  mat <- matrix(NA, nrow = (ncol(data)-1), ncol= 5)
  
  for(i in 1:(ncol(data)-1)){
      next_vector <- point_chooser(data, i+1, current_rss)
      mat[i,] <- c(i, next_vector)
      }
  #note should think of actual position of var_index as being var_index+1
  var_index <- which.is.max(-mat[,5])
  mat[var_index,]
  #c(split_var, split_pt, l_rss, r_rss, rss-(l_rss+r_rss))
}

split_chooser(d,d_rss)

point_chooser(d, 2, d_rss)

#split_chooser <- function(data, current_rss){
 # current_best
  
  #for(i in 1:(ncol(data)-1)){
  #  splitvars[i,] <- point_chooser(data, i+1, current_rss)
  #}
  #note should think of actual position of var_index as being var_index+1
  #var_index <- which.is.max(-splitvars[,3])
  #c(var_index, splitvars[var_index,])
#}


#if((current_rss-sum(LR_vals))<current_best[4]) 
#  current_best <- c(i, LR_vals, current_rss-sum(LR_vals))