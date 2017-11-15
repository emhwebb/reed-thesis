#Picks the mtry-element subset of the p variables for the random forest algorithm to choose 
#which variable and point to split on for the bth decision tree.

#split_choice is a function of 
#(i) the data. we assume that the data is n-entries of (p+1)-dimensional entries,
#where p of the dimensions are predictors and 1 of the dimensions is a response.
#For now assume that the data has been treated such that only the p-predictors can be chosen from
#will consider if necessary to consider p+1 columns which include the response
#(ii) mtry, which is the number of variables to randomly select from the data
#at each iteration of the algorithm

#outputs the indices of the predictors that can be split on for the decision tree

#note that this is only to be run when randomization is required.
#if mtry=p, then don't run split_choice 

split_choice <- function(data, mtry){
    split_index <- sample(1:ncol(data), mtry, replace = FALSE)
    split_index
}

#test to show split_choice works as we want it to
d <- data.frame(1:20, 21:40, 41:60, 61:80, 81:100, 101:120, 121:140, 141:160, 161:180, 181:200)
split_test <- split_choice(data = d, mtry = 4) 
