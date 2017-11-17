#grows binary tree given splitting variable and split point
#do so recursively

#init_list is called to determine starting values for the process 
init_list <- function(data){
    #compute rss of the data and mean of the response in the data, put in NA for l_rss/r_rss values
    #also put in NA for split_pt/split_var for this first split
   pred <- mean(data[,1])
   rss <- sum((data[,1]-pred)^2)
   #output is in following order:
   #c(pred, rss, l_rss, r_rss, split_pt, split_var, split_val, node_size, left or right node 
   #(code a 2 or 3))
   list(c(pred, rss, NA, NA, NA, NA, NA, nrow(data)), NA)
}

#use nested lists as the storing data structure for this 
#ex: tree_list <- c(tree_list, l_list, r_list)
#where l_list <- list(Lnode_info) and r_list <- list(Rnode_info)

bin_tree <- function(data, tree_list, min_node_size){
    #check if data is null, then terminate tree growing process if it is.
    #check if init_list is null.
    #if null, then run init_list to obtain starting values.
    is_null_data <- is.null(data)
    is_null_list <- is.null(tree_list)
    if(is_null_list){
      tree_list <- init_list(data)
    }
    
    #should the tree split based on the current node size?
    should_split <- min_node_size < tree_list[[8]]
    
    
    
    #figure out where next split should be.
    split_choice <- split_chooser(data, data_rss)
    #output of split_chooser:
    #note should think of actual position of var_index as being var_index+1
    #while position of split_pt is the one given
    #c(split_var, split_pt, l_rss, r_rss, rss-(l_rss+r_rss))
    
    split_var <- split_choice[1]+1
    split_pt <- split_choice[2]
    split_data <- split(data, data[,split_var]<data[,split_var][split_pt])
    Ldata <- split_data$"FALSE"
    Rdata <- split_data$"TRUE"
    l_rss <- split_choice[3]
    r_rss <- split_choice[4]
    #need to predict what the value of y is here, also need to include information on nodal 
    #size in tree_list
    
    #c(pred, rss, l_rss, r_rss, split_pt, split_var, split_val, node_size, l or r node)
    l_list <- c(l_pred, l_rss, split_pt, split_var, split_val, node_size, 2)
    r_list <- c(r_pred, r_rss, split_pt, split_var, split_val, node_size, 3)
    
    bin_tree(Ldata, l_list, min_node_size)
    bin_tree(Rdata, r_list, min_node_size)
  }