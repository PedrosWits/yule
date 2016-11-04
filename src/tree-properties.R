library("ape")

degreeOfBalance = function(tree){
  num_of_leaves = length(tree$tip.label)
  root_node = num_of_leaves+1
  c1 = tree$edge[which(tree$edge==root_node)[1],2]
  c2 = tree$edge[which(tree$edge==root_node)[2],2]
 
  if((c1<=num_of_leaves && c2<=num_of_leaves+1) || ((c2<=num_of_leaves && c1<=num_of_leaves+1))){
    ret = c(1,1)
    return(ret)
  }else if(c1<=num_of_leaves || c2<=num_of_leaves){
    desc1 = 1
    desc2 = num_of_leaves-1
    ret = c(desc1, desc2)
    return(ret)
  }else{
    sub_tree1 = extract.clade(tree, c1)
    sub_tree2 = extract.clade(tree, c2)
    desc1 = length(sub_tree1$tip.label)
    desc2 = length(sub_tree2$tip.label)
    ret = c(desc1,desc2)
    return(ret)
  }
}

numOfBranches = function(tree){
  num_of_leaves=length(tree$tip.label)
  root = num_of_leaves+1
  iterateBranch = function(tree, up){
    count = 0
    while(up!=root){
      up = tree$edge[which(tree$edge[,2]==up)]
      count = count + 1
    }
    return(count)
  }
  store = sapply(1:num_of_leaves, function(i) iterateBranch(tree, i))
  names(store) = tree$tip.label[1:num_of_leaves]
  return(store)
}

lengthOfPendants = function(tree){
  num_of_leaves=length(tree$tip.label)
  penLength = rep(NA, num_of_leaves)
  penLength = tree$edge.length[1:num_of_leaves]
  return(penLength)
}




