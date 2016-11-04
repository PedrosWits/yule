jukes_cantor = function(tree, alpha, columns){
  tree = rtree(10)
    
  columns = 6
  alpha = 0.1
  root = sample(4, columns, replace=TRUE)
  x_v = root
  x_v
  
  num_of_leaves = length(tree$tip.label)
  root = num_of_leaves+1
  
   
  branching_Rec = function(v){  
    t = 1
    p = (1/4)*(1-exp(-4*alpha*t))
    p1 = (1/4)*(1+3*exp(-4*alpha*t))
    child = rep(NA, length(x_v))
    for(i in 1:length(x_v)){
      if(x_v[i]==1){
        child[i] = sample(1:4, size=1, prob = c(p1,p,p,p))
      }else if(x_v[i]==2){
        child[i] = sample(1:4, size=1, prob = c(p,p1,p,p))
      }else if(x_v[i]==3){
        child[i] = sample(1:4, size=1, prob = c(p,p,p1,p))
      }else{
        child[i] = sample(1:4, size=1, prob = c(p,p,p,p1))
      }
    }
    return(child)  
  }
  
  alignmentMatrix = matrix(data=NA, nrow = length(tree$edge.length)+1, ncol = 3)
  
  v = tree$edge[which(tree$edge==root)]
  v_length = tree$edge.length[v]
  
  alignmentMatrix[1,] = c(v, v_length, x_v)
  
  
  while(node != root){
    x_w1 = branching_Rec(v)
  }


  
  
}