jukes_cantor = function(tree, alpha, columns){

  branching_Rec = function(x_v, alpha, t){  
    p = (1/4)*(1-exp(-4*alpha*t))
    p1 = (1/4)*(1+3*exp(-4*alpha*t))
    child = rep(NA, length(x_v))
    for(ip in 1:length(x_v)){
      if(x_v[ip]==1){
        child[ip] = sample(1:4, size=1, prob = c(p1,p,p,p))
      }else if(x_v[ip]==2){
        child[ip] = sample(1:4, size=1, prob = c(p,p1,p,p))
      }else if(x_v[ip]==3){
        child[ip] = sample(1:4, size=1, prob = c(p,p,p1,p))
      }else{
        child[ip] = sample(1:4, size=1, prob = c(p,p,p,p1))
      }
    }
    return(child)  
  }

  # tree = rtree(4)
  # plot(tree)
  # nodelabels(frame="none")
  # 
  columns = 5
  alpha = 0.2
  x_v = sample(4, columns, replace=TRUE)
  
  num_of_leaves = length(tree$tip.label)
  root = num_of_leaves+1

  Matrix = matrix(data=NA, nrow = length(tree$edge.length)+1, ncol = columns+2)
  Matrix[1,-c(1,2)] = x_v
  Matrix[,1] = c(root, tree$edge[,2])
  Matrix[1:length(tree$edge.length)+1,2] = tree$tip.label[tree$edge[,2]]
  t = tree$edge.length

  
  for(i in 1:length(tree$edge.length)){
    x_w1 = branching_Rec(Matrix[i,-c(1,2)], t[i], alpha=alpha)
    Matrix[i+1,-c(1,2)] = x_w1
  }
  
  alignmentMatrix = Matrix[as.numeric(Matrix[,1])<=num_of_leaves,]
  
  return(alignmentMatrix)
  # mp = sapply(1:length(tree$edge.length), function(i){
  #   branching_Rec(alignmentMatrix[i,], t[i])
  # }, simplify = "array")
  # 
  # t(mp)
}

hammingDistance = function(alignmentMatrix){
  hamDistMat = matrix(data=NA, ncol=nrow(alignmentMatrix), nrow=nrow(alignmentMatrix))
  dimnames(hamDistMat) = list(alignmentMatrix[,2],alignmentMatrix[,2])
  for(i in 1:nrow(alignmentMatrix)){
    for(j in 1:nrow(alignmentMatrix)){
      s = sum(alignmentMatrix[i,-c(1,2)] != alignmentMatrix[j,-c(1,2)])
      hamDistMat[i,j] = s
    }
  }
  diag(hamDistMat) = NA
  return(hamDistMat)
}





  tree = rtree(10)
  plot(tree)
  nodelabels(frame="none")
  align = jukes_cantor(tree, alpha = 0.5, 5)


  data("bird.orders")
  tree = bird.orders
  plot(tree)
  nodelabels(frame="none")
  
  align = jukes_cantor(tree, alpha = 0.001, columns = 10000)
  alignTom = jukes.cantor(tree, alpha = 0.001, col = 10000)
  hamDist = hammingDistance(align)
  
  
  which.min(hamDist)
  hamDist[242]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  