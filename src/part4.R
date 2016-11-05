# Function to compute hamming distance
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
  
# Function to computer Mutated Sequence
mutateSequence = function(sequence, alpha, ts){  
  p = (1/4) * (1 - exp(-4 * alpha * ts))
  p1 = (1/4) * (1 + 3 * exp(-4 * alpha * ts))
  newSeq = rep(NA, length(sequence))
  
  #sapply(sequence, )
  for(ip in 1:length(sequence)){
    if(sequence[ip] == 1){
      newSeq[ip] = sample(1:4, size=1, prob = c(p1,p,p,p))
    } else if(sequence[ip] == 2){
      newSeq[ip] = sample(1:4, size=1, prob = c(p,p1,p,p))
    } else if(sequence[ip] == 3){
      newSeq[ip] = sample(1:4, size=1, prob = c(p,p,p1,p))
    } else{
      newSeq[ip] = sample(1:4, size=1, prob = c(p,p,p,p1))
    }
  }
  return(newSeq)  
}

# Yet Another Jukes Cantor Model - returns Alignment Matrix
yaJC = function(tree, alpha = 0.5, ncol = 5, ntypes = 4) {
  n = length(tree$tip.label)
  root = n + 1
  rootSequence = sample(ntypes, ncol, replace=TRUE)
  
  alignment = matrix(data=NA, 
                     nrow = length(tree$edge.length) + 1,
                     ncol = ncol + 2)
  alignment[1, -c(1,2)] = rootSequence
  alignment[,1] = c(root, tree$edge[,2])
  
  treeFall = function(phylo, parent, parentSequence, alpha) {
    # Find the childs of parent
    childs = phylo$edge[phylo$edge[,1]==parent , 2]
    # If no child, return
    if(length(childs) == 0) {
      return()
    }
    
    for (child in childs) {
      # Get the length of this edge
      t = phylo$edge.length[phylo$edge[,2]==child]
      print(t)
      ## Compute child mutated sequence
      childSeq = mutateSequence(parentSequence, alpha, t)
      ## Act on matrix
      alignment[alignment[,1] == child, -c(1,2)] <<- childSeq
      ## Spawn new treeFall
      treeFall(phylo, child, childSeq, alpha)
    }
  }
  treeFall(tree, root, rootSequence, alpha)
  alignment = alignment[as.numeric(alignment[,1]) <= n,]
  # Get labels for second column
  labels = tree$tip.label[tree$edge[,2]]
  alignment[,2] = labels[!is.na(labels)]
  return(alignment)
}

tree=rtree(10)
plot(tree)
nodelabels(frame="none")
m = yaJC(tree)
ham = hammingDistance(m)



# Reset

data("bird.orders")
plot(bird.orders)  
nodelabels(frame="none")
m= yaJC(bird.orders, alpha=0.001, ncol=10000)
ham = hammingDistance(m)  
ham
  