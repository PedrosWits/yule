getTimes = function(n=10, lambda=0.5) {
  l = sapply(1:n, function(i) rexp(1, rate = i*lambda))
  list(length=l, time=cumsum(l))
}

buildTree = function(n=10, DEBUG=FALSE) {
  tree = rep(0, (2*n - 1))
  root = sample(length(tree), 1)
  tree[root] = root
  
  for(s in 2:n) {
    notZero = which(tree != 0)
    noChilds = notZero[!(notZero %in% tree)]
    # Root case
    if(length(noChilds) == 0) {
      noChilds = notZero
    }
    # Sample behavior undesired
    if(length(noChilds) > 1) {
      parent = sample(noChilds, 1)  
    } else {
      parent = noChilds
    }
    
    childs = sample(which(tree==0), 2)
    tree[childs] = parent
    
    if(DEBUG) {
      message(cat("candidates = ", noChilds))
      message(cat("Childs = ", childs))
      message(cat("Parent = ", parent))
      message(cat("Tree = ", tree))  
    }
  }
  return(tree)
}

getParent = function(tree, index){
  tree[index]
}

getChild = function(tree, index){
  which(tree == index)
}

isExtant = function(tree, index) {
  length(getChild(tree, index)) == 0
}

giveMeAYule = function(n, lambda=0.5) {
  
  
  yule = data.frame(Name   = as.character(1:n), 
                    Parent = parents,
                    UChild = uchilds,
                    LChild = lchilds,
                    Length = lengths)
  return()  
}

howManySpecies = function(yule, t) {
  
}