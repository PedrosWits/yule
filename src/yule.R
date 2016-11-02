buildTree = function(n=10, lambda=0.5) {
  nspecies = (2*n - 1)
  #cols = c("parent", "child", "birth", "speciation", "length")
  cols = c("parent" , "birth", "speciation", "length")
  
  tree = matrix(NA, nrow = nspecies, ncol = length(cols))
  colnames(tree) = cols
  tree[,1] = 0
  cumt = 0
  
  for(k in 1:(n-1)) {
    if(k == 1) {
      root = n + 1
      tree[root, "parent"] = root
      parent = root
      t = 0
    } else {
      notZero = which(tree[,"parent"] != 0)
      noChilds = notZero[!(notZero %in% tree[,"parent"])]
      # Undesired Sampling behavior for length == 1
      if(length(noChilds) > 1) {
        parent = sample(noChilds, 1)  
      } else {
        parent = noChilds
      }
      t = rexp(1, rate = k*lambda) 
    }
    cumt = cumt + t
    
    childs = sample(which(tree[,"parent"]==0), 2)
    #tree[childs, "child"] = childs
    tree[childs, "parent"] = parent
    tree[childs, "birth"] = cumt
    tree[parent, "speciation"] = cumt
  }
  tree[, "length"] = tree[, "speciation"] - tree[, "birth"]
  return(tree)
}

isRoot = function(tree, index=1:nrow(tree)) {
  tree[index, "parent"] == index
}

isExtant = function(tree, index=1:nrow(tree)) {
  is.na(tree[, "speciation"])
}

loadSpecies = function(path="../aux/species.txt") {
  species = read.table(path, header=FALSE, sep = "+", stringsAsFactors = FALSE)$V1
  species[-which(species=="unavailable")]
}

# Yet Another Yule (YAY)
yay = function(n=10, lambda=0.5) {
  tree = buildTree(n)
  species = loadSpecies()
  if(length(species) > 0) {
    nomes = species[sample(1:length(species), nrow(tree))]  
  } else {
    nomes = paste("poney", 1:length(tree), sep="")
  }
  
  yule = data.frame(Name       = nomes,
                    ParentName = nomes[tree[, "parent"]],
                    Parent     = tree[, "parent"],
                    #Child      = nomes[tree[, "child"]],
                    #ChildName  = tree[, "child"],
                    isRoot     = isRoot(tree),
                    isExtant   = isExtant(tree, 1:nspecies),
                    Birth      = tree[, "birth"],
                    Length     = tree[, "length"],
                    Speciation = tree[, "speciation"])
  yule[yule$isRoot == TRUE, ]$ParentName = NA
  #class(yule) = "yay"
  return(yule)  
}

howManySpecies = function(yule, t) {
  
}


##

# 
# getLevel = function(tree, index) {
#   level = 0
#   parent = index
#   while(!isRoot(tree, parent)) {
#     parent = getParent(tree, parent)
#     level = level + 1
#   }
#   return(level)
# }
# 
# getLevels = function(tree) {
#   sapply(tree, function(i) getLevel(tree,i))
# }
# 
# 
# getChild = function(tree, index){
#   childs = which(tree[,"parent"] == index)
#   childs = childs[!isRoot(tree, childs)]
#   if(length(childs)==0) {
#     return(NA)
#   } else {
#     return(childs)
#   }
# }