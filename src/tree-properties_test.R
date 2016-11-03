source("tree-properties.R")

par(mfrow=c(1,1))
tree = rtree(n=10)
plot(tree)
nodelabels(frame="none")
n = degreeOfBalance(tree)
n
o = numOfBranches(tree)
o
p = lengthOfPendants(tree)
p
