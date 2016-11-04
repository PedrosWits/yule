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


# Task 3 ii)

tree = rtree(16)
plot(tree)
nodelabels(frame="none")
balance = degreeOfBalance(tree)
sample(balance, 1)
extants = numOfBranches(tree)
sample(extants, 1)
penEdge = lengthOfPendants(tree)
sample(penEdge, 1)
