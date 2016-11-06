source("tree-properties.R")
source("yule.R")
library(rbenchmark)
library(parallel)
library("ape")
library(ggplot2)

# tree = yaPhylo(n = 16, lambda = 2.5)
# plot(tree)
# nodelabels(frame="none")
# balance = degreeOfBalance(tree)
# sample(balance, 1)
# extants = numOfBranches(tree)
# sample(extants, 1)
# penEdge = lengthOfPendants(tree)
# sample(penEdge, 1)
# length(tree)



tree = yaPhylo(n = 16, lambda = 2.5)
plot(tree)
nodelabels(frame="none")



deg = function(cl,tree, N)
  parSapply(cl, 1:N, function(i) sample(degreeOfBalance(tree),1))
num = function(cl,tree, N)
  parSapply(cl, 1:N, function(i) sample(numOfBranches(tree),1))
penlen = function(cl,tree, N)
  parSapply(cl, 1:N, function(i) sample(lengthOfPendants(tree)))
len = function(cl,tree, N)
  parSapply(cl, 1:N, function(i) sample(length(tree),1))


calc_properties = function(dummy){
  tree = yaPhylo(n = 16, lambda = 2.5)
  d1 = sample(degreeOfBalance(tree),1) # Histogram
  d2 = sample(numOfBranches(tree),1) # Barplot 
  d3 = sample(lengthOfPendants(tree),1) # Barplot 
  d4 = sample(length(tree),1) # Histogram
  ret = c(d1,d2,d3,d4)
  return(ret)
}

cl = makeCluster(4)
clusterExport(cl, c("isExtant","loadSpecies","buildTree","yay","yaPhylo","calc_properties","N","degreeOfBalance","numOfBranches","lengthOfPendants","tree","length.phylo","extract.clade"))

  n = parSapply(cl, 1:1000, calc_properties)
stopCluster(cl)

par(mfrow=c(1,1))
n=t(n)
hist(n[,1], breaks = 50, main = "Diversity")
hist(n[,2], breaks = 50, main = "Num of Branches")
hist(n[,3], breaks = 50, main = "Pendant Length")
hist(n[,4], breaks = 50, main = "Length") 
n = as.data.frame(n)

library(gridExtra)

p1 = ggplot(data = n, aes(n[,1])) + geom_histogram() + ggtitle("Diversity") + labs(x="Value", y="Density")
p2 = ggplot(data = n, aes(n[,2])) + geom_histogram() + ggtitle("Number of Branches") + labs(x="Value", y="Density")
p3 = ggplot(data = n, aes(n[,3])) + geom_histogram() + ggtitle("Length of Pendant") + labs(x="Value", y="Density")
p4 = ggplot(data = n, aes(n[,4])) + geom_histogram() + ggtitle("Length") + labs(x="Value", y="Density")
grid.arrange(p1,p2,p3,p4)

mat = matrix(NA, ncol = 4, nrow = length(I)*4)

cl = makeCluster(4)
clusterExport(cl, c("i","N","deg","num","penlen","len","degreeOfBalance","numOfBranches","lengthOfPendants","tree","length.phylo","extract.clade"))
N=100000
deg = deg(cl,tree,N)
num = num(cl, tree, N)
penlen = penlen(cl,tree,N)
len = len(cl,tree,N)

stopCluster(cl)

hist(deg)
hist(num)
hist(penlen)
hist(len)

df[1,] = data.frame(test=non$test[1],elapsed=non$elapsed[1],relative=non$relative[1], N=i)


m$test[4]



