source("tree-properties.R")
source("yule.R")
library(rbenchmark)
library(parallel)
library("ape")

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

cl = makeCluster(4)
#I = c(50,100,200,500)
I = c(50)
mat = matrix(NA, ncol = 4, nrow = length(I)*4)

df=data.frame(test=character(length(I)*4),elapsed=numeric(length(I)*4),relative=numeric(length(I)*4), N = numeric(length(I)*4))
test = rep(NA, length(I)*4)
elapsed = rep(NA, length(I)*4)
relative = rep(NA, length(I)*4)
N = rep(NA, length(I)*4)

count = 0
for(i in I){
  clusterExport(cl, c("i","N","deg","num","penlen","len","degreeOfBalance","numOfBranches","lengthOfPendants","tree","length.phylo","extract.clade"))
  non = benchmark(replications=i, deg(cl,tree, N),num(cl,tree, N),penlen(cl,tree, N),len(cl,tree, N), columns = c("test","elapsed","relative"))
  count = count + 1
  df[count,] = data.frame(as.character(non$test[1]),non$elapsed[1],non$relative[1], i)
  # count = count + 1
  # df[count,] = c(as.character(non$test[2]),non$elapsed[2],non$relative[2], i)
  # count = count + 1
  # df[count,] = c(as.character(non$test[3]),non$elapsed[3],non$relative[3], i)
  # count = count + 1
  # df[count,] = c(as.character(non$test[4]),non$elapsed[4],non$relative[4], i)
}  
stopCluster(cl)


df[1,] = data.frame(test=non$test[1],elapsed=non$elapsed[1],relative=non$relative[1], N=i)


m$test[4]



