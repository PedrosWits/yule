source("yule.R")
library(ggplot2)
library(reshape2)
library(plyr)

# # Representative Case 
n = 6

order =   c(5, 5,  3,  3,  4,  4, 0, 1, 2, 2,  1)
tree =    c(9, 9, 10, 10, 11, 11, 7, 7, 8, 8,  7)
indexes = c(1, 2,  3,  4,  5,  6, 7, 8, 9, 10, 11)

births =  c(0, 10, 75, 90, 140)
expected_lengths = c(NA, NA, NA, NA, NA, NA, NA, 10, 130, 80, 75)
expected_l_chars = c("T-140", "T-140",  # Leaves 1 and 2 = NA or 0
                     "T-90", "T-90",    # Leaves 3 and 4 = (140 - 90)
                     "T-75", "T-75",    # Leaves 5 and 6 = (140 - 75)
                     "NA", 
                     "10-0",   # Edge 8  (was born at 0, speciated at 10)
                     "140-10", # Edge 9  (was born at 10, speciated at 140)
                     "75-10",  # Edge 10 (was born at 10, speciated at 90)
                     "")       # Edge 11 (was born at 0, speciated at 75)

## Part 1 - Question iii

n = 10
lambda=0.5
four_yays = lapply(1:4, function(i) evolutionOf(yay(n, lambda)))
four_yays = rbind.fill(four_yays)
four_yays$group = ((as.numeric(rownames(four_yays)) - 1) %/% n) + 1

ggplot(four_yays, aes(x = tstep, y = nlineages))   +
  geom_step(aes(colour=factor(group))) +
  facet_wrap(~ group, ncol=2) + 
  ylab("Number of Lineages") +
  xlab("Time Steps") +
  theme_bw() +
  scale_colour_discrete(guide = FALSE)

## Part 2 - Question iii
n = 10
lambda =  0.5
par(mfrow=c(2,2))

for (i in 1:4) {
  phylo = yaPhylo(n, lambda)
  plot(phylo)
  title(main=paste("Tree", i),
        sub=paste("Phylogenetic Diversity:", round(length(phylo),2)),
        line = 1)
}
