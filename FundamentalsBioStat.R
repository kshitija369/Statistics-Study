
# This computes Pr(X <= 4)
pbinom(4, 100, 0.009)

#This computes Pr(X > 5)
1 - pbinom(4, 100, 0.009)

# Table 4.8 Table of binomial probabilities for n = 100, p =0.05, k = 0,1,...,5
k <- 0:5
prob <- dbinom(k, 100, 0.05)
results <- rbind(round(k,0), prob)
rownames(results) <- c("k", "Pr(X=k)")
results

barplot(prob, main="Display of Binomial Probabilities \n N=100, p=0.05, for k =0, 1, 2, 3, 4, 5", ylab = "Probabilities", ylim = c(0,0.2), names.arg = c("0", "1", "2", "3", "4", "5"))
