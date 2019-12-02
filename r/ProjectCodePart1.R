set.seed(10)

#Part 1: Convergence in Probability
n = c(1:50)
N = 1000

#Initiate data structures for empicial data, minimums, and convergence probabilities
p1data = list()
p1min = list()
p1convprob = list()

for (i in 1:length(n)) {
  p1data[[i]] = matrix(0, nrow = N, ncol = n[i])
  p1min[[i]] = matrix(0, nrow = N, ncol = 1)
  p1convprob[[i]] = 0
}

#Generate values of the exp(1) distribution and find the minimum for each sample
for (i in 1:length(n)) {
  for (j in 1:N) {
    p1data[[i]][j,] = rexp(n = n[i], rate = 1)
    p1min[[i]][j,] = min(p1data[[i]][j,])
  }
}

#Set epsilon as 0.05 and find the proportion of abs(minimum minus 0) within this epsilon for sample sizes 1 thru 50
eps = 0.05
for (i in 1:length(n)) {
  p1convprob[[i]] = sum(abs(p1min[[i]]-0) < eps)/N
}

#Plot sample size vs. probability computed above
plot(x = 1:length(n), y = p1convprob, main = "Showing That Minimum of exp(1) Distribution Approaches Zero With Increasing Sample Size",
     xlab = "Sample Size", ylab = "Probability of Minimum Within 0.05 of Zero", ylim = c(0.0, 1.1), cex = 0.5)
abline(h=1, col = "blue")

#Plot distribution of minimums for each sample size
plotdata = matrix(0, nrow = 0, ncol = 2)
for (x in 1:length(n)) {
  plotdata = rbind(plotdata, cbind(rep(x,N), p1min[[x]]))
}

plot(plotdata, main = "Distribution of Minimums With Increasing Sample Size",
     xlab = "Sample Size", ylab = "Sample Minimums", cex = 0.5)
