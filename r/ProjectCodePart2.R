set.seed(10)

#Part 2: Convergence in Distribution
lambda = c(1,5,25)
n = c(5,10,30,100)
N = 50000

#Initiate data structures for empicial data, means, empirical & derived probabilities
p2data = list()
p2mean = list()
p2empprob = list()
p2derprob = list()

for (l in 1:length(lambda)) {
  p2data[[l]] = list()
  p2mean[[l]] = list()
  p2empprob[[l]] = list()
  p2derprob[[l]] = list()
  for (i in 1:length(n)) {
    p2data[[l]][[i]] = matrix(0, nrow = N, ncol = n[i])
    p2mean[[l]][[i]] = matrix(0, nrow = N, ncol = 1)
    p2empprob[[l]][[i]] = 0
    p2derprob[[l]][[i]] = 0
  }
}

#Generate values of the Poi(lambda) distributions and compute their means for every combination of lambda and sample size
for (l in 1:length(lambda)) {
  for (i in 1:length(n)) {
    for (j in 1:N) {
      p2data[[l]][[i]][j,] = rpois(n = n[i], lambda = lambda[l])
    }
    p2mean[[l]][[i]] = rowMeans(p2data[[l]][[i]])
  }
}

#Plot empirical vs. derived (normal) distributions for means
for (l in 1:length(lambda)) {
  for (i in 1:length(n)) {
    mu = lambda[l]
    sigma = sqrt(lambda[l]/n[i])
    start = min(p2mean[[l]][[i]])
    end = max(p2mean[[l]][[i]])
    steps = 1/n[i]
    hist(p2mean[[l]][[i]], breaks = seq(from = start-steps/2, to = end+steps/2, by = steps), 
         main = sprintf("lambda = %d & n = %d", lambda[l], n[i]), freq = FALSE, xlab = "Sample Mean")
    curve(1/(sigma*sqrt(2*pi))*exp(-(x-mu)^2/(2*sigma^2)), from = start-steps/2, to = end+steps/2, 
          add = TRUE, lwd = 2, col = "blue")
  }
}

#For every lambda/sample size combination, compute proportion of empirical means ge lambda+2*sqrt(lambda/n) and compare to normal probability
for (l in 1:length(lambda)) {
  for (i in 1:length(n)) {
    mu = lambda[l]
    sigma = sqrt(lambda[l]/n[i])
    steps = 1/n[i]
    p2empprob[[l]][[i]] = sum(p2mean[[l]][[i]] >= mu+2*sigma-steps/2)/N
    p2derprob[[l]][[i]] = pnorm(q = mu+2*sigma, mean = mu, sd = sigma, lower.tail = FALSE)
    print(sprintf("For lambda = %-2d and n = %-3d the empirical prob is %-1.5f and the normal prob is %-1.5f",
                  lambda[l], n[i], p2empprob[[l]][[i]], p2derprob[[l]][[i]]))
  }
}
