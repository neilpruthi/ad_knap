### get functions
source('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/r/single_comparison.R', echo = TRUE)

### set probabilities of winning
probs <- matrix(NA, 100, 3)

### compute probabilities
for(i in 1:100) {
	probs[i, ] <- knapsack_compare(dists = list('Uniform', 'Uniform', 'Uniform'), params = list(c(min = -1, max = 1), c(min = -2, max = 2), c(min = -i, max = i)), reps = 1, iters = 100000, summary = FALSE, store = TRUE)
	print(i)
}

### make plot
png('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/plots/uniform_dists.png')
plot(1:100, probs[, 1], type = 'line', col = 'red', ylim = c(25, 50), xlab = 'x', ylab = 'Probability of Winning')
lines(1:100, probs[, 2], type = 'line', col = 'blue')
lines(1:100, probs[, 3], type = 'line', col = 'black')
box(which = 'plot', lty = 'solid')
legend(x = 'right', bg = 'white', legend = c('U[-1, 1]', 'U[-2, 2]', 'U[-x, x]'), col = c('red', 'blue', 'black'), lty = rep(1, 3))
dev.off()

### make plot of gamma vs. normal
png('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/plots/skewness.png')
plot(density(rgamma(10000000, shape = 1, scale = 2)), xlim = c(-10, 20), xlab = 'Value', main = '')
lines(density(rnorm(10000000, mean = 2, sd = sqrt(2))), col = 'blue')
legend(x = 'topright', legend = c('Mean = 2', 'Variance = 2'))
dev.off()

### performance of gamma vs. normal
knapsack_compare(dists = list('Gamma', 'Normal'), params = list(c(shape = 1, scale = 2), c(mean = 2, sd = sqrt(2))))