### packages
library(igraph)

### get functions
source('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/r/single_comparison.R', echo = TRUE)

### set mean = 10
mu <- 10
   
### make distributions
dists_full <- list('Uniform', 
	'Uniform', 
	'Uniform', 
	'Gamma',
	'Gamma',
	'Hypergeometric',
	'Hypergeometric', 
	'Binomial',
	'Binomial',
	'Poisson',
	'Geometric', 
	'Negative Binomial', 
	'Negative Binomial',
	'Exponential',
	'Log-normal', 
	'Normal',
	'Chi-squared',
	'Weibull',
	'Weibull',
	'Discrete Uniform',
	'Discrete Uniform', 
	'Discrete Uniform')
params_full <- list(c(min = mu - mu, max = mu + mu), 
	c(min = mu - (mu/2), max = mu + (mu/2)), 
	c(min = mu - (2*mu), max = mu + (2*mu)), 
	c(shape = mu, scale = 1),
	c(shape = 10*mu, scale = mu/100),
	c(m = 2*mu, n = 4*mu, k = 3*mu),
	c(m = 4*mu, n = 2*mu, k = 1.5*mu),
	c(size = mu/0.1, prob = 0.1),
	c(size = mu*1.5, prob = 1/1.5),
	c(lambda = mu),
	c(prob = 1/(mu+1)),
	c(size = (0.1/0.9)*mu, prob = 0.1),
	c(size = (0.9/0.1)*mu, prob = 0.9),
	c(rate = 1/mu),
	c(meanlog = log(mu/2), sdlog = sqrt(2*(log(mu) - log(mu/2)))),
	c(mean = mu, sd = 4),
	c(df = mu),
	c(shape = mu, scale = mu / gamma(1 + (1/mu))),
	c(shape = 2, scale = mu / gamma(1 + (1/2))),
	c(min = mu - mu, max = mu + mu),
	c(min = mu - (mu/2), max = mu + (mu/2)),
	c(min = mu - (2*mu), max = mu + (2*mu)))

### make sure means are the same
reps <- 10000
for(j in 1:length(dists_full)) {
	funcs <- dist_funs[match(dists_full[[j]], dist_names)]
	print(mean(do.call(funcs[[1]], c(reps, as.list(params_full[[j]])))))
}

### initialize matrix of results
results <- t(combn(1:length(dists_full), 2))
results <- cbind(results, matrix(NA, nrow(results), 2))
colnames(results) <- c('Dist1', 'Dist2', 'Win1', 'Win2')

### make comparisons
for(i in 1:nrow(results)) {
	results[i, 3:4] <- knapsack_compare(dists = list(dists_full[[results[i, 1]]], dists_full[[results[i, 2]]]), params = list(params_full[[results[i, 1]]], params_full[[results[i, 2]]]), reps = 1, iters = 10000, summary = FALSE, store = TRUE)
	print(i)
}

### remove cases where margin is less than 2%
margin <- results[abs(results[, 3] - results[, 4]) >= 1.3, ]

### determine winner
win <- margin[margin[, 3] > margin[, 4], 1:2]
lose <- margin[margin[, 4] > margin[, 3], 2:1]
winmat <- rbind(win, lose)
winmat <- winmat[order(winmat[, 1], winmat[, 2]), ]

### make graph, get adjacency matrix
g <- graph.data.frame(winmat, vertices = 1:22)
adj <- as_adj(g, sparse = FALSE)

### test for cycles
for(i in 1:nrow(adj)) {
	for(j in 1:nrow(adj)) {
		for(k in 1:nrow(adj)) {
			if(adj[i, j] == 1 & adj[j, k] == 1 & adj[k, i] == 1) {
				print(paste(i, j, k, sep = ', '))
			}
		}
	}
}
