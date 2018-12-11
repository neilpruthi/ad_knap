
### source('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/r/mean-variance.R', echo = TRUE)

### load packages
library(latex2exp)

### get functions
source('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/r/single_comparison.R', echo = TRUE)

### set random seed
set.seed(1016)

##### STEP 1: PLOT UNIFORM PROBABILITY OF WINNING WITH CHANGE IN A #####

### set probabilities of winning
probs <- matrix(NA, 100, 3)

### compute probabilities
for(i in 1:100) {
	probs[i, ] <- knapsack_compare(dists = list('Uniform', 'Uniform', 'Uniform'), params = list(c(min = -1, max = 1), c(min = -2, max = 2), c(min = -i, max = i)), reps = 1, iters = 100000, summary = FALSE, store = TRUE)
	print(i)
}

### make plot
setEPS()
postscript('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/plots/uniform_dists.eps')
plot(1:100, probs[, 1], type = 'line', col = 'red', ylim = c(25, 50), xlab = 'a', ylab = 'Probability of Winning (%)', cex.lab = 1.5)
lines(1:100, probs[, 2], type = 'line', col = 'blue')
lines(1:100, probs[, 3], type = 'line', col = 'black')
box(which = 'plot', lty = 'solid')
legend(x = 'right', bg = 'white', legend = c('U[-1, 1]', 'U[-2, 2]', 'U[-a, a]'), col = c('red', 'blue', 'black'), lty = rep(1, 3))
dev.off()

##########

##### STEP 2: SYMMETRIC DISTRIBUTIONS, FIXED NUMBER OF PLAYERS #####

### set sequence of mean values, number of players, distributions
mean_seq <- seq(0.05, 1.95, by = 0.05)
players <- c(3, 4, 5, 10)

### create matrices
mv_uniform <- matrix(NA, length(mean_seq), length(players) + 1)
mv_uniform[, 1] <- mean_seq
colnames(mv_uniform) <- c('Mean', paste0(players, '-players'))
mv_normal <- matrix(NA, length(mean_seq), length(players) + 1)
mv_normal[, 1] <- mean_seq
colnames(mv_normal) <- c('Mean', paste0(players, '-players'))

### compute indifferences for each mean, number of players
for(p in 1:length(players)) {
	for(j in 1:length(mean_seq)) {
		print(paste0(mean_seq[j], ' ', p))
		params_uniform <- quote(c(list(c(min = -1 + mean_seq[j], max = 1 + mean_seq[j]), c(min = -i, max = i)), list(c(min = -1, max = 1))[rep(1, players[p]-2)]))
		params_normal <- quote(c(list(c(mean = mean_seq[j], sd = 1), c(mean = 0, sd = i)), list(c(mean = 0, sd = 1))[rep(1, players[p]-2)]))
		mv_uniform[j, p + 1] <- compare(dists = list('Uniform')[rep(1,players[p])], params = params_uniform)[1]
		mv_normal[j, p + 1] <- compare(dists = list('Normal')[rep(1,players[p])], params = params_normal)[1]
	}
}

### set number of datapoints to plot
n <- 20

### plot
setEPS()
postscript('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/plots/mv_symmetric.eps', width = 10.5, height = 8)
par(mfrow = c(2,2), oma = c(5,5,0,0) + 0.1, mar = c(2,2,2,2) + 0.1)
plot(c(0, mv_uniform[1:n, 1]), c(0, (mv_uniform[1:n, 2]^2)/3 - (1/3)), type = 'line', xlim = c(0, 1), ylim = c(0, 90), xlab = '', ylab = '')
lines(c(0, mv_normal[1:n, 1]), c(0, mv_normal[1:n, 2]^2 - 1), col = 'red')
mtext('1 Baseline Player', side = 3, line = 0, at = 0.5, col = 'black', cex = 1.25)
legend(x = 'topleft', bg = 'white', legend = c('Uniform', 'Normal'), col = c('black', 'red'), lty = rep(1, 2))
plot(c(0, mv_uniform[1:n, 1]), c(0, (mv_uniform[1:n, 3]^2)/3 - (1/3)), type = 'line', xlim = c(0, 1), ylim = c(0, 90), xlab = '', ylab = '')
lines(c(0, mv_normal[1:n, 1]), c(0, mv_normal[1:n, 3]^2 - 1), col = 'red')
mtext('2 Baseline Players', side = 3, line = 0, at = 0.5, col = 'black', cex = 1.25)
plot(c(0, mv_uniform[1:n, 1]), c(0, (mv_uniform[1:n, 4]^2)/3 - (1/3)), type = 'line', xlim = c(0, 1), ylim = c(0, 90), xlab = '', ylab = '')
lines(c(0, mv_normal[1:n, 1]), c(0, mv_normal[1:n, 4]^2 - 1), col = 'red')
mtext('3 Baseline Players', side = 3, line = 0, at = 0.5, col = 'black', cex = 1.25)
plot(c(0, mv_uniform[1:n, 1]), c(0, (mv_uniform[1:n, 5]^2)/3 - (1/3)), type = 'line', xlim = c(0, 1), ylim = c(0, 90), xlab = '', ylab = '')
lines(c(0, mv_normal[1:n, 1]), c(0, mv_normal[1:n, 5]^2 - 1), col = 'red')
mtext('8 Baseline Players', side = 3, line = 0, at = 0.5, col = 'black', cex = 1.25)
title(xlab = TeX('Excess Mean, x'), ylab = TeX('Excess Variance, s^{2}'), outer = TRUE, line = 3, cex.lab = 2)
dev.off()

##########

##### STEP 3: SKEWED DISTRIBUTIONS, FIXED NUMBER OF PLAYERS #####

# ### set sequence of mean values, number of players, distributions
# mean_seq_skew <- seq(5.25, 9.75, by = 0.25)
# players_skew <- c(2, 4, 5, 10)

# ### create matrices
# mv_gamma <- matrix(NA, length(mean_seq_skew), length(players_skew) + 1)
# mv_gamma[, 1] <- mean_seq_skew
# colnames(mv_gamma) <- c('Mean', paste0(players_skew, '-players'))

# ### set initial value
# eps <- 1e-10

# for(p in 1:length(players_skew)) {
# 	for(j in 1:length(mean_seq_skew)) {
# 		print(paste0(mean_seq_skew[j], ' ', p))
# 		mu <- mean_seq_skew[j]
# 		params_gamma <- quote(c(list(c(shape = (mu^2)/5, scale = 5/mu), c(shape = i, scale = 5/i)), list(c(shape = 5, scale = 1))[rep(1, players_skew[p]-2)]))
# 		mv_gamma[j, p + 1] <- compare(dists = list('Gamma')[rep(1,players_skew[p])], params = params_gamma, initval = 1, inc = 1000)[1]
# 	}
# }

# ##########

##### STEP 4: SYMMETRIC DISTRIBUTIONS, VARIABLE NUMBER OF PLAYERS, FIXED MEAN #####

### set sequence of mean values, number of players, distributions
mean_seq <- c(0.5, 1)
players <- 3:22

### create matrices
mv_uniform_players <- matrix(NA, length(players), length(mean_seq))
rownames(mv_uniform_players) <- paste0(players, '-players')
colnames(mv_uniform_players) <- paste0('Mean ', mean_seq)
mv_normal_players <- matrix(NA, length(players), length(mean_seq))
rownames(mv_normal_players) <- paste0(players, '-players')
colnames(mv_normal_players) <- paste0('Mean ', mean_seq)

### compute indifferences for each mean, number of players
for(p in 1:length(players)) {
	for(j in 1:length(mean_seq)) {
		print(paste0(mean_seq[j], ' ', players[p]))
		params_uniform <- quote(c(list(c(min = -1 + mean_seq[j], max = 1 + mean_seq[j]), c(min = -i, max = i)), list(c(min = -1, max = 1))[rep(1, players[p]-2)]))
		params_normal <- quote(c(list(c(mean = mean_seq[j], sd = 1), c(mean = 0, sd = i)), list(c(mean = 0, sd = 1))[rep(1, players[p]-2)]))
		mv_uniform_players[p, j] <- compare(dists = list('Uniform')[rep(1,players[p])], params = params_uniform)[1]
		mv_normal_players[p, j] <- compare(dists = list('Normal')[rep(1,players[p])], params = params_normal)[1]
	}
}

### plot
setEPS()
postscript('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/plots/mv_players.eps', width = 10.5, height = 8)
par(mar = c(5, 6, 2, 2) + 0.1)
plot(c(0, players - 2), c(0, 0.5/(((mv_uniform_players[, 1]^2)/3) - 1/3)), type = 'line', xlim = c(0, 20), ylim = c(0, 1.3), xlab = TeX('Number of Baseline Players, N-2'), ylab = TeX('Excess Mean/Variance, \\frac{x}{s^{2}}'), cex.lab = 1.5)
lines(c(0, players - 2), c(0, 1/(((mv_uniform_players[, 2]^2)/3) - 1/3)), col = 'black', lty = 5)
lines(c(0, players - 2), c(0, 0.5/((mv_normal_players[, 1]) - 1)), col = 'red', lty = 1)
lines(c(0, players - 2), c(0, 1/((mv_normal_players[, 2]) - 1)), col = 'red', lty = 5)
legend(x = 'topleft', legend = c('Uniform, x = 0.5', 'Uniform, x = 1', 'Normal, x = 0.5', 'Normal, x = 1'), col = c('black', 'black', 'red', 'red'), lty = c(1, 5, 1, 5))
dev.off()

##### STEP 5: PLOT GAMMA VS. NORMAL DISTRIBUTION #####

### make plot of gamma vs. normal
setEPS()
postscript('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/plots/skewness.eps')
par(mar = c(5, 6, 2, 2) + 0.1)
plot(density(rgamma(10000000, shape = 1, scale = 2)), xlim = c(-10, 20), xlab = 'Value', main = '', cex.lab = 1.5)
lines(density(rnorm(10000000, mean = 2, sd = sqrt(2))), col = 'blue')
legend(x = 'topright', legend = c('Mean = 2', 'Variance = 2'))
dev.off()

### performance of gamma vs. normal
knapsack_compare(dists = list('Gamma', 'Normal'), params = list(c(shape = 1, scale = 2), c(mean = 2, sd = sqrt(2))), iters = 100000)

##### SAVE DATA #####
write.csv(mv_uniform[1:20, ], file = '~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/data/mean_variance_uniform.csv')
write.csv(mv_normal[1:20, ], file = '~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/data/mean_variance_normal.csv')
write.csv(mv_uniform_players, file = '~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/data/mean_variance_uniform_n.csv')
write.csv(mv_normal_players, file = '~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/data/mean_variance_normal_n.csv')
