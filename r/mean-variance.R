
### source('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/r/mean-variance.R', echo = TRUE)

### get functions
source('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/ad_knap/r/single_comparison.R', echo = TRUE)

### set random seed
set.seed(1016)

##### STEP 1: SYMMETRIC DISTRIBUTIONS, FIXED NUMBER OF PLAYERS #####

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
plot(mv_uniform[1:n, 1], (mv_uniform[1:n, 2]^2)/3, type = 'line', ylim = c(0, 90), xlab = '', ylab = '')
lines(mv_normal[1:n, 1], mv_normal[1:n, 2]^2, col = 'red')
mtext('3 Players', side = 3, line = 0, at = 0.5, col = 'black')
legend(x = 'topleft', bg = 'white', legend = c('Uniform', 'Normal'), col = c('black', 'red'), lty = rep(1, 2))
plot(mv_uniform[1:n, 1], (mv_uniform[1:n, 3]^2)/3, type = 'line', ylim = c(0, 90), xlab = '', ylab = '')
lines(mv_normal[1:n, 1], mv_normal[1:n, 3]^2, col = 'red')
mtext('4 Players', side = 3, line = 0, at = 0.5, col = 'black')
plot(mv_uniform[1:n, 1], (mv_uniform[1:n, 4]^2)/3, type = 'line', ylim = c(0, 90), xlab = '', ylab = '')
lines(mv_normal[1:n, 1], mv_normal[1:n, 4]^2, col = 'red')
mtext('5 Players', side = 3, line = 0, at = 0.5, col = 'black')
plot(mv_uniform[1:n, 1], (mv_uniform[1:n, 5]^2)/3, type = 'line', ylim = c(0, 90), xlab = '', ylab = '')
lines(mv_normal[1:n, 1], mv_normal[1:n, 5]^2, col = 'red')
mtext('10 Players', side = 3, line = 0, at = 0.5, col = 'black')
title(xlab = 'Excess Mean', ylab = 'Excess Variance', outer = TRUE, line = 3, cex.lab = 1.5)
dev.off()

##########

##### STEP 2: SKEWED DISTRIBUTIONS, FIXED NUMBER OF PLAYERS #####

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

##### STEP 3: SYMMETRIC DISTRIBUTIONS, VARIABLE NUMBER OF PLAYERS, FIXED MEAN #####

### set sequence of mean values, number of players, distributions
mean_seq <- c(0.5, 1)
players <- 3:20

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
		print(paste0(mean_seq[j], ' ', p))
		params_uniform <- quote(c(list(c(min = -1 + mean_seq[j], max = 1 + mean_seq[j]), c(min = -i, max = i)), list(c(min = -1, max = 1))[rep(1, players[p]-2)]))
		params_normal <- quote(c(list(c(mean = mean_seq[j], sd = 1), c(mean = 0, sd = i)), list(c(mean = 0, sd = 1))[rep(1, players[p]-2)]))
		mv_uniform_players[p, j] <- compare(dists = list('Uniform')[rep(1,players[p])], params = params_uniform)[1]
		mv_normal_players[p, j] <- compare(dists = list('Normal')[rep(1,players[p])], params = params_normal)[1]
	}
}
