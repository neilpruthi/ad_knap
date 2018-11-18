### source('~/Dropbox/Mory/Duke/Third Year/COMPSCI 590/Project/single_comparison.R', echo = TRUE)

knapsack_compare <- function(dists, params, reps = 1, iters = 1000, ...) {
	
	### make half-normal distribution function and discrete uniform
	rhnorm <- function(n, mean = 0, sd = 1) {abs(rnorm(n, mean, (sqrt(pi) / sqrt(2))*sd))}
	rdunif <-function(n, min, max) {sample(min:max, n, replace = TRUE)}

	### make map of distributions to functions
	dist_names <- c('Uniform', 'Hypergeometric', 'Binomial', 'Poisson', 'Geometric', 
		'Negative Binomial', 'Exponential', 'Log Normal', "Student's t", 'Normal', 
		'Chi-Squared', 'Weibull', 'Gamma', 'Beta', 'Half-Normal', 'Discrete Uniform')
	dist_funs <- c(runif, rhyper, rbinom, rpois, rgeom, rnbinom, rexp, rlnorm, rt, rnorm, 
		rchisq, rweibull, rgamma, rbeta, rhnorm, rdunif)

	### get number of distributions
	n <- length(dists)

	### get functions for distributions and parameter numbers
	funcs <- dist_funs[match(dists, dist_names)]

	### initialize matrix of random variables
	random <- array(NA, dim = list(reps, iters, n))

	### iterate over iters, set progress bar
	pb <- txtProgressBar(min = 0, max = iters, initial = 0, char = '=', style = 3)
	for(i in 1:iters) {

		for(j in 1:n) {

			# simulate values
			random[, i, j] <- do.call(funcs[[j]], c(reps, as.list(params[[j]])))

		}
		
		# update progress bar
		setTxtProgressBar(pb, i)

	}
	close(pb)

	### compute number of victories and margin of victory each iteration
	win <- matrix(NA, n, iters)
	margin <- matrix(NA, n, iters)
	for(i in 1:n) {
		maxelse <- apply(random[, , -i, drop = FALSE], 1:2, max)
		diff <- random[, , i] - maxelse
		win[i, ] <- colSums(random[, , i] > maxelse)
		margin[i, ] <- apply(diff, 2, function(x) mean(x[x > 0], na.rm = TRUE))
	}

	### compute winner across reps
	which.max.global <- function(x) {ifelse(length(which(x == max(x))) != 1, NA, which(x == max(x)))}
	avgwin <- na.omit(unlist(apply(win, 2, which.max.global)))

	### compute average margin of victory across reps
	avgmargin <- rowMeans(margin, na.rm = TRUE)

	### print winner
	for(i in 1:n) {
		if(is.null(names(params[[i]]))) {
			assign(paste0('params', i), paste(round(params[[i]], 3), sep = '', collapse = ', '))
		} else {
			assign(paste0('params', i), paste(names(params[[i]]), round(params[[i]], 3), sep = ': ', collapse = ', '))
		}
	}
	winpct <- round(100 * table(avgwin)/iters, 2)
	avgmargin <- round(avgmargin, 5)

	for(i in 1:n) {
		out_winpct <- ifelse(length(winpct[which(names(winpct) == i)]) == 0, 0, winpct[which(names(winpct) == i)])
		out_margin <- ifelse(is.na(avgmargin[i]), 0, avgmargin[i])
		print(paste0(dists[i], '(', get(paste0('params', i)), ') wins in ', out_winpct, '% of ', iters, ' iterations by an average of ', out_margin, ' over ', reps, ' repetitions.'))
	}
	print(paste0('The distributions tie in ', round(100 * (iters - length(avgwin))/iters, 2), '% of ', iters, ' iterations.'))
}

#knapsack_compare(dists = list('Uniform', 'Normal'), params = list(c(min = 0, max = 1), c(mean = 0.5, sd = 1)), reps = 100, iters = 10000)
#knapsack_compare(dists = list('Gamma', 'Negative Binomial'), params = list(c(shape = 6, rate = .4), c(size = 10, prob = .4)), reps = 1, iters = 100000)
#knapsack_compare(dists = list('Uniform', 'Normal', 'Poisson', 'Exponential'), params = list(c(0, 1), c(0.5, 1), c(0.5), c(2)), reps = 1, iters = 10000)
#knapsack_compare(dists = list('Uniform', 'Uniform', 'Uniform'), params = list(c(min = -1, max = 1), c(min = -2, max = 2), c(min = -3, max = 3)), reps = 1, iters = 100000)
#knapsack_compare(dists = list('Discrete Uniform', 'Discrete Uniform', 'Discrete Uniform'), params = list(c(min = -1, max = 1), c(min = -1, max = 1), c(min = -1, max = 1)), reps = 1, iters = 10000)
#knapsack_compare(dists = list('Uniform', 'Normal', 'Binomial'), params = list(c(min = 5-sqrt(30)/2, max = 5+sqrt(30)/2), c(mean = 5, sd = sqrt(2.5)), c(size = 10, prob = .5)), iters = 100000)
