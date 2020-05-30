library(odin)

simulator <- odin({
	
	update(out[,]) <- input[i,j] * multiplier[j]
	
	dim(out) <- c(2,2)
	dim(input) <- c(2,2)
	dim(multiplier) <- 2
	
	input[,] <- user()
	multiplier[] <- user()
	y[,] <- user()
	initial(out[,]) <- y[i,j]
	
	dim(y) <-c(2,2)
	
	})
generate_simz <- simulator(input = matrix(rep(40,4), nrow = 2),
													 multiplier = c(2,2),
													 y = cbind(c(1,2),c(1,2)))
generate_simz$run(step = 10)
