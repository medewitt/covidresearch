hospitalisation_to_death_truncated_low <- function(x){
	hospitalisation_to_death_truncated(x, muLow, sigmaLow)
}

hospitalisation_to_death_truncated_mid <- function(x){
	hospitalisation_to_death_truncated(x, muMid, sigmaMid)
}

hospitalisation_to_death_truncated_high <- function(x){
	hospitalisation_to_death_truncated(x, muHigh, sigmaHigh)
}