library(cmdstanr)

mod <- cmdstan_model("lognormal.stan")

dat <- list(
	N = 100,
	LOS = rlnorm(100, log(5), sdlog = log(1.5))
)
hist(dat$LOS)
mean(dat$LOS)
fit <- mod$sample(data = dat)

fit$summary()

log(51/5
log(5)
1/exp(log(5))
EpiNow2::calc_CrI
