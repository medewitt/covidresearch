library(rstan)
library(gridExtra)
library(outbreaks)
rstan_options (auto_write = TRUE)
options (mc.cores = parallel::detectCores ())
set.seed(336) # for reproductibility

# time series of cases
cases <- influenza_england_1978_school$in_bed  # Number of students in bed
cases <- rpois(20,5)
# total count
N <- 763;

# times
n_days <- length(cases) +1
t <- seq(0, n_days, by = 1)
t0 = 0 
t <- t[-1]

#initial conditions
i0 <- 1
s0 <- N - i0
r0 <- 0
y0 = c(S = s0, I = i0, R = r0)

# data for Stan
data_sir <- list(n_days = n_days, y0 = y0, 
								 t0 = t0, ts = t, N = N, cases = cases,
								 compute_likelihood = 1)

data_sir_prior <- list(n_days = n_days, y0 = y0, 
								 t0 = t0, ts = t, N = N, cases = cases,
								 compute_likelihood = 0)

# number of MCMC steps
niter <- 2000

model <- stan_model("stan/sir.stan")
model2 <- stan_model("stan/sir2.stan")

fit_sir_negbin <- sampling(model,
													 data = data_sir,
													 iter = niter,
													 chains = 2, cores = 2)

print(fit_sir_negbin, pars = "incidence")

fit_sir_prior <- sampling(model,
													 data = data_sir_prior,
													 iter = niter,
													 chains = 1)


pars=c('beta', 'gamma', "R0", "recovery_time")
print(fit_sir_negbin, pars = pars)
stan_dens(fit_sir_negbin, pars = pars, separate_chains = TRUE)

smr_pred <- cbind(as.data.frame(summary(
	fit_sir_negbin, pars = "pred_cases", probs = c(0.05, 0.5, 0.95))$summary), t[1:20], cases)
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t[1:20])) +
	geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "orange", alpha = 0.6) +
	geom_line(mapping = aes(x = t[1:20], y = X50.)) + 
	geom_point(mapping = aes(y = cases)) +
	labs(x = "Day", y = "Number of students in bed")
