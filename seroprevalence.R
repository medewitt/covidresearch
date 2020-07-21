library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model <- stan_model(here::here("seroprevalence.stan"))

# Wake Forest Data
# USe Abbot Testing Platform Data From:
# Evaluation of sensitivity and specificity of 
#four commercially available SARS-CoV-2 antibody
# Oxford University Hospitals Study



postitives <- 18750*.14
dat <- list(
	y_sample = as.integer(postitives),
	n_sample = 18750L,
	sensitivity = .863,
	specificity = .999,
	sensitivity_sd = .01,
	specificity_sd= .01
)

fit <- sampling(model, data = dat, iter = 2000,
								control = list(adapt_delta = .95))

summary(fit)

# change testing params ---------------------------------------------------

dat <- list(
	y_sample = as.integer(3500L*.14),
	n_sample = 3500L,
	sensitivity = .873,
	specificity = .999,
	sensitivity_sd = .01,
	specificity_sd= .001
)

fit <- sampling(model, data = dat, iter = 2000,
								control = list(adapt_delta = .95))

summary(fit)$summary

stan_plot(fit, pars = "p")
