library(cmdstanr)

mod <- cmdstan_model(here::here("stan", "new-sir.stan"))


# fake data ---------------------------------------------------------------
library(deSolve)
sir_model = function (current_timepoint, state_values, parameters)
{
	# create state variables (local variables)
	S = state_values [1]        # susceptibles
	I = state_values [2]        # infectious
	R = state_values [3]        # recovered
	N = state_values [1] + state_values [2] + state_values [3]
	
	with ( 
		as.list (parameters),     # variable names within parameters can be used 
		{
			# compute derivatives
			dS = (-beta * S * I)/N
			dI = (beta * S * I)/N  - (gamma * I)
			dR = (gamma * I)
			
			# combine results
			results = c (dS, dI, dR)
			list (results)
		}
	)
}

beta_value <- 1/3
gamma_value <- 1/10

(r_0 <- beta_value/gamma_value)

parameter_list <- c (beta = beta_value, gamma = gamma_value)
times <- 1:120
initial_values <- c(S= 1000-2, I =2, R = 0)
output = as.data.frame(lsoda (initial_values, 
															times, sir_model, 
															parameter_list))

output <- dplyr::mutate_if(output, is.numeric, round)

cases <- vector(mode = "numeric")
for(i in 1:25){
	cases[i]<- output[i,2]-output[i+1,2]
}

cases_all <- vector(mode = "numeric")
for(i in 1:(length(cases)+14)){
	cases_all[i]<- output[i,2]-output[i+1,2]
}

# stan_data ---------------------------------------------------------------
slice_n <- 25
stan_data <- list(
	N_t = as.integer(slice_n),
	y0 = c(998,2,0),
	t = 1:slice_n,
	cases = cases[1:24],
	pred_window = 14,
	t_pred = 1:(slice_n+14)
)
fit <- mod$sample(data = stan_data, 
									chains = 2, parallel_chains = 2,
									iter_sampling = 5000)
fit$summary(variables = c("beta", "gamma", "r0", "recovery"))
fit$summary(variables = "gamma")
plot(fit$summary(variables = "reff")$mean)
library(tidybayes)
library(posterior)
library(dplyr)
library(tidyverse)
library(ggdist)
tidy_draws.CmdStanMCMC <- function(model, ...) {
	return(as_draws_df(model$draws()))
}
fit$draws(variables = c("incidence"))
fit$draws(variables = c("incidence_out"))

gather_draws(fit, incidence[i]) %>%
	dplyr::ungroup() %>% 
	group_by(i) %>%
	curve_interval(.value, .width = c(.5, .8, .95)) %>%
	ggplot(aes(x = i, y = .value)) +
	geom_hline(yintercept = 1, color = "gray75", linetype = "dashed") +
	geom_lineribbon(aes(ymin = .lower, ymax = .upper))+
	scale_fill_brewer() +
	labs(
		title = "Simulated SIR Curve for Infections",
		y = "Cases"
	)+
	geom_point(data = tibble(cases = cases, i = 1:length(cases)),
						 aes(i, cases), inherit.aes = FALSE, colour = "orange")+
	theme_minimal()
	
fit$summary(variables = "incidence_out")
gather_draws(fit, incidence_out[i]) %>%
	dplyr::ungroup() %>% 
	group_by(i) %>%
	curve_interval(.value, .width = c(.5, .8, .95)) %>%
	ggplot(aes(x = i, y = .value)) +
	geom_hline(yintercept = 1, color = "gray75", linetype = "dashed") +
	geom_lineribbon(aes(ymin = .lower, ymax = .upper))+
	scale_fill_brewer() +
	labs(
		title = "Simulated SIR Curve for Infections",
		y = "Cases"
	)+
	geom_point(data = tibble(cases = cases, i = 1:length(cases)),
						 aes(i, cases), inherit.aes = FALSE, colour = "orange")+
	geom_point(data = tibble(cases = tail(cases_all,14), 
													 i = tail(1:length(cases_all),14)),
						 aes(i, cases), inherit.aes = FALSE, colour = "red")+
	theme_minimal()


# nc data -----------------------------------------------------------------
nc_data <- nccovid::pull_covid_summary()
phase_3 <- nccovid::nc_events[nccovid::nc_events$event=="Phase 3",]$date

cut_pt <- which(nc_data$date==phase_3)
cases <- nc_data[cut_pt:nrow(nc_data)]$daily_cases
dates <- nc_data[cut_pt:nrow(nc_data)]$date

plot(cases, type = "b")
recovered <- sum(nc_data[1:(cut_pt-1)]$daily_cases)*4
pop <- 11000000
infected <- nc_data[,daily_cases][cut_pt]*10
pred_duration <- 180
stan_data <- list(
	N_t =length(cases),
	y0 = c(pop-recovered-infected,infected,recovered),
	t = 1:length(cases),
	cases = cases[-1],
	pred_window = pred_duration,
	t_pred = 1:(length(cases)+pred_duration)
)

output_frame_ready <- data.frame(
	date = seq.Date(min(dates), max(dates)+pred_duration, by = 1),
	i = 1:(length(cases)+pred_duration)
)

fit <- mod$sample(data = stan_data, init = list(list(beta = .2), 
																								list(gamma = .1)),
									chains = 2, parallel_chains = 2,
									iter_sampling = 4000, 
									adapt_delta = .98, max_treedepth = 14)

params_of_interest <- c("r0", "recovery", "beta", "gamma")
fit$summary(variables = params_of_interest)
fit$summary(variables = "gamma")

gather_draws(fit, incidence_out[i]) %>%
	#mutate(.value = .value * .07) %>% 
	dplyr::ungroup() %>% 
	dplyr::left_join(output_frame_ready, by = "i") %>% 
	group_by(date) %>%
	curve_interval(.value, .width = c(.5, .8, .95,.99)) %>%
	ggplot(aes(x = date, y = .value)) +
	geom_hline(yintercept = 1, color = "gray75", linetype = "dashed") +
	geom_lineribbon(aes(ymin = .lower, ymax = .upper))+
	scale_fill_brewer() +
	labs(
		title = "Simulated SIR Curve for Infections",
		subtitle = "Daily COVID-19 Cases for NC Since Phase 3",
		y = "Cases"
	)+
	geom_point(data = tibble(cases = cases, date = dates),
						 aes(date, cases), inherit.aes = FALSE, colour = "orange")+
	theme_minimal()


gather_draws(fit, reff[i]) %>%
	#mutate(.value = .value * .07) %>% 
	dplyr::ungroup() %>% 
	dplyr::left_join(output_frame_ready, by = "i") %>% 
	group_by(date) %>%
	curve_interval(.value, .width = c(.5, .8, .95,.99)) %>%
	ggplot(aes(x = date, y = .value)) +
	geom_hline(yintercept = 1, color = "gray75", linetype = "dashed") +
	geom_lineribbon(aes(ymin = .lower, ymax = .upper))+
	scale_fill_brewer() +
	labs(
		title = "Effective Reproduction Number Simple SIR",
		subtitle = "Daily COVID-19 Cases for NC Since Phase 3",
		y = expression(R[t])
	)+
	theme_minimal()
# Think about adding convolution 
# https://github.com/cobeylab/epidemic-deconvolution/blob/main/stan/deconvolve-uncorrelated.stan