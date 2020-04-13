# Source : https://github.com/thimotei/CFR_calculation/blob/poisson_GAMs/global_estimates/R/delay_distributions.R

# utilities ---------------------------------------------------------------

# setting functions for the delay distribution
muTransform <- function(zMedian){
	mu <- log(zMedian) 
}

sigmaTransform <- function(zMean, mu){
	sigma <- sqrt(2*(log(zMean) - mu))
}

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x, mu, sigma) {
	plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}


hospitalisation_to_death_truncated_low <- function(x){
	hospitalisation_to_death_truncated(x, muLow, sigmaLow)
}

hospitalisation_to_death_truncated_mid <- function(x){
	hospitalisation_to_death_truncated(x, muMid, sigmaMid)
}

hospitalisation_to_death_truncated_high <- function(x){
	hospitalisation_to_death_truncated(x, muHigh, sigmaHigh)
}

# Define CFR function -----------------------------------------------------

# Function to work out correction CFR
scale_cfr <- function(data_1_in, delay_fun){
	case_incidence <- data_1_in$new_cases
	death_incidence <- data_1_in$new_deaths
	cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
	# Sum over cases up to time tt
	for(ii in 1:nrow(data_1_in)){
		known_i <- 0 # number of cases with known outcome at time ii
		for(jj in 0:(ii - 1)){
			known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
			known_i <- known_i + known_jj
		}
		cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
	}
	# naive CFR value
	b_tt <- sum(death_incidence)/sum(case_incidence) 
	# corrected CFR estimator
	p_tt <- sum(death_incidence)/cumulative_known_t
	data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
						 cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}

# working out under-reporting estimate and CIs
underReportingEstimates <- function(data, delay_fun){ 
	dplyr::group_by(data, country) %>%
		dplyr::do(scale_cfr(., delay_fun)) %>%
		dplyr::filter(cum_known_t > 0 & cum_known_t >= total_deaths)  %>%
		dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
									nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
									cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
									cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
									underreporting_estimate = cCFRBaseline / (100*cCFR),
									lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
									upper = cCFREstimateRange[2] / (100 * cCFR_LQ),
									quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
									quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2]) %>% 
		dplyr::filter(total_deaths > 1)}

# setting the baseline CFR
cCFRBaseline <- 1.4
cCFREstimateRange <- c(1.2, 1.7)

# set parameters of delay distribution -----------------------------------

# lower end of the range
zmeanLow <- 8.7
zmedianLow <- 6.7
muLow <- muTransform(zmedianLow)
sigmaLow <- sigmaTransform(zmeanLow, muLow)


# middle of the range
zmeanMid <- 13
zmedianMid <- 9.1
muMid <- muTransform(zmedianMid)
sigmaMid <- sigmaTransform(zmeanMid, muMid)

# upper end of the range
zmeanHigh <- 20.9
zmedianHigh <- 13.7
muHigh <- muTransform(zmedianHigh)
sigmaHigh <- sigmaTransform(zmeanHigh, muHigh)

# Load data -----------------------------------------------------

httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- read.csv(tf)

library(dplyr)
# munge data, pad data and select only those with greater than 10 deaths
allTogetherClean <- allDat %>% 
	dplyr::arrange(countriesAndTerritories, dateRep) %>% 
	dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
	dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
	dplyr::select(date, country, new_cases, new_deaths) %>%
	dplyr::bind_rows(nc_data) %>% 
	dplyr::filter(country != "CANADA", 
								country != "Cases_on_an_international_conveyance_Japan") %>%
	dplyr::group_by(country) %>%
	padr::pad() %>%
	dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
								new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
	dplyr::group_by(country) %>%
	dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
	dplyr::filter(cum_deaths > 0) %>%
	dplyr::select(-cum_deaths)

nc_data <- nccovid::get_covid_state()

nc_data <- nc_data %>% 
	select(county,date, cases_daily,deaths_daily) %>% 
	setNames(c("country", "date", "new_cases", "new_deaths")) %>% 
	mutate(date = lubridate::mdy(date))

nc_death <- nc_data %>% 
	group_by(country) %>% 
	summarise(tot = sum(new_deaths)) %>% 
	filter(tot > 1) %>% 
	pull(country)

nc_data <- nc_data %>% 
	filter(country %in% nc_death)

allTogetherClean <- allTogetherClean %>% 
	bind_rows(nc_data)

# calculate table of estimates using three delay distributions (both ends of the reported ranges and the mean)
allTogetherLow <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_low) 
allTogetherMid <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_mid) 
allTogetherHigh <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_high)


# choosing CIs such that they include all uncertainty from delay distribution
finalRes <- dplyr::tibble(
	country = allTogetherMid$country,
	total_cases = allTogetherMid$total_cases,
	total_deaths = allTogetherMid$total_deaths,
	underreporting_estimate  = pmin(allTogetherLow$underreporting_estimate, allTogetherMid$underreporting_estimate, allTogetherHigh$underreporting_estimate, na.rm = T),
	lower = pmin(allTogetherLow$lower, allTogetherMid$lower, allTogetherHigh$lower),
	upper = pmax(allTogetherLow$upper, allTogetherMid$upper, allTogetherHigh$upper))


# putting all of the data together in a readable format for the Rmd file
reportDataFinal <- finalRes %>%
	dplyr::select(country, total_cases, total_deaths, underreporting_estimate, lower,
								upper) %>%
	dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
	dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
	dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
	dplyr::mutate(lower = signif(lower, 2)) %>%
	dplyr::mutate(upper = signif(upper, 2)) %>%
	#dplyr::ungroup(country, ) %>%
	dplyr::mutate(country = gsub(x = country, pattern = "_",replacement =  " ")) %>% 
	dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
																											 "% (",lower*100,"% - ",upper*100,"%)"))

# saving the output

guilford_county <- allTogetherClean %>% 
	filter(country=="Guilford")
# fit a Poisson GAM to deaths and known cases data, with a given value for the
# true CFR, and return the timeseries mean and 95% CI
get_one_timeseries <- function (true_cfr, data) {
	
	# fit model
	data <- data %>%
		mutate(log_offset = log(cases_known * true_cfr/100))
	
	model <- mgcv::gam(deaths ~ s(date_num, k = 3) + offset(log_offset),
										 data = data,
										 family = stats::poisson)
	
	# predict timeseries without log-offset to get timeseries -log(reporting rate)
	pred_data <- data.frame(date_num = data$date_num,
													log_offset = 0)
	preds <- stats::predict(model,
													newdata = pred_data,
													type = "link",
													se.fit = TRUE)
	
	# parameters of distribution over log reporting rate
	mu <- -preds$fit
	sigma <- preds$se.fit
	
	# convert to expectation and 95% CI over reporting rate and return
	tibble::tibble(
		estimate = exp(mu + (sigma ^ 2) / 2),
		lower = qlnorm(0.025, meanlog = mu, sdlog = sigma),
		upper = qlnorm(0.975, meanlog = mu, sdlog = sigma)
	)
	
}

scale_cfr_temporal <- function(data_1_in, delay_fun = hospitalisation_to_death_truncated){
	
	case_incidence <- data_1_in$new_cases
	death_incidence <- data_1_in$new_deaths
	cumulative_known_t <- NULL # cumulative cases with known outcome at time tt
	# Sum over cases up to time tt
	for(ii in 1:nrow(data_1_in)){
		known_i <- 0 # number of cases with known outcome at time ii
		for(jj in 0:(ii - 1)){
			known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
			known_i <- known_i + known_jj
		}
		cumulative_known_t <- c(cumulative_known_t,known_i) # Tally cumulative known
	}
	
	# naive CFR value
	b_tt <- sum(death_incidence)/sum(case_incidence) 
	# corrected CFR estimator
	p_tt <- (death_incidence/cumulative_known_t) %>% pmin(.,1)
	
	data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
						 cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}

get_plot_data <- function(country_name, data = allTogetherCleanA){
	
	true_cfr <- 1.4/100
	
	#filter country data and adjust date
	country_data <- data %>% 
		filter(country == country_name) %>% 
		mutate(date = date - zmeanHDT)
	
	#date where cumulative deaths passed 10
	death_threshold_date <- country_data %>% 
		mutate(death_cum_sum = cumsum(new_deaths)) %>% 
		filter(death_cum_sum >= 10) %>% 
		pull(date) %>% 
		min()
	
	#return adjusted date and reporting_estimate
	cfr <- scale_cfr_temporal(country_data) %>% 
		as_tibble() %>% 
		mutate(reporting_estimate = true_cfr/cCFR) %>% 
		mutate(reporting_estimate = pmin(reporting_estimate, 1),
					 country = country_data$country,
					 date = country_data$date,
					 date_num = as.numeric(country_data$date),
					 deaths = country_data$new_deaths,
					 cases_known = cum_known_t) %>% 
		filter(date >= death_threshold_date) %>% 
		select(country, date, date_num, reporting_estimate, deaths, cases_known)
	
	return(cfr)
	
}

out <- scale_cfr_temporal(guilford_county, hospitalisation_to_death_truncated_mid)

#plot time varying cfr for a country
plot_country <- function(plot_data){
	
	# get timeseries with the expectation, lower, and upper bounds of true CFR
	expectation <- get_one_timeseries(cCFRBaseline, plot_data)*100
	lower <- get_one_timeseries(cCFREstimateRange[1], plot_data)*100
	upper <- get_one_timeseries(cCFREstimateRange[2], plot_data)*100
	
	estimate <- expectation$estimate
	ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
														y = c(upper$upper, rev(lower$lower)))
	
	# clip all of these to (0, 1]
	estimate <- pmin(estimate, 100)
	ci_poly$y <- pmin(ci_poly$y, 100)
	
	p <- plot_data %>% 
		ggplot2::ggplot() +
		ggplot2::theme_bw() + 
		#ggplot2::geom_point(aes(x = date, y = reporting_estimate), size = 0.2) + 
		#ggplot2::geom_path(aes(x = date, y = estimate), colour = 'red', size = 0.3) +
		ggplot2::geom_polygon(data = ci_poly, 
													ggplot2::aes(x = x, y = y), fill = viridis::viridis_pal()(10)[6], alpha = 0.3) +
		ggplot2::coord_cartesian(ylim = c(0, 100)) +
		ggplot2::ylab("") +
		ggplot2::ggtitle(gsub("_", " ", plot_data$country %>% unique())) +
		cfr_plot_theme()
	
	return(p)
}

#default theme for plotting time varying cfr estimates
#can be changed to multiple themes for different outputs - html report / publication graphic
cfr_plot_theme <- function(){
	t <- ggplot2::theme(axis.title.x = element_blank(),
											axis.text.x = element_text(size = 8),
											axis.text.y = element_text(size = 8),
											panel.grid = element_blank(),
											plot.title = element_text(hjust = 0.5, size = 13),
											text = element_text(size = 6))
	
	return(t)
}

#get time varying cfr data for a country
get_plot_data <- function(country_name, data = allTogetherCleanA){
	
	true_cfr <- 1.4/100
	
	#filter country data and adjust date
	country_data <- data %>% 
		filter(country == country_name) %>% 
		mutate(date = date - zmeanHDT)
	
	#date where cumulative deaths passed 10
	death_threshold_date <- country_data %>% 
		mutate(death_cum_sum = cumsum(new_deaths)) %>% 
		filter(death_cum_sum >= 10) %>% 
		pull(date) %>% 
		min()
	
	#return adjusted date and reporting_estimate
	cfr <- scale_cfr_temporal(country_data) %>% 
		as_tibble() %>% 
		mutate(reporting_estimate = true_cfr/cCFR) %>% 
		mutate(reporting_estimate = pmin(reporting_estimate, 1),
					 country = country_data$country,
					 date = country_data$date,
					 date_num = as.numeric(country_data$date),
					 deaths = country_data$new_deaths,
					 cases_known = cum_known_t) %>% 
		filter(date >= death_threshold_date) %>% 
		select(country, date, date_num, reporting_estimate, deaths, cases_known)
	
	return(cfr)
	
}

# Code to fit GAMs to time-series of under-reporting estimates

# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)
library(mgcv)
require(gridExtra)
require(ggplot2)

# Set parameters
zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))
cCFRBaseline <- 1.38
cCFREstimateRange <- c(1.23, 1.53)
#cCFRIQRRange <- c(1.3, 1.4)


# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
	plnorm(x + 1, muHDT, sigmaHDT) - plnorm(x, muHDT, sigmaHDT)
}


# Load data -----------------------------------------------------
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- readr::read_csv(tf)

nc_overall <- nccovid::get_covid_state()

nc_overall_data <- nc_overall %>% 
	dplyr::group_by(state, date) %>% 
	summarise(new_cases = sum(cases_daily),
						new_deaths = sum(deaths_daily)) %>% 
	ungroup() %>% 
	dplyr::rename(country = state) %>% 
	mutate(country = gsub(x = country, " ", "_")) %>% 
	mutate(date = lubridate::mdy(date))

allDatDesc <- allDat %>% 
	dplyr::arrange(countriesAndTerritories, dateRep) %>% 
	dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
	dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
	dplyr::select(date, country, new_cases, new_deaths) %>%
	dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan")) %>% 
	dplyr::bind_rows(nc_overall_data)

# Do analysis
allTogetherCleanA <- allDatDesc %>%
	dplyr::group_by(country) %>%
	padr::pad() %>%
	dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
								new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
	#What is this doing?
	dplyr::group_by(country) %>%
	dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
	dplyr::filter(cum_deaths > 0) %>%
	dplyr::select(-cum_deaths)


# Plot rough reporting over time -----------------------------------------

plot_country_names <- allTogetherCleanA %>% 
	dplyr::filter(country %in% c("North_Carolina")) %>% 
	dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
	dplyr::filter(death_cum_sum >= 1) %>% 
	dplyr::mutate(max_deaths = max(death_cum_sum)) %>% 
	dplyr::arrange(-max_deaths) %>% 
	dplyr::group_by(country) %>% 
	dplyr::filter(n() >= 8) %>%
	dplyr::pull(country) %>% 
	unique() 


cfr_plots <- list()
for (country_name in plot_country_names){
	plot_data <- get_plot_data(country_name = country_name)
	
	p <- try(plot_country(plot_data = plot_data))
	
	if ('try-error' %in% class(p)){next}
	
	cfr_plots[[country_name]] = p
	
}

cfr_plot_grid = arrangeGrob(grobs = cfr_plots,
														ncol = 4,
														left = "Percentage of symptomatic cases reported", 
														rot = 90)


ggsave('global_estimates/outputs/figure_1.png',
			 cfr_plot_grid,
			 width =11, 
			 height = 25, 
			 units = 'in', 
			 dpi = 450)
