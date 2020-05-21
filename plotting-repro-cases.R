library(dplyr)
library(outbreaktools)

dat <- pull_state_cases()



reports <- generate_underreporting_estimates(dat,
																						 cCFRBaseline = 1/.7,
																						 cCFREstimateRange = c(.67, 1.34)/.7)

dat <- nccovid::get_covid_state()

dat <- dat %>% 
	filter(date >= as.Date("2020-03-01")) %>% 
	as_tibble()

# Cases per 100
first_case_by_county <-dat %>% 
	group_by(county) %>% 
	filter(cases_daily>0) %>% 
	slice(1) %>% 
	select(county, date)

avg_cases_per_day <- list()

all_variables <- unique(first_case_by_county$county)

for(i in 1:length(all_variables)){
	x <- dat %>% 
		select(county, date, cases_daily) %>% 
		filter(county == all_variables[i]) %>% 
		filter(cases_daily>0) %>% 
		padr::pad(interval = "day") %>% 
		mutate(cases_daily = tidyr::replace_na(cases_daily,0))
	
	if(nrow(x)>0){
		avg_cases_per_day[[i]] <- x %>% 
			summarise(avg_cases_per = mean(cases_daily))
	} else{
		avg_cases_per_day[[i]] <- 0
	}
		
}

avg_cases_per_day <- avg_cases_per_day %>% 
	setNames(all_variables) %>% 
	bind_rows(.id = "county")

dat_total <- dat %>% 
	group_by(county) %>% 
	filter(date == max(date)) %>% 
	select(county, cases_confirmed_cum) %>% 
	left_join(nccovid::nc_fatality_estimates, by = c("county" ="NAME")) %>% 
	mutate(cases_per_100k = cases_confirmed_cum/(pop/100000)) %>% 
	left_join(avg_cases_per_day) %>% 
	left_join(reports %>% select(country, underreporting_estimate),
						by =c("county" = "country")) %>% 
	mutate(daily_cases_per_100k_adj = (avg_cases_per/underreporting_estimate) /(pop/100000))
	mutate(daily_cases_per_100k =avg_cases_per /(pop/100000))

library(EpiEstim)

source("get_nishiura_si_sample.R")
nishi_si_sample  <- get_nishiura_si_sample()

# configs for eff R estimation
# SI distribution from Nishiura et al.
parametric_si_nishiura_config <- make_config(list(mean_si = 4.7,
																									std_si = 2.9))

# values from https://www.nejm.org/doi/full/10.1056/NEJMoa2001316
parametric_si_li_config <- make_config(list(mean_si = 7.5,
																						std_si = 3.4))

# posterior sample based on Nishiura et al SI data
si_from_sample_nishiura_config <-  make_config(list(n1=500, n2 = 50, seed=2))


all_variables <- unique(dat$county)
reff <- list()

for(i in 1:length(all_variables)){
	incid <- filter(dat, county == all_variables[i]) %>% 
		rename(I = cases_daily,
					 dates = date) %>%
		select(-county) %>%
		tidyr::drop_na() %>%
		arrange(dates)
	
	reff[[i]] <- estimate_R(incid,
										 method="si_from_sample",
										 si_sample=nishi_si_sample,
										 config = si_from_sample_nishiura_config)
}
	
library(purrr)
current_repo_rate <- reff %>% 
	map("R") %>% 
	setNames(all_variables) %>% 
	bind_rows(.id = "county") %>% 
	group_by(county) %>% 
	filter(t_start == max(t_start))

combined_dat_plotting <- current_repo_rate %>% 
	left_join(dat_total)

library(ggplot2)
library(ggrepel)
p1<- combined_dat_plotting %>% 
	filter(county %in% c("Wake", "Durham", "Mecklenburg", "Orange", nccovid::triad_counties)) %>% 
	mutate(my_col = ifelse(county %in% nccovid::triad_counties, "A", "B")) %>% 
	ggplot(aes(daily_cases_per_100k_adj,`Median(R)`, group = county, color = my_col))+
	geom_pointrange(aes(ymin=`Quantile.0.05(R)`, ymax=`Quantile.0.95(R)`))+
	geom_text_repel(aes(label = county), size = 2)+
	geom_hline(yintercept = 1, lty = "dashed")+
	labs(
		title = "Analysis Shows Different Counties Are Experiencing Different Outcomes",
		subtitle = "Reproductive Rate Estimated from 7 Day Sliding Window and Serial Interval from Nishiura",
		y = expression("Estimated Reproductive Rate ("*R[t]*")"),
		x = "Adjusted Average Cases per Day per 100k",
		caption = "Cases Adjusted Using CFR Method by Russel 2020\nAnalysis: Michael DeWitt"
	)+
	theme_minimal()+
	theme(legend.position = "none")+
	scale_color_manual(values = c("orange", "blue"))

cowplot::save_plot(p1, base_height = 8, base_width = 10.5,
									 filename = here::here("output", paste0(Sys.Date(),"-triad-cases-reproduction-number.pdf")))

combined_dat_plotting %>% 
	mutate(my_col = ifelse(county %in% nccovid::triad_counties, "A", "B")) %>% 
	ggplot(aes(daily_cases_per_100k,`Median(R)`, group = county, color = my_col))+
	geom_point()+
	geom_text_repel(aes(label = county), size = 2)+
	geom_hline(yintercept = 1, lty = "dashed")+
	theme_minimal()



