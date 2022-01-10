library(tidyverse)
library(data.table)

url <- "https://raw.githubusercontent.com/conedatascience/covid-data/master/data/timeseries/testing-by-county-aggregate.csv"

dat_raw <- fread(url)

dat_raw[,county:=stringr::str_remove(string = county, pattern = " County")]

dat_raw <- dat_raw[nccovid::nc_population[,1:2], on = "county"]

dat_raw[,tests_per_capita := test_n_14day/(july_2020/100000)]

dat_cases <- nccovid::get_covid_state(reporting_adj = TRUE)

dat_combined <- merge(dat_raw, dat_cases[,.(county, date, cases_daily_roll_sum)],
											all.x = TRUE,
											by.x = c("county", "reference_date"), by.y = c("county", "date"))

dat_combined[,cases_per_capita:=cases_daily_roll_sum/(july_2020/100000)]

dat_combined[,cases_per_capita_adj := cases_per_capita/tests_per_capita]

mask_counties <- c("Durham", "Wake", "Mecklenburg", "Buncombe", "Orange")

d_reduced <- dat_combined %>% 
	filter(reference_date > Sys.Date()-7*4) %>% 
	filter(county %in% c("Guilford", "Durham", "Buncombe",
											 "Mecklenburg", "Orange", "Wake",
											 "Randolph", "Alamance", "Davidson", 
											 "Stokes", "Rockingham")) %>% 
	mutate(use_masks = ifelse(county %in% mask_counties, 1, 0)) %>% 
	mutate(t  = as.numeric(reference_date)- min(as.numeric(reference_date)) + 1)

d_reduced	%>% 
	filter(county %in% c(mask_counties, "Guilford", "Alamance", "Randolph")) %>% 
	ggplot(aes(reference_date,cases_per_capita_adj,color = county))+
	geom_line()+
	ggrepel::geom_label_repel(max.overlaps = 15,data = filter(d_reduced, 
																					reference_date == max(reference_date)) %>% 
																						filter(county %in% c(mask_counties, "Guilford", "Alamance", "Randolph")),
														aes(label = county, x = reference_date))+
	theme_bw()+
	theme(legend.position = "none")+
	ggsci::scale_fill_jama()+
	labs(
		y = "Rolling 7 Day Cases (sum) / Total Tests (Rolling 14 Days)",
		title = "NC Counties are Experiencing Different Growth Rates of Cases",
		x = NULL
	)

library(brms)

fit <- brm(cases_per_capita_adj ~ use_masks + (1|county) + s(t),
					 data = d_reduced, backend = "cmdstan", save_model = "nc-masking")

summary(fit)

hypothesis(fit, "use_masks < 0")
