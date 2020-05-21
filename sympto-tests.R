dat <- pull_state_cases()
reports <- generate_underreporting_estimates(dat, 
																						 cCFRBaseline = 1/.7,
																						 cCFREstimateRange = c(.67, 1.34)/.7)
reports %>% 
	dplyr::arrange(-lower) %>% 
	dplyr::select(country, total_cases, total_deaths,underreporting_estimate_clean) %>% 
	knitr::kable(col.names = c("County", "Cases", "Deaths", "Perc Reported"), caption = "Estimated Reported Symptomatic Cases by North Carolina County (Minimum of Five Reported Deaths)")

p5 <- reports %>% 
	filter(country %in% c("Wake", "Durham", "Mecklenburg", "Orange", nccovid::triad_counties)) %>% 
ggplot(aes(reorder(country,lower), underreporting_estimate))+
			 	geom_pointrange(aes(ymin=lower, ymax=upper))+
	coord_flip()+
	geom_text(aes(label = underreporting_estimate_clean), vjust=-.5, hjust = -.05)+
	scale_x_discrete(expand = c(0,.5))+
	labs(
		title = "Estimated Number of Symptomatic Cases Detected of Selected Counties",
		subtitle = "Analysis shows that some counties are still undertesting",
		caption = "Estimated Symptomatic from Cases Fatality Ratio of 1",
		y = "Percent of Symptomatic Cases Detected",
		x = NULL
	)+
	theme_minimal()+
	scale_y_continuous(labels = scales::percent)

cowplot::save_plot(p5, base_height = 8, base_width = 10.5,
									 filename = here::here("output", paste0(Sys.Date(),"-testing-capture-rate.pdf")))

