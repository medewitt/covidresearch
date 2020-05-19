library(tidyverse)

forsyth_test <-c(19, 27, 35, 27, 19, 23, 20, 34, 35, 61, 53, 43, 27, 25, 34, 61, 99, 59, 59, 42, 31)
forsyth_pos <- c(1, 7, 5, 3, 7, 1, 6, 23, 14, 26, 25, 21, 4, 3, 16, 14, 27, 13, 17, 3, 7)
forsyth_dates <- seq.Date(as.Date("2020-04-20"),as.Date("2020-05-10"),1 )

dat_forsyth <- data.frame(
	forsyth_dates,
	forsyth_pos,
	forsyth_test
) %>% 
	mutate(pos_rate = forsyth_pos/forsyth_test) %>% 
	mutate(date_n = as.numeric(forsyth_dates)) %>% 
	rename(positive_increase = forsyth_pos) %>% 
	mutate(date = forsyth_dates)

library(mgcv)

mod <- gam(pos_rate ~ s(date_n), 
					 data = dat_forsyth, 
					 family = quasibinomial)

dat_forsyth$pos_rate_smoothed <- predict(mod, newdata = dat_forsyth, type = "response")
summary(mod)

dat_forsyth %>%
	ggplot(aes(x = forsyth_dates, y = pos_rate)) +
	geom_line(aes(y = pos_rate_smoothed)) +
	geom_point(aes(size = 1), alpha = 0.1) +
	scale_size_area(label = scales::comma, max_size = 12) +
	labs(size = "Number of daily tests", 
			 x = "",
			 y = "",
			 title = "Test positivity rates for COVID-19 in 12 US states",
			 caption = "Source: covidtracking.com, smoothing by freerangestats.info") +
	scale_y_continuous(label = scales::percent, limits = c(0, 1))

increase_cases <- function(observed_cases, pos_rate, m, k){
	y <- observed_cases * pos_rate ^ k * m
	return(y)
}


the_data <- dat_forsyth %>%
	mutate(`Simple multiplier\n(confirmed x 6)` = increase_cases(positive_increase, pos_rate, m = 6.35, k = 0),
				 #`Deaths 7 days later x 100` = deaths_x_days_later * 100,
				 `Ratio multiplier` = increase_cases(positive_increase, pos_rate, m = 17.75, k = 1),
				 `Generalized adjustment` = increase_cases(positive_increase, pos_rate, m = 10.83, k = 0.5))  %>%
	select(date, `Confirmed cases` = positive_increase, 
				 `Simple multiplier\n(confirmed x 6)`:`Generalized adjustment`) %>%
	gather(variable, value, -date) %>%
	mutate(variable = fct_reorder(variable, -value, .fun = last),
				 variable = fct_relevel(variable, "Confirmed cases", after = Inf))
library(scales)
var_cols <- RColorBrewer::brewer.pal(length(unique(the_data$variable)), name = "Set1")
names(var_cols) <- unique(the_data$variable)
p1 <- the_data %>%
	ggplot(aes(x = date, y = value, colour = variable)) +
	theme(legend.position = "right") +
	scale_y_continuous(label = comma ) +
	scale_colour_manual(values = var_cols) +
	theme(axis.text.y = element_blank()) +
	labs(colour = "Adjustment method",
			 title = "Different methods of adjusting the raw observed daily COVID-19 case count in New York",
			 subtitle = "Comparing a simple 'times 10' multiplier with methods that adjust for the ratio of positive test rates.
The 'simple multiplier' method probably overestimates cases when testing is good, and underestimates it when testing is inadequate.",
			 y = "Daily new cases",
			 x = "",
			 caption = "Source: Confirmed cases and testing data from covidtracking.com, analysis by freerangestats.info")

p1+
	geom_line()

p1 + geom_smooth(method = "loess", se = FALSE, span = 0.5)

p1 %+% filter(the_data, variable %in% 
								c("Ratio multiplier", "Generalized adjustment")) + geom_line()


the_data %>%
	group_by(variable) %>%
	summarise(total_cases = sum(value, na.rm = TRUE)) %>%
	ggplot(aes(y = variable, x = total_cases, colour = variable)) +
	scale_colour_brewer(palette = "Set1") +
	geom_point() +
	geom_segment(xend = 0, aes(yend = variable)) +
	scale_x_continuous(label = comma) +
	labs(title = "Different methods of adjusting the raw observed daily COVID-19 case count in New York",
			 subtitle = "Comparing a simple 'times 10' multiplier with methods that adjust for the ratio of positive test rates.
The four adjustment methods have been calibrated to deliver similar total results for illustrative purposes.",
			 y = "Adjustment method",
			 x = "Total cases to 1 May 2020") +
	theme(legend.position = "none")


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

all_variables <- unique(the_data$variable)
plots <- list()

for(i in 1:length(all_variables)){
	incid <- filter(the_data, variable == all_variables[i]) %>% 
		rename(I = value,
					 dates = date) %>%
		select(-variable) %>%
		drop_na() %>%
		arrange(dates)
	
	effr <- estimate_R(incid,
										 method="si_from_sample",
										 si_sample=nishi_si_sample,
										 config = si_from_sample_nishiura_config)
	
	
	p <-  plot(effr, what = "R", legend = FALSE, options_R = list(col = "steelblue")) + theme_minimal()
	p$labels$title <- all_variables[i]
	p$labels$x <- ""
	p$labels$y <- "Estimated R for COVID-19 in Forsyth County"
	plots[[i]] <- p + coord_cartesian(ylim = c(0, 7) )
	
}

annotation <- "All methods shown here have an unrealistic spike in effective reproduction number
in mid March as testing started to reveal very high numbers of unrecorded cases prevalent in the population. The first day with significant number of tests was 13 March
(2,900, compared to a previous high of 44). Estimates of R, based on a sliding 7 day window, cannot
be taken regarded as useful until 20 March onwards. Estimates based on 'deaths 7 days later' are problematic 
for other reasons." %>%
	str_wrap(., 60)

annotation_plot <- ggplot() +
	annotate("text", x = -1, y = 0, label = annotation, hjust = 0, size = 4) +
	xlim(-1, 1) +
	theme_void() +
	labs(title = str_wrap("These charts show attempts to adjust the estimation of reproduction 
                        number by scaling up case numbers with a high test positivity.", 70))
library(cowplot)
library(patchwork)
draw_plots <- plots[[1]] + plots[[2]] +plots[[3]] +plots[[4]]  + annotation_plot

plots[[1]] + plots[[4]] + annotate("text", x = as.Date("2020-04-20"), y = 5, label = str_wrap(
	"Adjusting case numbers for test positivity makes estimates of
                                                  R more realistic but cannot make up for the poor data quality in 
                                                  March.", 40), hjust = 0)
