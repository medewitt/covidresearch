url <- "https://raw.githubusercontent.com/conedatascience/covid-data/master/data/timeseries/cases-delay.csv"

library(data.table)

dat_raw <- fread(url)

dat_raw <- janitor::clean_names(dat_raw)

dat_raw <- dat_raw[county=="Guilford"]

dat_clean <- copy(dat_raw)[,max_cases := max(confirm),
													 by = c("county", "reference_date")]

one_day <- dat_clean[county=="Guilford"][
	order(reference_date, report_date)][
	confirm<=max_cases ][
		,perc_returned:=confirm/max_cases
	][
		,report_lag := report_date -reference_date
	][
		,first(.SD), by = c("reference_date", "perc_returned")
	]

one_day[,which.max(perc_returned),by = .(date,reported_date)]

one_day[,report_date_n := report_date-reference_date]

dcast(data = one_day, reference_date~report_date_n , value.var = "confirm")->wide

one_day[,perc_max:=confirm/max_cases]
library(tidyverse)
one_day %>% 
	ggplot(aes(report_date_n,perc_max, group = reference_date))+
	geom_line(alpha = .2)

library(epinowcast)

full_grid <- expand.grid(county = unique(one_day$county),
												 reference_date = as.IDate(seq.Date(min(one_day$reference_date),
												 													max(one_day$reference_date), "day")),
												 report_date = as.IDate(seq.Date(min(one_day$reference_date),
												 													max(one_day$reference_date), "day")))

one_day <- one_day %>% 
	group_by(reference_date) %>% 
	mutate(report_date = as.Date(report_date)) %>% 
	padr::pad(by = "report_date") %>% 
	fill(c(confirm,county,confirm_death),.direction = "down") %>% 
	ungroup() %>% 
	as.data.table()


full_grid_list <- list()
date_range <- unique(one_day$reference_date)
for(i in seq_along(date_range)){
	print(i)
	full_grid_list[[i]] <- expand.grid(reference_date = date_range[i],
																		 report_date = seq.Date(from = date_range[i], to = date_range[i]+40, by = "day"))
}

full_grid <- do.call(rbind, full_grid_list) %>% 
	filter(report_date < Sys.Date())

use_data <- full_grid %>% 
	left_join(one_day) %>% 
	mutate_if(is.numeric, function(x) ifelse(is.na(x), 0 , x)) %>% 
	as.data.table()

nat_germany_hosp <- use_data
nat_germany_hosp[, holiday := FALSE]
nat_germany_hosp[report_date == (as.Date("2022-01-08") - 30), holiday := TRUE]
nat_germany_hosp[,report_date:=as.IDate(report_date)]
nat_germany_hosp[,reference_date:=as.IDate(reference_date)]
nat_germany_hosp <- nat_germany_hosp[report_date <= "2022-01-08"]
retro_nat_germany <- enw_retrospective_data(
	nat_germany_hosp,
	rep_days = 30, ref_days = 30
)

latest_germany_hosp <- enw_latest_data(nat_germany_hosp, ref_window = c(80, 30))

pobs <- enw_preprocess_data(retro_nat_germany, max_delay = 30)

reference_effects <- enw_formula(pobs$metareference[[1]])

report_effects <- enw_formula(pobs$metareport[[1]], random = "day_of_week")

model <- enw_model(threads = TRUE)

options(mc.cores = 2)
nowcast <- epinowcast(pobs,
											model = model,
											report_effects = report_effects,
											reference_effects = reference_effects,
											save_warmup = FALSE, pp = TRUE,
											chains = 2, threads_per_chain = 2,
											show_messages = FALSE, refresh = 0
)

head(summary(nowcast, probs = c(0.05, 0.95)), n = 10)

plot(nowcast, latest_obs = latest_germany_hosp)

# extract samples
samples <- summary(nowcast, type = "nowcast_samples")

# Take a 7 day rolling sum of both samples and observations
cols <- c("confirm", "sample")
samples[, (cols) := lapply(.SD, frollsum, n =  7),
				.SDcols = cols, by = ".draw"][!is.na(sample)]


latest_germany_hosp_7day <- copy(latest_germany_hosp)[,
																											confirm := frollsum(confirm, n =  7)
][!is.na(confirm)]

# Summarise samples
sum_across_last_7_days <- enw_summarise_samples(samples)

# Plot samples
enw_plot_nowcast_quantiles(sum_across_last_7_days, latest_germany_hosp_7day)
