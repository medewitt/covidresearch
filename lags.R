url <- "https://raw.githubusercontent.com/conedatascience/covid-data/master/data/timeseries/cases-delay.csv"

library(data.table)

dat_raw <- fread(url)

dat_raw <- janitor::clean_names(dat_raw)

dat_clean <- copy(dat_raw)[,max_cases := max(total_cases_by_date_of_specimen_collection),
													 by = c("county", "date")]

one_day <- dat_clean[county=="Guilford"& date >= "2021-11-01"][
	order(date)][
	total_cases_by_date_of_specimen_collection<=max_cases ][
		,perc_returned:=total_cases_by_date_of_specimen_collection/max_cases
	][
		,report_lag := reported_date -date
	][
		,first(.SD), by = c("date", "perc_returned")
	]

one_day[,which.max(perc_returned),by = .(date,reported_date)]
