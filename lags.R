url <- "https://raw.githubusercontent.com/conedatascience/covid-data/master/data/timeseries/cases-delay.csv"

library(data.table)

dat_raw <- fread(url)

dat_raw <- janitor::clean_names(dat_raw)

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
