# Purpose: Estimate NC Fatality Lag for SARS-CoV-2

# libraries ---------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)

dat <-nccovid::pull_covid_summary()

dat <- dat[!is.na(daily_cases)]

dat_red <- dat[,.(date,daily_cases,daily_deaths)] %>% 
	.[ ,daily_deaths:=fifelse(is.na(daily_deaths),0,daily_deaths)] %>% 
	.[date>as.Date("2020-03-20")]

corz <- vector(mode = "numeric")
for(i in 1:30){
	lagged <- dat_red[,shift(daily_cases, i)]
	lagged <- lagged[!is.na(lagged)]
	corz[i] <- cor(lagged, tail(dat_red$daily_deaths,-i))
}
plot(corz, type = "b", pch = 19, 
		 xlab = "Lag", ylab = "Correlation",
		 main = "Lags Between Reported Cases and Deaths in NC", adj = 0)
abline(v = which.max(corz), lty = 2)
abline(v = which.max(corz), lty = 2)

plot(dat_red$daily_cases, type = "b", pch = 19, 
		 xlab = "Lag", ylab = "Correlation",
		 main = "Lags Between Reported Cases and Deaths in NC", adj = 0)