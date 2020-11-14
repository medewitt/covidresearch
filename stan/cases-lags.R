# Purpose: Estimate NC Fatality Lag for SARS-CoV-2

# libraries ---------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)

dat <-nccovid::pull_covid_summary()

dat <- dat[!is.na(daily_cases)]

dat_red <- dat[,.(date,daily_cases,daily_deaths)] %>% 
	.[ ,daily_deaths:=fifelse(is.na(daily_deaths),0,daily_deaths)]

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

plot(shift(dat_red$daily_cases,6, fill = 0)*.01, type = "b", pch = 19, ylim = c(0,70),
		 xlab = "Lag", ylab = "Correlation",
		 main = "Lags Between Reported Cases and Deaths in NC", adj = 0)
lines(dat_red$daily_deaths, col = "red")

under <- dat_red %>% 
	.[daily_cases > 0] %>% 
	.[,daily_cfr := daily_deaths/daily_cases] %>% 
	.[,undercount := daily_cfr/.008] %>% 
	.[,date_n := as.numeric(date)]
fit <- mgcv::gam(undercount ~ s(date_n,bs = "gp"), 
								 data = under,
								 weights = daily_cases)
plot(fitted(fit), type = "b", pch = 19)
abline(h = 1,  col = "firebrick", lty = 2)
abline(h = 2,  col = "firebrick", lty = 3)

