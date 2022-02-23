library(tidyverse)
library(data.table)
library(mgcv)

dat <- nccovid::get_covid_state(select_county = "Alamance", reporting_adj = TRUE)

setDT(dat)

dat_pop <- merge(dat, nccovid::nc_population[,1:2], by = "county", all.x = TRUE)

dat_pop[,rolling_7day_pop:=cases_daily_roll_sum/(july_2020/100000)]

dat_pop[,date_n:=as.numeric(date)]

dat_pop <- tail(dat_pop,90)

plot(rolling_7day_pop~date, data = dat_pop, 
		 ylab = "7 Day Cases per 100k", xlab = "", type = "b", pch = 19,
		 ylim = c(0,3000))
abline(h = 100, col = "red")

fit <- gam(rolling_7day_pop ~ s(date_n), data =dat_pop, 
					 family = quasipoisson())

plot(fit)
date_seq <- seq.Date(Sys.Date()-45, length.out = 90, by = "day")
out <- predict(fit, 
							 newdata = data.frame(date_n = as.numeric(date_seq)), 
							 se.fit = TRUE)

out

out <- data.frame(out = exp(out$fit), 
									out_lo = exp(out$fit + 2* out$se.fit),
									out_hi = exp(out$fit - 2* out$se.fit),
									date = date_seq)
out %>% 
	ggplot(aes(date, out))+
	geom_line()+
	geom_ribbon(aes(ymin = out_lo, ymax = out_hi), alpha = .2)+
	labs(
		y = "7 Day Cases per 100k",
		x = NULL,
		title = "Estimated 7 Day Cases per 100k",
		subtitle = "For Alamance County"
	)+
	theme_classic()+
	geom_hline(yintercept = 100, col = "red", lty = "dashed")+
	geom_hline(yintercept = 50, col = "orange", lty = "dashed")+
	geom_point(data = tail(dat_pop,45), aes( date, rolling_7day_pop))

