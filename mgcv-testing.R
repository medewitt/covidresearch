library(mgcv)
cases_raw <- nccovid::get_covid_state(select_county = "Guilford")

fit <- gam(cases_daily ~ 0 +  s(as.numeric(date), bs = "cc", k = 4), 
					 data = cases_raw, family = nb)


plot(fit)
mm <- model.matrix(fit)
fit$smooth
gam(cases ~ 0 + s(day_of_week, bs = "cc", k = 4)