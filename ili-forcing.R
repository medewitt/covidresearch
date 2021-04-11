#devtools::install_github("hrbrmstr/cdcfluview")
#install.packages("cdcfluview")
library(cdcfluview)
dat <- ilinet(region = c( "state"), 
							years = c(2016,2017,2018,2019))
library(data.table)

dat_dt <- as.data.table(dat)

dat_nc <- dat_dt[region=="North Carolina"]

dat_nc[, scaled_ili := scale(unweighted_ili), by = "year"]

plot(unweighted_ili~week_start, dat_nc, type = "l")
plot(scaled_ili~week_start, dat_nc, type = "l")
plot(fft(z = dat_nc$unweighted_ili))

library(ggplot2)
library(dplyr)
dat_nc %>% 
	ggplot(aes(week, unweighted_ili, colour = as.factor(year)))+
	geom_line()
library(cmdstanr)

mod <- cmdstan_model("seasonal-forcing.stan")

stan_dat <- list(
	y = dat_nc$unweighted_ili,
	N = nrow(dat_nc),
	week_no = dat_nc$week,
	period = 52,
	weeks = 52
)
fit <- mod$sample(stan_dat)
fit$summary()
set1 <- RColorBrewer::brewer.pal(7, "Set1")
posterior::as_draws_df(fit$draws("y_hat")) %>% 
	dplyr::sample_frac(.10) %>% 
	tidyr::pivot_longer(!starts_with("."),
							 names_to="ind",
							 names_transform = list(ind = readr::parse_number),
							 values_to="mu") %>%
	mutate(week=dat_nc$week[ind]) %>% 
	ggplot(aes(week, mu, group = .draw)) +
	geom_line(color=set1[2], alpha = 0.1) +
	geom_point(data=dat_nc, mapping=aes(x=week,y=unweighted_ili,
																			colour = year), inherit.aes=FALSE)+
	labs(x="Time (ms)", y="Acceleration (g)")

mod2 <- cmdstan_model("seasonal-forcing-2.stan")
modalt <- cmdstan_model("seasonal-forcing-alt.stan")

stan_dat <- list(
	y = dat_nc$unweighted_ili,
	N = nrow(dat_nc),
	week_no = dat_nc$week,
	period = 52,
	weeks = 52,
	N_years = length(unique(dat_nc$year)),
	years = as.numeric(as.factor(dat_nc$year))
)
fit2 <- mod2$sample(stan_dat)
fitalt <- modalt$sample(stan_dat)
fit2$summary()
fitalt$summary()
fitalt$loo()
posterior::as_draws_df(fit2$draws("y_hat")) %>% 
	dplyr::sample_frac(.10) %>% 
	tidyr::pivot_longer(!starts_with("."),
											names_to="ind",
											names_transform = list(ind = readr::parse_number),
											values_to="mu") %>%
	mutate(week=dat_nc$week[ind]) %>% 
	ggplot(aes(week, mu, group = .draw)) +
	geom_line(color=set1[2], alpha = 0.1) +
	geom_point(data=dat_nc, mapping=aes(x=week,y=unweighted_ili,
																			colour = factor(year)), inherit.aes=FALSE)+
	labs(x="Week", y="ILI")

fit2$summary()->year_effect
plot(dat_nc$week_start,fit2$summary(variables = "y_hat")$mean, ylim = c(0,max(dat_nc$unweighted_ili)))
lines(dat_nc$week_start,dat_nc$unweighted_ili, col = "red")

geo <- geographic_spread(years = NULL)
geo_nc <- geo[geo$statename=="North Carolina",]

geo_nc %>% 
	as.data.table() %>% 
	.[,c("weekend", "activity_estimate")] %>% 
	.[,id:=1] %>% 
	dcast(weekend~activity_estimate,value.var = "id", fill = 0) %>% 
	melt(id.vars = "weekend") %>% 
	ggplot(aes(weekend, value))+
	geom_line()+
	facet_grid(variable~.)

b0 <- 1.9
b1 <- 1.9
forcing_test <- function(b0= 1.9,b1=1.9, week_no, period = 52){
	b0 + b1 * sin(2 * pi * week_no/period)
}
plot(forcing_test(week_no = 1:52))
plot(forcing_test(week_no = 1:52)/b0)

fit2$summary(c("b0","b1"))
fitalt$summary(c("b0","b1"))
2.12/2.03
year_offset

fit2$summary(c("year_offset"))
fitalt$summary(c("year_offset"))
13/52