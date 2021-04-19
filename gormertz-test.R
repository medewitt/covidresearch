library(cmdstanr)
library(data.table)
library(dplyr)
library(ggplot2)

mod <- cmdstan_model("gompertz.stan")

# Pull data -----------------------------------------

vax_raw <- nccovid::get_vaccinations(county_list = nccovid::cone_region)

vax_daily <- vax_raw[,.(first_dose = sum(daily_partial_vax)), by = c("date","county")][,c("county","date","first_dose")]

vax_daily =vax_daily %>%
  group_by(county) %>%
  tidyr::nest() %>%
  mutate(thing=purrr::map(data, padr::pad)) %>%
  select(county, thing) %>%
  tidyr::unnest(cols= c(thing)) %>%
  as.data.table() %>%
  .[,first_dose := fifelse(is.na(first_dose),0,first_dose)] %>%
  .[,first_dose_cum := cumsum(first_dose), by = "county"]

vax_daily[order(date), t := seq_len(.N), by = c("county")]

vax_daily[, group_id := as.numeric(factor(county))]

#----------------prepare stan data

n_out <- 200

pred_matrix <- data.frame(
    county = rep(c("Alamance", "Guilford", "Randolph", "Rockingham"), times = 200),
    t = rep(1:n_out,each = 4),
    groups_pred = rep(1:4,times = 100)
)

popz <- nccovid::nc_population[ county %in% nccovid::cone_region]$july_2020

stan_dat <- list(
  N = nrow(vax_daily),
  Y = vax_daily$first_dose_cum,
  X = vax_daily$t,
  N_group = 4L,
  groups = vax_daily$group_id,
  N_pred = nrow(pred_matrix),
  X_pred = pred_matrix$t,
  groups_pred = pred_matrix$groups_pred,
  population_prior = popz * 3/4
)

fit <- mod$sample(stan_dat, parallel_chains = 4, adapt_delta = .98, max_treedepth =15)

## Looking at Results

fit$summary("A_group")
fit$summary(c("k", "delay", "sigma"))

fit$summary("y_pred")->pred_out

results <- cbind(pred_matrix, 
    mu = pred_out$mean, 
    q05 = pred_out$q5, 
    q95 = pred_out$q95)

results %>%
as.data.table() %>%
.[,rates := mu, by ="county"] %>%
  ggplot(aes(t, rates, colour = county)) +
  geom_line()+
  geom_point(data = vax_daily,
  aes(x = t, y = first_dose_cum, colour = county), inherit.aes = FALSE)

results %>%
as.data.table() %>%
.[,rates := mu, by ="county"] %>%
.[county=="Guilford"] %>%
.[,daily_vax:= mu - shift(mu, type='lag')]-> daily_vax_estimated