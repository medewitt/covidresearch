
# D3 map ------------------------------------------------------------------


# Packages ----
library(r2d3maps)
library(rnaturalearth)
library(sf)

# Data ----
Nigeria <- ne_states(country = "Nigeria", returnclass = "sf")

covid_cases <-nccovid::get_covid_state()

nc_counties <- tigris::counties(state = "NC")
nc_counties <- st_as_sf(nc_counties)
library(tidyverse)
combined <- covid_cases[date==Sys.Date()] %>%
	left_join(nc_counties, by = c("county" = "NAME")) %>% 
	st_as_sf() %>% 
	select(county, cases_daily)
# Map ----
r2d3map(
  data = combined,
  script = "my_map.js"
)

out <- d3_map(shape = combined) %>% 
	add_continuous_gradient(
		var = "cases_daily"
	) %>%
	add_tooltip(value = "<b>{county}</b>: {cases_daily}", .na = NULL) %>%
	add_legend(title = "Prénoms") %>%
	add_labs(
		title = "Prénoms féminins les plus attribués en 1989",
		caption = "Data: Insee"
	)
out
r2d3maps::