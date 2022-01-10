library(data.table)
library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)

url <- "https://raw.githubusercontent.com/alex1770/Covid-19/master/VOCgrowth/EarlyOmicronEstimate/extracthospdata/SouthAfricaHospData.json"

dat_raw <- jsonlite::fromJSON(url, simplifyVector = FALSE)



out <- map(dat_raw, as.data.frame)

rbindlist(out, fill = TRUE, idcol = "report_date") %>% 
	select(report_date, contains("South.Africa")) %>% 
	mutate(report_date = as.Date(report_date)) %>% 
	ggplot(aes(report_date, South.Africa.Currently.Admitted))+
	geom_line()
