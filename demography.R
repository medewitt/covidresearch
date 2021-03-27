library(tidyverse)

dat <- readxl::read_excel("Table04.xlsx", skip = 2)

names(dat)[1] <- "Age"

dat %>% 
	select(Age, lx) %>% 
	mutate(Age = str_extract(Age, "\\d+")) %>% 
	ggplot(aes(as.numeric(Age), lx/100000)) + geom_line(lwd = 1.1) + 
	ylab("lx") + xlab("age")

nccovid::get_vaccinations_demo(demographic = "race",county_list = nccovid::triad_counties)->a

di_nc = a %>% 
	mutate(white = ifelse(demographic_identity=="White", "white", "lrg")) %>% 
	group_by(county,white) %>% 
	summarise(pop = mean(county_demo_pop, na.rm = TRUE),
						vax=sum(vax_n, na.rm = T)) %>% 
	filter(!is.na(pop)) %>% 
	ungroup() %>% 
	mutate(Y = vax/sum(vax),
				 X = pop/sum(pop)) %>% 
	select(county, Y, white) %>% 
	spread(white, Y, fill = 0) %>% 
	mutate(DI = .5 * abs(lrg-white))

di_race= a %>% 
	mutate(white = ifelse(demographic_identity=="White", "white", "lrg")) %>% 
	group_by(county,white) %>% 
	summarise(pop = mean(county_demo_pop, na.rm = TRUE),
						vax=sum(vax_n, na.rm = T)) %>% 
	filter(!is.na(pop)) %>% 
	ungroup() %>% 
	mutate(Y = vax/sum(vax),
				 X = pop/sum(pop)) %>% 
	select(county, X, white) %>% 
	spread(white, X, fill = 0) %>% 
	mutate(DI = .5 * abs(lrg-white))

di_nc %>% 
	ggplot(aes(reorder(county, DI), DI))+
	geom_point()+
	coord_flip()

di_race %>% 
	ggplot(aes(reorder(county, DI), DI))+
	geom_point()+
	coord_flip()
