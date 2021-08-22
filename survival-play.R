data(Melanoma, package = "MASS")

library(survival)
library(rms)
library(cmprsk)

cuminc(Melanoma$time, Melanoma$status, cencode = 2)

lung

ci_fit <- 
	cuminc(
		ftime = Melanoma$time, 
		fstatus = Melanoma$status, 
		cencode = 2
	)
plot(ci_fit)

ci_fit <- 
	cuminc(
		ftime = lung$time, 
		fstatus = lung$status
	)
plot(ci_fit)

chr_fit <- 
	cph(
		Surv(time, ifelse(status == 1, 1, 0)) ~ sex + age, 
		data = Melanoma
	)
chr_fit

summary(chr_fit)
