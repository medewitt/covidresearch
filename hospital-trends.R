nccovid::get_hospitalizations()->hosp

setDT(hosp)

overall_hos <- hosp[,list(covid_pts = sum(number_of_covid_19_positive_patients_in_hospital),
													pts = sum(inpatient_full_all_bed_types),
													total_beds = sum(licensed_inpatient_beds),
													total_mp = sum(inpatient_empty_beds_all_types),
					 beds = sum(total_staffed_inpatient_capacity_all_bed_types)), by = c("date", "coalition")]

overall_hos<-overall_hos[order(date)][,utilization:=pts/beds]


plot(beds ~ date, data = overall_hos, type = "l")


for(i in unique(overall_hos$coalition)){
	plot(utilization ~ date, data = overall_hos[coalition==i], type = "l",
			 ylim = c(.5,1))
	title(i)
	abline(h = .9, col = "red")
	abline(h = .8, col = "blue")
}

check <- overall_hos[parent_name==28]
