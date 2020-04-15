library(deSolve)
library(plotly)
library(dplyr)
library(reshape)
library(RColorBrewer)
library(htmltools)

source(here::here("harvard-epi", "src","functions.R"))
# Set parameters
# (in Shiny app these are input by sliders)

IncubPeriod= 5 #Duration of incubation period
DurMildInf= 6 #Duration of mild infections
FracSevere= 15 #% of symptomatic infections that are severe
FracCritical= 5 #% of symptomatic infections that are critical
ProbDeath= 40 #Death rate for critical infections
DurHosp= 6 #Duration of severe infection/hospitalization
TimeICUDeath= 8 #Duration critical infection/ICU stay

AllowPresym="No"
AllowAsym="No"
FracAsym=25 #Fraction of all infections that are asymptomatic
PresymPeriod=2 #Length of infectious phase of incubation period
DurAsym=6 #Duration of asympatomatic infection

b1= 0.5 #Transmission rate (mild infections)
b2= 0.01 #Transmission rate (severe infections)
b3= 0.01 #Transmission rate (critical infections)
be = 0.5 #Transmission rate (pre-symptomatic)
b0 = 0.5 #Transmission rate (asymptomatic infections)
N= 1000 #Total population size
Tmax= 300 #Initial # infected
InitInf= 1 #Maximum time
yscale="linear"

AllowSeason="No"
seas.amp=0.0 #relative amplitude of seasonal fluctuations, in [0,1]
seas.phase=-90 #phase of seasonal fluctuations, measuered in days relative to time zero when peak will occur (0=peak occurs at time zero, 30 = peak occurs one month after time zero). Can be negative
#Note that the beta values chosen above will be those for time zero, wherever that occurs in the seasonality of things.

#Capacity parameters (US)
HospBedper=2.8 #Total hospital beds per 1000 ppl 
HospBedOcc=66 # average % occupancy of hospital beds
ICUBedper=0.26 # Total ICU beds per 1000 ppl
ICUBedOcc=68 # average % occupancy of hospital beds
IncFluOcc=10 # increase in occupancy during flu season
ConvVentCap=0.062 # conventional capacity for mechanical ventilation, per 1000 ppl
ContVentCap=0.155 # contingency capacity for mechanical ventilation, per 1000 ppl
CrisisVentCap=0.418 # crisis capacity for mechanical ventilation, per 1000 ppl

#Overflow death/progression rates
FracProgressNoCare = 50 # % of symptomatic infections that would progress to critical without any hospital care
FracDieNoCare=99 # % of critical infections that would die without ICU care (can't be exactly 100%)

# Choose overflow option to impose
CapHosp="HospBedsOcc" # This is the type of cap to be used for individuals requiring regular hospitalization. Options include "None" (no cap on hospital beds), "HospBeds" (cap based on total hospital beds), "HospBedsOcc" (cap based on average annual unoccupied hospital beds), or "HospBedsOccFlu" (cap based on unoccupied hospital beds during flu season)
CapICU="ICUBedsOcc" # This is the type of cap to be used for individuals requiring critical care (ICU beds + mechanical ventilators). Options include "None" (no cap on ICU beds/ventilators), "ICUBeds" (cap based on total ICU beds), "ICUBedsOcc" (cap basd on average annual unocuppied hospital beds), "ICUBedsOccFlu" (cap based on unoccupied hospital beds during flu season), "ConvVentCap" (cap based on conventional capacity for mechanical ventilation), "ContVentCap" (cap based on contingency capacity for mechanical ventilation), "CrisisVentCap" (cap based on crisis capacity for mechanical ventilation)

#Intervention parameters
Tint = 30 #Intervention start time (days)
Tend = Tmax #Intervention end time (days)
s0=0 #Reduction in transmission from asymptomatic/presymptomatic infections
s1=90 #Reduction in transmission from mild infections
s2=0 #Reduction in transmission from severe infections
s3 =0 #Reduction in transmission rate from critical infections
RoundOne = FALSE #Round values to nearest integar post-intervention?"

#Put these into an input structure
input=list("IncubPeriod"=IncubPeriod,"DurMildInf"=DurMildInf,"FracSevere"=FracSevere,"FracCritical"=FracCritical,"ProbDeath"=ProbDeath,"DurHosp"=DurHosp,"TimeICUDeath"=TimeICUDeath,"FracAsym"=FracAsym, "PresymPeriod"=PresymPeriod, "DurAsym"=DurAsym, "be"=be,"b0"=b0, "b1"=b1,"b2"=b2,"b3"=b3,"seas.amp"=seas.amp, "seas.phase"=seas.phase,"N"=N,"Tmax"=Tmax,"InitInf"=InitInf,"yscale"=yscale,"AllowPresym"=AllowPresym,"AllowAsym"=AllowAsym,"AllowSeason"=AllowSeason,"HospBedper"=HospBedper,"HospBedOcc"=HospBedOcc,"ICUBedper"=ICUBedper,"ICUBedOcc"=ICUBedOcc,"IncFluOcc"=IncFluOcc,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap,"CapHosp"=CapHosp,"CapICU"=CapICU,"FracProgressNoCare"=FracProgressNoCare,"FracDieNoCare"=FracDieNoCare,"Tint"=Tint,"Tend"=Tend,"s0"=s0,"s1"=s1,"s2"=s2,"s3"=s3,"RoundOne"=RoundOne)

CFR=(input$ProbDeath/100)*(input$FracCritical) #Case fatality rate

# get capacity levels
capParams=SetHospCapacityOverflow(input)
icap=unname(capParams["icap"])
hcap=unname(capParams["hcap"])

# ------- Run simulations (no intervention) ------------

sim=SimSEIR(input)

out.df=sim$out.df
N=sim$N
Ro=sim$Ro
r=sim$r
DoublingTime=sim$DoublingTime

#combine exposed classes for plotting
out.df$E0=out.df$E0+out.df$E1
out.df$E1=NULL
out.df=rename(out.df, c(E0="E"))

#reformat data for plotting
out.df2=rename(out.df, c(S="Susceptible", E="Exposed", I0="Infected.Asymptomatic", I1="Infected.Mild", I2="Infected.Severe", I3="Infected.Critical", R="Recovered", D="Dead"))
out=melt(out.df,id="time")
out2=melt(out.df2,id="time")
out$variableName=out2$variable
out$variableLegend = paste0(out$variableName,' (',out$variable,')')
out$variableLegend = factor(out$variableLegend, levels = unique(out[["variableLegend"]]))

#plot
if(input$PlotCombine=="Yes"){
	
	#create data for plotting total infected
	Comb.df=out.df
	Comb.df$I0=rowSums(out.df[,c("I0","I1","I2","I3")]) # create total infected
	Comb.df=rename(Comb.df, c(I0="I"))
	Comb.df$I1=NULL
	Comb.df$I2=NULL
	Comb.df$I3=NULL
	
	Comb.df2=rename(Comb.df, c(S="Susceptible", E="Exposed", I="Infected", R="Recovered", D="Dead"))
	Comb=melt(Comb.df,id="time")
	Comb2=melt(Comb.df2,id="time")
	Comb$variableName=Comb2$variable
	Comb$variableLegend = paste0(Comb$variableName,' (',Comb$variable,')')
	Comb$variableLegend = factor(Comb$variableLegend, levels = unique(Comb[["variableLegend"]]))
	
	
	
	p=plot_ly(data = Comb, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')
	
}else{
	if(input$AllowAsym=="Yes"){
		p=plot_ly(data = out, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')
	}else{
		#don't want to show the I0 class in the plot
		outSym=out[out$variable!="I0",]
		p=plot_ly(data = outSym, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')
	}
}

p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per",formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscale),
				 annotations=list(text=HTML(paste("R", tags$sub(0),'=',formatC(Ro,digits=2)," <br>r =", formatC(r,digits=2)," per day <br>T",tags$sub(2)," = ",formatC(DoublingTime,digits=2)," days")),showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0.35, align="left"))

if(seas.amp>0){
	
	Ro.Season=sim$Ro.Season
	tpeak=365+input$seas.phase
	tmin=180+input$seas.phase
	
	p=layout(p,annotations=list(text=HTML(paste("Seasonal R", tags$sub(0), ": <br>Initial R", tags$sub(0),'=',formatC(Ro.Season$Ro.now,digits=2),"<br>Peak R", tags$sub(0),'=',formatC(Ro.Season$Ro.max,digits=2),"@day",formatC(tpeak,format = "f",digits=0),"<br>Min R", tags$sub(0),'=',formatC(Ro.Season$Ro.min,digits=2),"@day",formatC(tmin,format = "f",digits=0))),
															showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0.05, align="left"))
	
}

p
