######################################################################
## R code generating graphs and numbers for the Analysis of 
## Outbreak Data section of the book chapter "Iterated filtering 
## methods for Markov process epidemic models"  by T. Stocks in the
## Handbook of Infectious Disease Data Analysis.
##
## Author: Theresa Stocks <http://www.su.se/english/profiles/tstoc-1.219526>
## Affiliation: Department of Mathematics, Stockholm University, Sweden
##
## Date: 7/12/2017
######################################################################

###Install packages
library(pomp)
library(dplyr)
library(readr)
library(dplyr)
library(magrittr)
library(plyr)
library(reshape2)
library(ggplot2)


dat <- read.table("sim_data.txt")
dat <- nccovid::get_covid_state()
library(data.table)
dat <- dat[, .(cases=sum(cases_daily)), .(state, date)]
dat$times <- 1:nrow(dat)

dat_selected <- dat %>% 
	mutate(B = 1000000- cases) %>% 
	select(cases,times)

read.table("http://kingaa.github.io/short-course/stochsim/bsflu_data.txt") -> bsflu

sir_step <- Csnippet("
  double dN_SI = rbinom(S,1-exp(-Beta*I/N*dt));
  double dN_IR = rbinom(I,1-exp(-gamma*dt));
  S -= dN_SI;
  I += dN_SI - dN_IR;
  R += dN_IR;
")

sir_init <- Csnippet("
  S = N-1;
  I = 1;
  R = 0;
")

pomp(dat_in,time="times",t0=0,rprocess=euler(sir_step,delta.t=1/6),
		 rinit =sir_init,paramnames=c("N","Beta","gamma"),
		 statenames=c("S","I","R")) -> sir

sir_step <- Csnippet("
  double dN_SI = rbinom(S,1-exp(-Beta*I/N*dt));
  double dN_IR = rbinom(I,1-exp(-gamma*dt));
  S -= dN_SI;
  I += dN_SI - dN_IR;
  R += dN_IR;
  H += dN_IR;
")

sir_init <- Csnippet("
  S = N-1;
  I = 1;
  R = 0;
  H = 0;
")

pomp(sir,rprocess=euler(sir_step,delta.t=1/6),rinit =sir_init,
		 paramnames=c("Beta","gamma","N"),statenames=c("S","I","R","H")) -> sir

sir <- pomp(sir,rmeasure=rmeas,statenames="H",paramnames="rho")

rmeas <- Csnippet("B = rbinom(H,rho);")

sir <- pomp(sir,rmeasure=rmeas,statenames="H",paramnames="rho")

sims <- simulate(sir,params=c(Beta=1.5,gamma=1,rho=0.9,N=2600),
								 nsim=20,include.data=TRUE)

ggplot(sims,mapping=aes(x=time,y=B,group=sim, color=sim=="data"))+
	geom_line()+guides(color=FALSE)

sims[[1]]@states
