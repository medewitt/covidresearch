require("deSolve")
# from http://www.sherrytowers.com/sir_age.R
##################################################################################
##################################################################################
# this is an age structured SIR model
# the parameters in the vparameters list are:
#    the recovery period, gamma
#    the probability of transmission on contact, beta
#    the contact matrix, C, that is the # contacts per day among age groups
#
# Note that x is a vector of length (#model compartment types)*(#age classes)
# For the SIR model, there are 3 model compartment types (S, I, and R)
# The code at the beginning of the function fills the age classes for each
# model compartment type in turn.
# Thus, S, I and R are vectors, all of length nage
##################################################################################
calculate_derivatives=function(t, x, vparameters){
	ncompartment = 3
	nage = length(x)/ncompartment
	S    = as.matrix(x[1:nage])
	I    = as.matrix(x[(nage+1):(2*nage)])
	R    = as.matrix(x[(2*nage+1):(3*nage)])
	
	I[I<0] = 0
	with(as.list(vparameters),{
		# note that because S, I and R are all vectors of length nage, so will N,
		# and dS, dI, and dR
		N = S+I+R
		dS = -as.matrix(S*beta)*(as.matrix(C)%*%as.matrix(I/N))
		dI = -dS - gamma*as.matrix(I)
		dR = +gamma*as.matrix(I)
		# remember that you have to have the output in the same order as the model
		# compartments are at the beginning of the function
		out=c(dS,dI,dR)
		list(out)
	})
}

##################################################################################
##################################################################################
# Let's set up some initial conditions at time t=0
# assume 25% of the population is kids
##################################################################################
lcalculate_transmission_probability = 1 # if this is 1, then calculate the transmission probability from R0
# otherwise, assume it is beta=0.05
npop = 10000000
f = c(0.25,0.75) # two age classes, with 25% kids, and 75% adults
N =  npop*f      # number in each age class

nage = length(f)
I_0    = rep(1,nage) # put one infected person each in the kid and adult classes
S_0    = N-I_0
R_0    = rep(0,nage)

gamma = 1/3        # recovery period of influenza in days^{-1}
R0    = 1.5        # R0 of a hypothetical pandemic strain of influenza

C = matrix(0,nrow=nage,ncol=nage)
C[1,1] = 18   # number contacts per day kids make with kids
C[1,2] = 6    # number contacts per day kids make with adults (all kids have an adult in the home)
C[2,1] = 3    # number contacts per day adults make with kids (not all adults have kids)
C[2,2] = 12   # number contacts per day adults make with adults
if (lcalculate_transmission_probability==1){
	M = C
	M[1,1] = C[1,1]*f[1]/f[1]
	M[1,2] = C[1,2]*f[1]/f[2]
	M[2,1] = C[2,1]*f[2]/f[1]
	M[2,2] = C[2,2]*f[2]/f[2]
	eig = eigen(M)
	# reverse engineer beta from the R0 and gamma 
	beta = R0*gamma/max(Re(eig$values))  
	beta = beta
}else{
	beta = 0.05
}

##################################################################################
# numerically solve the model
##################################################################################
vparameters = c(gamma=gamma,beta=beta,C=C)
inits = c(S=S_0,I=I_0,R=R_0)

##################################################################################
# let's determine the values of S,I and R at times in vt
##################################################################################
vt = seq(0,150,1)  
mymodel_results = as.data.frame(lsoda(inits, 
																			vt, 
																			calculate_derivatives, 
																			vparameters))

##################################################################################
# output some results
##################################################################################
cat("The fraction of kids   that were infected is ",max(mymodel_results$R1)/N[1],"\n")
cat("The fraction of adults that were infected is ",max(mymodel_results$R2)/N[2],"\n")
cat("The total final size is ",max(mymodel_results$R1+mymodel_results$R2)/npop,"\n")

##################################################################################
# plot some results
##################################################################################
par(mfrow=c(1,1))

##################################################################################
# We are going to be plotting the prevalence in the first and second age classes,
# overlaid on the same plot
# This is a little coding trick to ensure that the yaxis limits are set large 
# enough that both curves will show on the plot
##################################################################################
ymax = max(c(mymodel_results$I1/N[1],mymodel_results$I2/N[2]))

plot(mymodel_results$time
		 ,mymodel_results$I1/N[1]
		 ,type="l"
		 ,xlab="Time, in days"
		 ,ylab="Fraction of each sub-population infected (prevalence)"
		 ,ylim=c(0,ymax)
		 ,lwd=5
		 ,col=2
		 ,main="Pandemic influenza simulation with age-structured SIR model")
lines(mymodel_results$time
			,mymodel_results$I2/N[2]
			,col=4
			,lwd=5)
legend("topleft"
			 ,legend=c("kids","adults")
			 ,col=c(2,4)
			 ,bty="n"
			 ,lwd=5
			 ,cex=2)
