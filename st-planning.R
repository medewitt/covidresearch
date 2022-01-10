library(osrm)
library(stplanr)

trip2 <- route_osrm(
	from = c(-79.786228,36.0910),
	to = c(-79.77073558152786,36.08704627090165),
)

routes <- route(
	l = desire_lines,
	route_fun = osrmRoute,
	returnclass = "sf"
)

mapview::

rnet <- overline(trip2[,1:2], attrib = "foot")
od2line(flow = trip2, zones = trip2)

library(INLA) 

n= 100; a = 1; b = 1; tau = 100
z = rnorm(n)
eta = a + b*z

scale = exp(rnorm(n))
prec = scale*tau
y = rnorm(n, mean = eta, sd = 1/sqrt(prec))


data = list(y=y, z=z)
formula = y ~ 1+z
result = inla(formula, family = "gaussian", data = data)
