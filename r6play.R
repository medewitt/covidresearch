library(R6)

Person <- R6Class("Person",
									public = list(
										name = NULL,
										hair = NULL,
										initialize = function(name = NA, hair = NA, greeting = "hello") {
											self$name <- name
											self$hair <- hair
											self$greet()
										},
										set_hair = function(val) {
											self$hair <- val
										},
										greet = function(greeting = "Hello") {
											cat(paste0(greeting,", my name is ", self$name, ".\n"))
										}
									)
)
ann<-Person$new("ann")
ann$greet(greeting = "You there")

Covidata <- R6Class("Covidata",
										public = list(
											county = NULL,
											data = list(),
											initialize = function(county = "Guilford"){
												self$county <- county
												self$greet()
											},
											greet = function(){
												cat(paste0("Pulling data for", self$county))
											},
											pull_data = function(){
												d <- nccovid::get_covid_state(select_county = self$county)
												self$data <- d
											},
											print_data = function(){
												self$data
											}
										))
dat <- Covidata$new(county = "Forysth")

dat$pull_data()
dat$print_data()
