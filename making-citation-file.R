library(R6)

author <- eval(parse(text = getOption("usethis.description")$`Authors@R`))

Citation <- R6Class("Citation", list(
	name = NULL,
	orcid = NULL,
	initialize = function(name, orcid = NA) {
		stopifnot(is.character(name), length(name) == 1)
		stopifnot(is.character(orcid), length(orcid) == 1)
		
		self$name <- name
		self$orcid <- orcid
	},
	
	print = function(...){
		cat("Person: \n")
		cat("  Name: ", self$name, "\n", sep = "")
		cat("  ORCID:  ", self$orcid, "\n", sep = "")
		invisible(self)
	}
))
Citation$new("Michael", "Test")

# Scema
#https://github.com/citation-file-format/citation-file-format/blob/main/schema-guide.md

